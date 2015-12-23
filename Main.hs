{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import GHC.Generics
import Control.Concurrent
import Control.Error
import Control.Exception
import Control.Lens hiding ((<.>))
import Control.Logging
import Control.Monad
import Control.Monad.IO.Class
import Data.IntMap.Strict (IntMap)
import Data.IORef
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Text.Encoding
import Data.Trie (Trie)
import Data.Aeson as Aeson
import Data.Aeson.Encode as Aeson
import Network.HTTP.Client (HttpException(..))
import Network.HTTP.Types.Status hiding (statusCode)
import Network.Wai hiding (responseStatus)
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Network.Wai.Middleware.Gzip
import Network.Wreq hiding (auth)
import Prelude hiding (id, log)
import System.Directory
import System.Environment
import System.FilePath
import Text.XML.Light
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy.Builder as LazyText
import qualified Data.Trie as Trie
import qualified Web.Scotty as Scotty
import qualified Data.IntMap.Strict as IntMap

data Thing = Thing
  { id :: Int
  , typ :: Text
  , thumbnail :: Text
  } deriving (Show, Generic, ToJSON)

type DB = IORef (Trie [(Text, Thing)], IntMap Thing)

status :: Text -> IO ()
status msg = do log msg; putStrLn (Text.unpack msg)

warning :: Text -> IO ()
warning msg = do warn msg; putStrLn (Text.unpack msg)

u :: String -> QName
u = unqual

munchName :: Element -> Either Text Text
munchName name =
  case findAttr (u "value") name of
    Nothing -> Left "No value attribute in name element"
    Just s -> Right $! Text.pack s

munchItem :: Element -> Either Text [(ByteString, (Text, Thing))]
munchItem item = do
  typ0 <- note "No type attribute in item element" $ findAttr (u "type") item
  str <- note "No id attribute in item element" $ findAttr (u "id") item
  id <- note "Non-integer ID" $ readMay str
  let typ = Text.pack typ0
  let thumbnail = Text.pack (maybe "" strContent (findChild (u "thumbnail") item))
  case findChildren (u "name") item of
    [] -> Left "No names"
    names -> do
      titles <- mapM munchName names
      let things = repeat Thing {id, typ, thumbnail}
      let toKey = encodeUtf8 . Text.toCaseFold
      let assoc = [(toKey title, (title, t)) | (title, t) <- zip titles things]
      thumbnail `seq` typ `seq` id `seq` return assoc

munchItems :: Element -> Either Text [(ByteString, (Text, Thing))]
munchItems xml =
  case findChildren (u "item") xml of
    [] -> Left "No items"
    items -> concat <$> mapM munchItem items

boardGame :: Thing -> Bool
boardGame t =
  case typ t of
    "boardgame" -> True
    "boardgameexpansion" -> True
    _ -> False

loadThing :: Int -> FilePath -> Text -> Bool -> DB -> IO Bool
loadThing id fp txt writeToDisk db = do
  status ("Loading " <> Text.pack (show id))
  case parseXMLDoc txt of
    Nothing -> do warning "Failed to parse XML"; return False
    Just xml ->
      case munchItems xml of
        Left msg -> do
          warning (msg <> " in BGG XML: " <> txt)
          return False
        Right [] -> error "impossible"
        Right l@((_, (_, thang)) : _) -> do
          let trie = Trie.fromList [(name, [x]) | (name, x@(_, t)) <- l, boardGame t]
          modifyIORef' db (\(t, m) -> (Trie.mergeBy (\a b -> Just (a ++ b)) t trie, IntMap.insert id thang m))
          when writeToDisk (Text.IO.writeFile fp txt)
          return True

fetchThing :: Int -> DB -> IO Bool
fetchThing id db = do
  status $ "Sending thing query to BGG: " <> Text.pack (show id)
  let url = "http://www.boardgamegeek.com/xmlapi2/thing?id=" ++ show id
  r <- Network.Wreq.get url
  case r ^. responseStatus . statusCode of
    200 -> do
      let fp = "things" </> show id <.> "xml"
      loadThing id fp (decodeUtf8 $ BL.toStrict $ r ^. responseBody) True db
    _ -> do
      let msg = Text.pack $ show (r ^. responseStatus)
      warning $ "BGG API returned error: " <> msg
      return False

fetchRetryThing :: Int -> DB -> IO Bool
fetchRetryThing id db =
  catch
    (fetchThing id db)
    (\(exn :: HttpException) -> do
      print exn;
      sleep 60;
      fetchRetryThing id db
    )

-- Number of consecutive IDs yielding empty BGG responses such that
-- we consider the DB exhausted
maxSkips :: Int
maxSkips = 100

sleep :: Int -> IO ()
sleep secs = threadDelay (secs * 1000000)

fetchNew :: Int -> Int -> DB -> IO ()
fetchNew id skips db
  | skips == maxSkips = do
      status "#### STOP querying BGG, trying again in 24 hours"
      sleep (24 * 60 * 60)
      fetchNew (id-skips) 0 db
  | otherwise = do
      status "#### START querying BGG for new things..."
      ok <- fetchRetryThing id db
      fetchNew (id + 1) (if ok then 0 else skips+1) db

thingFiles :: FilePath -> IO [(Int, FilePath)]
thingFiles thingdir = do
  filepaths <- getDirectoryContents thingdir
  let files = filter ((==) ".xml" . takeExtension) filepaths
  return $ sort [(read $ takeBaseName fp, "things" </> fp) | fp <- files]

missing :: [Int] -> [Int]
missing ids =
  let f (groups, last_id) id
        | id - last_id > 1 = ([last_id+1..id-1] : groups, id)
        | otherwise = (groups, id)
  in
  let (groups, _) = foldl' f ([], (-1)) ids in
  concat groups

fetchMissing :: FilePath -> DB -> IO ()
fetchMissing thingdir db = do
  status "#### START re-querying BGG for \"missing\" things..."
  things <- thingFiles thingdir
  let ids = map fst things
  mapM_ (\id -> fetchRetryThing id db) (missing ids)
  status "#### STOP querying BGG for \"missing\" things, trying again in 48 hours"
  sleep (48 * 60 * 60)
  fetchMissing thingdir db

loadThings :: FilePath -> DB -> IO ()
loadThings thingdir db = do
  createDirectoryIfMissing False thingdir
  things <- thingFiles thingdir
  status "Loading things from disk..."
  let load (id, fp) = do txt <- Text.IO.readFile fp; loadThing id fp txt False db
  mapM_ load things
  status "Finished loading things from disk"
  let ids = map fst things
  let start = if null ids then 0 else last ids + 1
  -- do fetching of new games and "missing" games in parallel
  _ <- forkIO (fetchMissing thingdir db)
  fetchNew start 0 db

autocomplete :: DB -> Scotty.ActionM ()
autocomplete db = do
  string <- Text.toCaseFold <$> Scotty.param "string"
  callback <- Scotty.param "callback"
  let bytestring = encodeUtf8 string
  (trie, _) <- liftIO $ readIORef db
  let things = concat $ Trie.elems $ Trie.submap bytestring trie
  let completions = map (\(title, t) -> (title, id t)) things
  let array = LazyText.toLazyText . Aeson.encodeToTextBuilder . Aeson.toJSON $ completions
  let js = callback <> "(" <> array <> ")"
  Scotty.status ok200
  Scotty.setHeader "Content-type" "text/javascript; charset=utf-8"
  Scotty.text js

describe :: DB -> Scotty.ActionM ()
describe db = do
  ids <- Scotty.param "ids"
  callback <- Scotty.param "callback"
  (_, m) <- liftIO $ readIORef db
  -- frontend should not show thumbnails with empty URL
  let lkp id = fromMaybe (Thing {id, typ = "", thumbnail = ""}) (IntMap.lookup id m)
  let l = map lkp ids
  let array = LazyText.toLazyText . Aeson.encodeToTextBuilder . Aeson.toJSON $ l
  let js = callback <> "(" <> array <> ")"
  Scotty.status ok200
  Scotty.setHeader "Content-type" "text/javascript; charset=utf-8"
  Scotty.text js

application :: DB -> IO Application
application db =
  Scotty.scottyApp $ do
    Scotty.middleware (gzip def)
    Scotty.get "/autocomplete" (autocomplete db)
    Scotty.get "/describe" (describe db)

main :: IO ()
main = withStdoutLogging $ do
  thingdir <- getEnv "GBM_SERVER_THING_DIR"
  t <- newIORef (Trie.empty, IntMap.empty)
  _ <- forkIO (loadThings thingdir t)
  app <- application t
  exePath <- getExecutablePath
  let pkgPath = takeDirectory (takeDirectory exePath)
  let certPath = pkgPath </> "certificate.pem"
  let keyPath = pkgPath </> "key.pem"
  let settings = tlsSettings certPath keyPath
  runTLS settings (setPort 443 defaultSettings) app
