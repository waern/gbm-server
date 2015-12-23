{ mkDerivation, aeson, base, bytestring, bytestring-trie
, containers, directory, errors, filepath, http-client, http-types
, lens, logging, scotty, stdenv, text, transformers, wai, wai-extra
, warp, warp-tls, wreq-sb, xml
}:
mkDerivation {
  pname = "gbm-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring bytestring-trie containers directory errors
    filepath http-client http-types lens logging scotty text
    transformers wai wai-extra warp warp-tls wreq-sb xml
  ];
  license = stdenv.lib.licenses.free;
  postInstall = ''
    cp *.pem $out/.
  '';
}
