{
  pkgs ? (import <nixpkgs> {}).pkgs,
  haskellPackages ? pkgs.haskellPackages }: let env = haskellPackages.ghcWithPackages (p: with p; [
  transformers
  filepath
  directory
  containers
  bytestring
  text
  errors
  lens
  logging
  http-client
  wreq-sb
  xml
  http-types
  wai
  aeson
  warp
  warp-tls
  scotty
  wai-extra
  bytestring-trie
]); in
pkgs.stdenv.mkDerivation {
  name = "gbm-server";
  buildInputs = [env];
}
