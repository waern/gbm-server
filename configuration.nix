{ pkgs, ... }:

let gbm-server = pkgs.haskellPackages.callPackage ./default.nix {}; in

{
  imports = [ <nixpkgs/nixos/modules/virtualisation/amazon-image.nix> ];
  ec2.hvm = true;

  networking.firewall.allowedTCPPorts = [ 443 ];

  systemd.services.gbm-server = {
    environment = {
      GBM_SERVER_THING_DIR = "/root/gbm-server/things";
    };

    serviceConfig = {
      ExecStart = "${gbm-server}/bin/gbm-server";
    };
  };
}
