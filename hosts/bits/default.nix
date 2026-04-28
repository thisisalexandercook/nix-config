# hosts/bits/default.nix
{ pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  networking.hostName = "bits";

  boot.kernelPackages = pkgs.linuxPackages_latest;
}
