# hosts/nibble/default.nix
{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  networking.hostName = "nibble";

  # Framework 13 (non-touch) host defaults.
  # Keep kernel current for newer laptop hardware support.
  boot.kernelPackages = pkgs.linuxPackages_latest;
}
