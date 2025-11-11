# hosts/bits/default.nix
{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  networking.hostName = "bits";

  boot.initrd.kernelModules = [ "pinctrl_tigerlake" ];
  hardware.sensor.iio.enable = true;

  # Use latest kernel.
  boot.kernelPackages = pkgs.linuxPackages_latest;
}
