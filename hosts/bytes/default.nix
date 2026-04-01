# hosts/bytes/default.nix
{ config, lib, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ./forge.nix ./forge-backup.nix ./forge-backup-job.nix ];

  fileSystems."/mnt/backup" = {
    device = "/dev/disk/by-uuid/c13e63fd-7167-4a11-8dfe-608d6f0242a7";
    fsType = "ext4";
  };

  fileSystems."/mnt/data" = {
    device = lib.mkForce "/dev/disk/by-uuid/362b3928-ce58-410a-b1e7-1e0f10f46d3b";
    fsType = "ext4";
    options = [ "nofail" "x-systemd.device-timeout=10s" ];
  };

  networking.hostName = "bytes";

  # kernal that helps with freezing issue
  # boot.kernelPackages = pkgs.linuxPackagesFor (
  #   pkgs.linux_6_12.override {
  #     argsOverride = rec {
  #       src = pkgs.fetchurl {
  #         url = "mirror://kernel/linux/kernel/v6.x/linux-${version}.tar.xz";
  #         sha256 = "sha256-sExbPl324KpenNHv5Sf6yZ+d05pDuX8Tsi+MqT5SS6c=";
  #       };
  #       version = "6.12.31";
  #       modDirVersion = "6.12.31";
  #     };
  #   }
  # );

  # Use latest kernel.
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Tailscale
  sops.secrets.tailscale_auth_key = { };
  services.tailscale = {
    authKeyFile = config.sops.secrets.tailscale_auth_key.path;
  };

   # SSD permissions
   systemd.tmpfiles.rules = [
  # Type Path       Mode  User  Group  Age  Argument
  "d    /mnt/data  0755  alex  users  -    -"
];

}
