# hosts/nibble/hardware-configuration.nix
# Generate and replace this file on nibble with:
#   sudo nixos-generate-config --show-hardware-config > /home/alex/nix-config/hosts/nibble/hardware-configuration.nix
{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  # Framework 13 typically uses NVMe + USB storage in initrd.
  boot.initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  # Replace UUIDs after generating on nibble.
  fileSystems."/" =
    { device = "/dev/disk/by-uuid/REPLACE_ME_ROOT_UUID";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/REPLACE_ME_BOOT_UUID";
      fsType = "vfat";
      options = [ "fmask=0077" "dmask=0077" ];
    };

  swapDevices = [ ];

  networking.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
