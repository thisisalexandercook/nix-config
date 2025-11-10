{
  description = "main config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {self, nixpkgs, home-manager, sops-nix , ...}@inputs:
    let
      lib = nixpkgs.lib;
      system = "x86_64-linux";
      specialArgs = { inherit inputs; };
    in
      {
        nixosConfigurations = {

          # Laptop Host
          bits = lib.nixosSystem {
            inherit system specialArgs;
            modules = [
              sops-nix.nixosModules.sops
              ./shared/system/common.nix
              ./hosts/bits
              home-manager.nixosModules.home-manager
              {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.extraSpecialArgs = specialArgs;
                home-manager.users.alex = {
                  imports = [
                    ./shared/home/common.nix
                    ./hosts/bits/home.nix
                    inputs.sops-nix.homeManagerModules.sops
                  ];
                };
              }
            ];
          };

          # Desktop Host
          bytes = lib.nixosSystem {
            inherit system specialArgs;
            modules = [
              ./shared/system/common.nix
              ./hosts/bytes
              sops-nix.nixosModules.sops
              home-manager.nixosModules.home-manager
              {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.extraSpecialArgs = specialArgs;
                home-manager.users.alex = {
                  imports = [
                    ./shared/home/common.nix
                    ./hosts/bytes/home.nix
                    inputs.sops-nix.homeManagerModules.sops
                  ];
                };
              }
            ];
          };
        };
      };
}
