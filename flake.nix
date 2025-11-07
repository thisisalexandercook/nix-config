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

    outputs = {self, nixpkgs, home-manager, sops-nix , ...}:
    let
      lib = nixpkgs.lib;
      system = "x86_64-linux";
    in
      {
        nixosConfigurations = {
          nixos = lib.nixosSystem {
            inherit system;
            modules = [
              ./configuration.nix
              home-manager.nixosModules.home-manager
              sops-nix.nixosModules.sops
            ];
          };
        };
      };
}
