{
  description = "main config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {self, nixpkgs, home-manager, ...}:
    let
      lib = nixpkgs.lib;
    in
    {
    nixosConfigurations = {
      nixos = lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ ./configuration.nix
                    home-manager.nixosModules.home-manager];
      };
    };
  };
}
