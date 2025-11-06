{
  description = "main config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {self, nixpkgs, home-manager, ...}:
    let
      lib = nixpkgs.lib;
      system = "x86_64-linux";
    in
      {
        nixosConfigurations = {
          nixos = lib.nixosSystem {
            inherit system;
            modules = [ ./configuration.nix
                        home-manager.nixosModules.home-manager ];
          };
        };
      };
}
