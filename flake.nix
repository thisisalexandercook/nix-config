{
  description = "main config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {self, nixpkgs, nixpkgs-unstable, home-manager, sops-nix , flake-utils, ...}@inputs:
    let
      lib = nixpkgs.lib;
      system = "x86_64-linux";
      specialArgs = { inherit inputs; };
    in
      (flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          pkgs-unstable = nixpkgs-unstable.legacyPackages.${system};
        in
          {
            devShells = {

              # rocq dev shell
              rocq = pkgs.mkShell {
                buildInputs = with pkgs; [
                  rocq-core
                  rocqPackages.stdlib
                ];
              };

              # jdk 21 dev shell
              java21 = pkgs.mkShell {
                buildInputs = with pkgs; [
                  jdk21
                  gradle
                  jdt-language-server
                ];
              };

              # jdk 25 dev shell
              java25 = pkgs-unstable.mkShell {
                buildInputs = with pkgs-unstable; [
                  javaPackages.compiler.openjdk25
                  gradle
                  jdt-language-server
                ];
              };
            };
          }
      ))
      //
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

