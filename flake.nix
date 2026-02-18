{
  description = "main config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    codex-cli-nix = {
      url = "github:sadjow/codex-cli-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {self, nixpkgs, home-manager, nixvim, sops-nix, flake-utils, ...}@inputs:
    let
      lib = nixpkgs.lib;
      system = "x86_64-linux";
      specialArgs = { inherit inputs; };
    in
      (flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          setupJdkSource = jdkPackage: folderName: ''
            LOCAL_SRC_DIR="$HOME/jdk-sources/${folderName}"
            JDK_ZIP="${jdkPackage}/lib/src.zip"

            if [ ! -d "$LOCAL_SRC_DIR" ]; then
              if [ -f "$JDK_ZIP" ]; then
                mkdir -p "$LOCAL_SRC_DIR"
                ${pkgs.unzip}/bin/unzip -q "$JDK_ZIP" -d "$LOCAL_SRC_DIR"
                chmod -R u+w "$LOCAL_SRC_DIR"
              else
                echo "Warning: Could not find src.zip at $JDK_ZIP"
              fi
            fi
          '';
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

              # jdk 25 dev shell
              java25 = pkgs.mkShell {
                buildInputs = with pkgs; [
                  javaPackages.compiler.openjdk25
                  gradle
                  jdt-language-server
                  jol
                ];
                shellHook = ''
                  ${setupJdkSource pkgs.javaPackages.compiler.openjdk25 "jdk25"}
                  export JAVA_HOME=${pkgs.javaPackages.compiler.openjdk25}
                  export JOL_CLI_JAR=${pkgs.jol}/share/jol-cli/jol-cli.jar
                  export JOL_CP="build/classes/java/main:build/resources/main:build/classes/java/test:build/resources/test"
                '';
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
              home-manager.backupFileExtension = "hm-backup";
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
              home-manager.backupFileExtension = "hm-backup";
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
