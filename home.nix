# ./home.nix
{ config, pkgs, ... }: {

  imports = [
    ./emacs.nix
  ];

  home.stateVersion = "25.05";

  programs.bash = {
    enable = true;

    sessionVariables = {
      EDITOR = "emacs";
    };
  };

programs.git = {
  enable = true;

  userName = "Alex Cook";
  userEmail = "a5cook@uwaterloo.ca";

  extraConfig = {
    init.defaultBranch = "main";
  };
};

sops = {
    age.keyFile = "/home/alex/.config/sops/age/keys.txt";
    defaultSopsFile = ./secrets/secrets.yaml; # Assumes home.nix is in the same dir

    secrets.github_key = {
      key = "github_key";
      mode = "0400";
    };
  };

programs.ssh = {
    enable = true;

    # We are using matchBlocks as you suggested
    matchBlocks = {
      "github.com" = {
        # 'github.com' is the host
        hostname = "github.com";
        user = "git";
        identityFile = "${config.sops.secrets.github_key.path}";
      };
    };
  };

  home.packages = [
    pkgs.rocq-core
    pkgs.jdk
    pkgs.xournalpp
    pkgs.python3
    pkgs.rclone
  ];
}
