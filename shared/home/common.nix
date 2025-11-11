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
  defaultSopsFile = ../../secrets/secrets.yaml;

  secrets.github_key = {
    key = "github_key";
    mode = "0400";
  };

  secrets.rclone_config = {
    key = "rclone_config";
    mode = "0400";
  };

  secrets.syncthing_password = {
    key = "syncthing_password";
    mode = "0400";
  };
};

programs.rclone = {
  enable = true;

  remotes = {
    gdrive = {
      config = {
        type = "drive";
        scope = "drive";
      };
      secrets = {
        token = config.sops.secrets.rclone_config.path;
      };
      mounts = {
        "books" = {
          enable = true;
          mountPoint = "${config.home.homeDirectory}/books";
          options = {
            vfs_cache_mode = "full";
            dir_cache_time = "10m";
          };
        };
      };
    };
  };
};

services.syncthing = {
  enable = true;

  guiAddress = "127.0.0.1:8384";
  passwordFile = config.sops.secrets.syncthing_password.path;

  settings = {
    gui = {
      user = "alex";
    };
  };
};

programs.ssh = {
    enable = true;

    matchBlocks = {
      "github.com" = {
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
    pkgs.syncthing
  ];

  # dconf settings (GNOME only)
  dconf = {
    enable = true;
    settings = {
      "org/gnome/desktop/input-sources" = {
        xkb-options = [ "ctrl:nocaps" ];
      };
    };
  };
}
