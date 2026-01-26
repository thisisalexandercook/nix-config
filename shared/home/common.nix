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
    pull.rebase = true;
  };
};

sops = {
  age.keyFile = "/home/alex/.config/sops/age/keys.txt";
  defaultSopsFile = ../../secrets/secrets.yaml;

  secrets.github_key = {
    key = "github_key";
    mode = "0400";
  };

  secrets.rclone_token = {
    key = "rclone_token";
    mode = "0400";
  };

  secrets.rclone_client_id = {
    key = "rclone_client_id";
    mode = "0400";
  };

  secrets.rclone_client_secret = {
    key = "rclone_client_secret";
    mode = "0400";
  };

  secrets.syncthing_password = {
    key = "syncthing_password";
    mode = "0400";
  };

  secrets.gitlab_key = {
    key = "gitlab_key";
    mode = "0400";
  };
};

programs.rclone = {
  enable = true;
  requiresUnit = "sops-nix.service";

  remotes = {
    gdrive = {
      config = {
        type = "drive";
        scope = "drive";
      };
      secrets = {
        client_id = config.sops.secrets.rclone_client_id.path;
        client_secret = config.sops.secrets.rclone_client_secret.path;
        token = config.sops.secrets.rclone_token.path;
      };
      mounts = {
        "books" = {
          enable = true;
          mountPoint = "${config.home.homeDirectory}/books";
          options = {
            vfs-cache-mode = "full";
            dir-cache-time = "10m";
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

    folders = {
      "notes" = {
        path = "${config.home.homeDirectory}/notes";
        id = "notes-folder-id";
        devices = [ "bits" "bytes" ];
      };
      "scratch" = {
        path = "${config.home.homeDirectory}/scratch";
        id = "scratch-folder-id";
        devices = [ "bits" "bytes"];
      };
    };

    devices = {
      "bits" = {
        id = "TVRTKW2-CBPGPV5-4EPO7NC-FCJNX75-6MVXVFE-LIMCX2C-RL45VIY-POMQHQT";
      };
      "bytes" = {
        id = "LP6IK6T-DVBDKZC-EIZN5SK-STYG7LU-NCDA3I6-APUUPWD-YAQAGID-2N7GFQ6";
      };
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
    "git.uwaterloo.ca" = {
      hostname = "git.uwaterloo.ca";
      user = "git";
      identityFile = "${config.sops.secrets.gitlab_key.path}";
    };
  };
};

programs.direnv = {
  enable = true;
  nix-direnv.enable = true;
  config = {
    global = {
      hide_env_diff = true;
    };
  };
};

  home.packages = [
    pkgs.xournalpp
    pkgs.python3
    pkgs.rclone
    pkgs.syncthing
    pkgs.unzip
    pkgs.direnv
    pkgs.nix-direnv
    pkgs.aporetic
    pkgs.jdk21
    pkgs.texlive.combined.scheme-full
    pkgs.ott
    pkgs.gradle
    pkgs.gnumake
    pkgs.gemini-cli
  ];

  # dconf settings (GNOME only)
  dconf = {
    enable = true;
    settings = {
      "org/gnome/desktop/input-sources" = {
        xkb-options = [ "ctrl:nocaps" ];
      };
      "org/gnome/desktop/interface" = {
      enable-hot-corners = false;
    };
    };
  };
}
