# ./home.nix
{ config, pkgs, inputs, ... }: {

  imports = [
    ./codex.nix
    ./emacs.nix
    ./nvim.nix
    ./projects.nix
    ./tmux.nix
    ./gemini.nix
    ./wezterm.nix
  ];

  fonts.fontconfig.enable = true;

  home.stateVersion = "25.05";

  programs.bash = {
    enable = true;

    sessionVariables = {
      EDITOR = "emacs";
    };
  };

  programs.git = {
    enable = true;
    lfs.enable = true;

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

    secrets.gemini_api_key = {
      key = "gemini_api_key";
      mode = "0400";
    };

    secrets.forgejo_key = {
      key = "forgejo_key";
      mode = "0400";
    };

    secrets.forgejo_backup_key = {
      key = "forgejo_backup_key";
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
        "bytes" = {
        hostname = "bytes";
        user = "forgejo";
        identityFile = "${config.sops.secrets.forgejo_key.path}";
      };
      "bytes-backup" = {
        hostname = "bytes";
        port = 2222;
        user = "forgejo";
        identityFile = "${config.sops.secrets.forgejo_backup_key.path}";
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

  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "application/pdf" = [ "org.pwmt.zathura.desktop" ];
      "application/x-pdf" = [ "org.pwmt.zathura.desktop" ];
      "application/acrobat" = [ "org.pwmt.zathura.desktop" ];
      "applications/vnd.pdf" = [ "org.pwmt.zathura.desktop" ];
      "text/pdf" = [ "org.pwmt.zathura.desktop" ];
      "text/x-pdf" = [ "org.pwmt.zathura.desktop" ];
    };
  };

  xdg.configFile."zathura/zathurarc".text = ''
    # Modus Operandi-aligned light palette
    set default-bg              "#ffffff"
    set default-fg              "#000000"
    set guioptions              ""
    set statusbar-bg            "#c8c8c8"
    set statusbar-fg            "#0a0a0a"
    set inputbar-bg             "#f2f2f2"
    set inputbar-fg             "#000000"
    set notification-bg         "#f2f2f2"
    set notification-fg         "#000000"
    set notification-error-bg   "#f2f2f2"
    set notification-error-fg   "#a60000"
    set notification-warning-bg "#f2f2f2"
    set notification-warning-fg "#813e00"
    set highlight-color         "rgba(0,52,151,0.35)"
    set highlight-fg            "rgba(255,255,255,0.85)"
    set completion-bg           "#e6e6e6"
    set completion-fg           "#000000"
    set completion-highlight-bg "#003497"
    set completion-highlight-fg "#ffffff"
    set recolor-lightcolor      "#ffffff"
    set recolor-darkcolor       "#000000"
    set selection-clipboard     "clipboard"
    map y exec "xclip -o -selection primary | xclip -i -selection clipboard"
  '';

  home.packages = [
    pkgs.xournalpp
    pkgs.zathura
    pkgs.xclip
    pkgs.python3
    pkgs.rclone
    pkgs.syncthing
    pkgs.unzip
    pkgs.direnv
    pkgs.nix-direnv
    pkgs.ibm-plex
    pkgs.aporetic
    pkgs.jdk21
    pkgs.texlive.combined.scheme-full
    pkgs.ott
    pkgs.gradle
    pkgs.gnumake
    pkgs.cmake
    pkgs.libtool
    pkgs.gcc
    pkgs.tmux
    pkgs.git-lfs
    inputs.codex-cli-nix.packages.${pkgs.system}.default
    pkgs.ripgrep
    (pkgs.aspellWithDicts (dicts: [
      dicts.en
      dicts.en-computers
      dicts.en-science
    ]))
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
