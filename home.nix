# ./home.nix
{ pkgs, ... }: {

  imports = [
    ./emacs.nix
  ];

  home.stateVersion = "25.05";

  programs.bash.enable = true;
  programs.git = {
    enable = true;

    settings = {
      user = {
        name = "Alex Cook";
        email = "a5cook@uwaterloo.ca";
      };
      init.defaultBranch = "main";
    };
  };

  home.packages = [
    pkgs.rocq-core
    pkgs.jdk
    pkgs.xournalpp
    pkgs.python3
  ];
}
