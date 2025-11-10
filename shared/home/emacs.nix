# ./emacs.nix
{ config, pkgs, ... }:

let
  sourcePath = config.home.homeDirectory + "/nix-config/shared/home/.emacs.d";
  configSrc = config.lib.file.mkOutOfStoreSymlink sourcePath;
in

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
  };

  home.file.".emacs.d".source = configSrc;
}
