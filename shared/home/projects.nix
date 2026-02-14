{ config, lib, ... }:

let
  curatedProjects = [
    "${config.home.homeDirectory}/nix-config"
    "${config.home.homeDirectory}/eisop/checker-framework"
    "${config.home.homeDirectory}/eisop/runtime-framework"
    "${config.home.homeDirectory}/codex"
  ];
in
{
  xdg.configFile."proj/projects".text =
    lib.concatStringsSep "\n" curatedProjects + "\n";
}
