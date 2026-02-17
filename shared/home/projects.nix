{ config, lib, ... }:

let
  curatedProjects = [
    "${config.home.homeDirectory}/nix-config"
    "${config.home.homeDirectory}/eisop/checker-framework"
    "${config.home.homeDirectory}/eisop/runtime-framework"
    "${config.home.homeDirectory}/codex"
    "${config.home.homeDirectory}/cs644/joos1w-compiler"
    "${config.home.homeDirectory}/scratch/java-sandbox"
  ];
in
{
  xdg.configFile."proj/projects".text =
    lib.concatStringsSep "\n" curatedProjects + "\n";
}
