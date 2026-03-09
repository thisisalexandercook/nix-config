{ lib, ... }:
let
  monitorsFile = ./monitors/monitors.xml;
in
lib.mkIf (builtins.pathExists monitorsFile) {
  xdg.configFile."monitors.xml".source = monitorsFile;
}
