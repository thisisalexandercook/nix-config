# ./gemini.nix
{ config, pkgs, ... }:

let
  gemini-wrapped = pkgs.writeShellScriptBin "gemini" ''
    export GEMINI_API_KEY="$(cat ${config.sops.secrets.gemini_api_key.path})"
    exec ${pkgs.nodejs}/bin/npx --yes @google/gemini-cli@latest "$@"
  '';
in
{
  home.packages = [
    gemini-wrapped
    pkgs.nodejs
  ];

  home.file.".gemini/settings.json" = {
    text = builtins.toJSON {
      ui = {
        theme = "GitHub Light";
        language = "en";
      };
      security = {
        auth = {
          selectedType = "gemini-api-key";
        };
      };
      general = {
        previewFeatures = true;
        disableAutoUpdate = true;
      };
      generationConfig = {
        temperature = 0.2;
        maxOutputTokens = 8192;
      };
    };
    force = true;
  };
}
