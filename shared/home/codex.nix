{ pkgs, ... }:
let
  codex-acp-wrapped = pkgs.writeShellScriptBin "codex-acp" ''
    exec ${pkgs.nodejs}/bin/npx --yes @zed-industries/codex-acp@latest "$@"
  '';
in
{
  home.packages = [
    codex-acp-wrapped
    pkgs.nodejs
  ];

  home.file.".codex/config.toml".text = ''
    model = "gpt-5.3-codex"
    approval_policy = "on-request"
    sandbox_mode = "workspace-write"
    web_search = "live"
    [sandbox_workspace_write]
    network_access = true
  '';
}
