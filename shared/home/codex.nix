{ ... }: {
  home.file.".codex/config.toml".text = ''
    model = "gpt-5.3-codex"
    approval_policy = "on-request"
    sandbox_mode = "workspace-write"
    web_search = "live"
    [sandbox_workspace_write]
    network_access = true
  '';
}
