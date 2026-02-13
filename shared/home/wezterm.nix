{ pkgs, ... }:

{
  programs.wezterm = {
    enable = true;
    extraConfig = ''
      local wezterm = require 'wezterm'
      local config = wezterm.config_builder()
      local act = wezterm.action

      -- Appearance
      config.color_scheme = 'Modus-Operandi'
      config.font = wezterm.font 'IBM Plex Mono'
      config.font_size = 13.0
      config.window_background_opacity = 1.0
      config.hide_tab_bar_if_only_one_tab = true
      config.notification_handling = 'NeverShow'
      config.audible_bell = 'Disabled'
      config.visual_bell = {
        fade_in_duration_ms = 0,
        fade_out_duration_ms = 0,
      }

      -- Start directly in tmux so pane/session management is unified.
      config.default_prog = { 'tmux', 'new-session', '-A', '-s', 'main' }

      -- Keybindings
      config.keys = {
        -- Standard Terminal Copy/Paste (Ctrl+Shift+C/V)
        { key = 'C', mods = 'CTRL|SHIFT', action = act.CopyTo 'Clipboard' },
        { key = 'V', mods = 'CTRL|SHIFT', action = act.PasteFrom 'Clipboard' },
      }

      -- Mouse bindings
      config.mouse_bindings = {
        -- Hold Super (Windows/Command) and drag anywhere to move the window.
        { event = { Down = { streak = 1, button = 'Left' } }, mods = 'SUPER', action = 'Nop' },
        { event = { Drag = { streak = 1, button = 'Left' } }, mods = 'SUPER', action = wezterm.action.StartWindowDrag },
      }

      return config
    '';
  };
}
