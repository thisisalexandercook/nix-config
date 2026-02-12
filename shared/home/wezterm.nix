{ pkgs, ... }:

{
  programs.wezterm = {
    enable = true;
    extraConfig = ''
      local wezterm = require 'wezterm'
      local config = wezterm.config_builder()
      local act = wezterm.action

      -- Appearance
      config.color_scheme = 'Tomorrow Night Bright'
      config.font = wezterm.font 'Aporetic Sans Mono'
      config.font_size = 13.0
      config.window_background_opacity = 1.0
      config.hide_tab_bar_if_only_one_tab = true

      -- Keybindings
      config.leader = { key = 'Space', mods = 'CTRL', timeout_milliseconds = 1000 }
      config.keys = {
        -- Vim-style pane management with a CTRL+Space leader
        { key = 'h', mods = 'LEADER', action = act.ActivatePaneDirection 'Left' },
        { key = 'j', mods = 'LEADER', action = act.ActivatePaneDirection 'Down' },
        { key = 'k', mods = 'LEADER', action = act.ActivatePaneDirection 'Up' },
        { key = 'l', mods = 'LEADER', action = act.ActivatePaneDirection 'Right' },
        { key = 's', mods = 'LEADER', action = act.SplitHorizontal { domain = 'CurrentPaneDomain' } },
        { key = 'v', mods = 'LEADER', action = act.SplitVertical { domain = 'CurrentPaneDomain' } },
        { key = 'c', mods = 'LEADER', action = act.SpawnTab 'CurrentPaneDomain' },
        { key = '[', mods = 'LEADER', action = act.ActivateCopyMode },

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
