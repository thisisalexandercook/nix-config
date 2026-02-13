{ ... }:

{
  programs.tmux = {
    enable = true;
    clock24 = true;
    keyMode = "vi";
    mouse = true;
    historyLimit = 100000;
    terminal = "screen-256color";
    prefix = "C-Space";
    escapeTime = 0;
    baseIndex = 1;

    extraConfig = ''
      set -g renumber-windows on

      bind | split-window -h
      bind - split-window -v
      bind v split-window -h
      bind s split-window -v

      # Seamless pane navigation with Neovim splits.
      is_vim="ps -o state= -o comm= -t '#{pane_tty}' | grep -iqE '^[^TXZ ]+ +(\S+\/)?g?(view|l?n?vim?x?)(diff)?$'"
      bind-key -n M-v if-shell "$is_vim" "send-keys M-v" "split-window -h"
      bind-key -n M-s if-shell "$is_vim" "send-keys M-s" "split-window -v"
      bind-key -n M-q if-shell "$is_vim" "send-keys M-q" "kill-pane"
      bind-key -n C-h if-shell "$is_vim" "send-keys C-h" "select-pane -L"
      bind-key -n C-j if-shell "$is_vim" "send-keys C-j" "select-pane -D"
      bind-key -n C-k if-shell "$is_vim" "send-keys C-k" "select-pane -U"
      bind-key -n C-l if-shell "$is_vim" "send-keys C-l" "select-pane -R"
      bind-key -n C-\\ if-shell "$is_vim" "send-keys C-\\" "select-pane -l"
    '';
  };
}
