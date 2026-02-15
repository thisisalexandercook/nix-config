{ pkgs, ... }:

let
  tmuxPalette = import ./tmux-modus-operandi-palette.nix;
  colors = tmuxPalette.colors;

  proj = pkgs.writeShellScriptBin "proj" ''
    set -euo pipefail

    is_project_dir() {
      [ -d "$1/.git" ] || [ -f "$1/flake.nix" ] || [ -f "$1/package.json" ] || [ -f "$1/pyproject.toml" ]
    }

    collect_projects() {
      local config_home projects_file candidate
      local -a seen_projects

      config_home="''${XDG_CONFIG_HOME:-$HOME/.config}"
      projects_file="$config_home/proj/projects"

      if [ -f "$projects_file" ]; then
        while IFS= read -r candidate || [ -n "$candidate" ]; do
          [ -n "$candidate" ] || continue
          case "$candidate" in
            \#*) continue ;;
          esac
          [ -d "$candidate" ] || continue
          seen_projects+=("$candidate")
        done <"$projects_file"
      fi

      printf '%s\n' "''${seen_projects[@]}" | awk '!seen[$0]++'
    }

    pick_project() {
      local selected

      if ! command -v fzf >/dev/null 2>&1; then
        echo "proj: fzf is required for project picking" >&2
        exit 1
      fi

      selected="$(
        collect_projects | fzf --prompt='Project > ' --height=40% --layout=reverse --border || true
      )"

      if [ -z "$selected" ]; then
        echo "proj: no project selected (or no entries in ~/.config/proj/projects)" >&2
        exit 1
      fi

      printf '%s\n' "$selected"
    }

    use_picker=0
    if [ "''${1:-}" = "-p" ] || [ "''${1:-}" = "--pick" ]; then
      use_picker=1
      shift
    fi

    if [ $# -gt 0 ]; then
      target_dir="$1"
    elif [ "$use_picker" -eq 1 ]; then
      target_dir="$(pick_project)"
    elif is_project_dir "$PWD"; then
      target_dir="$PWD"
    else
      target_dir="$(pick_project)"
    fi

    if ! cd "$target_dir" 2>/dev/null; then
      echo "proj: directory does not exist: $target_dir" >&2
      exit 1
    fi

    project_root="$(pwd -P)"
    session_name="$(printf '%s' "$(basename "$project_root")" | tr -cs '[:alnum:]_-' '_')"

    if [ -z "$session_name" ]; then
      session_name="project"
    fi

    if tmux has-session -t "$session_name" 2>/dev/null; then
      if [ -n "''${TMUX:-}" ]; then
        exec tmux switch-client -t "$session_name"
      fi
      exec tmux attach -t "$session_name"
    fi

    tmux new-session -d -s "$session_name" -n main -c "$project_root"
    tmux send-keys -t "$session_name:main" "nvim ." C-m
    tmux new-window -t "$session_name:" -n sandbox -c "$project_root"
    tmux split-window -h -t "$session_name:sandbox" -c "$project_root" "nvim '+Git' '+wincmd ='"
    tmux select-pane -t "$session_name:sandbox" -L
    tmux select-window -t "$session_name:main"

    if [ -n "''${TMUX:-}" ]; then
      exec tmux switch-client -t "$session_name"
    fi
    exec tmux attach -t "$session_name"
  '';
in
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
      set -g status-position top
      set -g renumber-windows on
      set -g status-justify centre
      set -g status-style "bg=${colors.bgStatusLineActive},fg=${colors.fgStatusLineActive}"
      set -g status-left-length 40
      set -g status-right-length 80
      set -g status-left "#[bold]#{session_name}#[nobold] | #H "
      set -g status-right "#{pane_current_path} "
      set -g window-status-format " #I:#W "
      set -g window-status-current-format " #I:#W "
      set -g window-status-current-style "fg=#ffffff,bg=${colors.blueFaint},bold"
      set -g window-status-style "fg=${colors.fgStatusLineInactive},bg=default,nobold"

      bind-key -N "cfg: split pane horizontally (prefix |)" | split-window -h
      bind-key -N "cfg: split pane vertically (prefix -)" - split-window -v
      bind-key -N "cfg: split pane horizontally (prefix v)" v split-window -h
      bind-key -N "cfg: split pane vertically (prefix s)" s split-window -v

      # Seamless pane navigation with Neovim splits.
      is_vim="ps -o state= -o comm= -t '#{pane_tty}' | grep -iqE '^[^TXZ ]+ +(\S+\/)?g?(view|l?n?vim?x?)(diff)?$'"
      bind-key -N "cfg: split pane horizontally (Alt+v)" -n M-v if-shell "$is_vim" "send-keys M-v" "split-window -h"
      bind-key -N "cfg: split pane vertically (Alt+s)" -n M-s if-shell "$is_vim" "send-keys M-s" "split-window -v"
      bind-key -N "cfg: close pane (Alt+q)" -n M-q if-shell "$is_vim" "send-keys M-q" "kill-pane"
      bind-key -N "cfg: resize pane left (Alt+Shift+h)" -n M-H if-shell "$is_vim" "send-keys M-H" "resize-pane -L 5"
      bind-key -N "cfg: resize pane down (Alt+Shift+j)" -n M-J if-shell "$is_vim" "send-keys M-J" "resize-pane -D 3"
      bind-key -N "cfg: resize pane up (Alt+Shift+k)" -n M-K if-shell "$is_vim" "send-keys M-K" "resize-pane -U 3"
      bind-key -N "cfg: resize pane right (Alt+Shift+l)" -n M-L if-shell "$is_vim" "send-keys M-L" "resize-pane -R 5"
      bind-key -N "cfg: enter copy mode (Alt+c)" -n M-c copy-mode
      bind-key -N "cfg: show key help popup (Alt+/)" -n M-/ display-popup -E "sh -lc 'printf \"tmux key help (live)\\n\\nNo prefix\\n\"; tmux list-keys -N -T root | grep \"cfg:\" | sed \"s/^/  /\"; printf \"\\nPrefix (Ctrl+Space)\\n\"; tmux list-keys -N -T prefix | grep \"cfg:\" | sed \"s/^/  /\"; printf \"\\nPress any key to close...\"; read -r -n 1 -s _'"
      # Fast window cycling within the current project/session.
      bind-key -N "cfg: next window (Alt+n)" -n M-n next-window
      bind-key -N "cfg: previous window (Alt+p)" -n M-p previous-window
      bind-key -N "cfg: jump to window 1 (Alt+1)" -n M-1 select-window -t:=1
      bind-key -N "cfg: jump to window 2 (Alt+2)" -n M-2 select-window -t:=2
      bind-key -N "cfg: jump to window 3 (Alt+3)" -n M-3 select-window -t:=3
      bind-key -N "cfg: jump to window 4 (Alt+4)" -n M-4 select-window -t:=4
      bind-key -N "cfg: jump to window 5 (Alt+5)" -n M-5 select-window -t:=5
      bind-key -N "cfg: jump to window 6 (Alt+6)" -n M-6 select-window -t:=6
      bind-key -N "cfg: jump to window 7 (Alt+7)" -n M-7 select-window -t:=7
      bind-key -N "cfg: jump to window 8 (Alt+8)" -n M-8 select-window -t:=8
      bind-key -N "cfg: jump to window 9 (Alt+9)" -n M-9 select-window -t:=9
      bind-key -N "cfg: jump to window 10 (Alt+0)" -n M-0 select-window -t:=10
      # Session cycling across projects.
      bind-key -N "cfg: next session/project (Alt+Shift+n)" -n M-N switch-client -n
      bind-key -N "cfg: previous session/project (Alt+Shift+p)" -n M-P switch-client -p
      bind-key -N "cfg: choose session tree (Alt+Shift+s)" -n M-S choose-tree -s
      bind-key -N "cfg: move to left pane (Ctrl+h)" -n C-h if-shell "$is_vim" "send-keys C-h" "select-pane -L"
      bind-key -N "cfg: move to lower pane (Ctrl+j)" -n C-j if-shell "$is_vim" "send-keys C-j" "select-pane -D"
      bind-key -N "cfg: move to upper pane (Ctrl+k)" -n C-k if-shell "$is_vim" "send-keys C-k" "select-pane -U"
      bind-key -N "cfg: move to right pane (Ctrl+l)" -n C-l if-shell "$is_vim" "send-keys C-l" "select-pane -R"
      bind-key -N "cfg: move to last pane (Ctrl+\\)" -n C-\\ if-shell "$is_vim" "send-keys C-\\" "select-pane -l"
    '';
  };

  home.packages = [
    proj
    pkgs.fzf
  ];
}
