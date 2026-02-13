{ pkgs, ... }:

let
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
    tmux split-window -h -t "$session_name:sandbox" -c "$project_root" "nvim '+Neogit'"
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
      bind-key -n M-n switch-client -n
      bind-key -n M-p switch-client -p
      bind-key -n M-S choose-tree -s
      bind-key -n C-h if-shell "$is_vim" "send-keys C-h" "select-pane -L"
      bind-key -n C-j if-shell "$is_vim" "send-keys C-j" "select-pane -D"
      bind-key -n C-k if-shell "$is_vim" "send-keys C-k" "select-pane -U"
      bind-key -n C-l if-shell "$is_vim" "send-keys C-l" "select-pane -R"
      bind-key -n C-\\ if-shell "$is_vim" "send-keys C-\\" "select-pane -l"
    '';
  };

  home.packages = [
    proj
    pkgs.fzf
  ];
}
