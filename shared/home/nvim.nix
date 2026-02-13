{ inputs, pkgs, ... }:

{
  imports = [
    inputs.nixvim.homeManagerModules.nixvim
  ];

  programs.nixvim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    defaultEditor = false; # keep Emacs as $EDITOR while we ramp up

      globals = {
        mapleader = " ";
        loaded_netrw = 1;
        loaded_netrwPlugin = 1;
      };

    opts = {
      number = true;
      relativenumber = true;
      shiftwidth = 2;
      tabstop = 2;
      showtabline = 0;
      expandtab = true;
      smartindent = true;
      breakindent = true;
      breakindentopt = "shift:2,sbr";
      linebreak = true;
      clipboard = "unnamedplus";
      background = "light";
      autoread = true;
    };

    extraPlugins = with pkgs.vimPlugins; [
      modus-themes-nvim
      which-key-nvim
      oil-nvim
      vim-tmux-navigator
      tiny-glimmer-nvim
    ];

    extraConfigLua = ''
      vim.cmd.colorscheme("modus_operandi")
      require("which-key").setup({})
      _G.oil_winbar = function()
        local ok, oil = pcall(require, "oil")
        if not ok then
          return ""
        end
        local dir = oil.get_current_dir()
        if not dir then
          return ""
        end
        return vim.fn.fnamemodify(dir, ":~")
      end
      require("oil").setup({
        default_file_explorer = true,
        win_options = {
          winbar = "%{v:lua.oil_winbar()}",
          winhighlight = "WinBar:Normal,WinBarNC:Normal",
        },
      })
      require("lualine").setup({
        options = {
          section_separators = { left = "|", right = "|" },
          component_separators = { left = "|", right = "|" },
        },
      })
      require("tiny-glimmer").setup({
        overwrite = {
          auto_map = true,
          yank = {
            enabled = true,
            default_animation = "pulse",
          },
          paste = {
            enabled = false,
          },
          search = {
            enabled = false,
          },
          undo = {
            enabled = false,
          },
          redo = {
            enabled = false,
          },
        },
        animations = {
          pulse = {
            from_color = "Visual",
            to_color = "Normal",
            pulse_count = 1,
            intensity = 1.06,
            chars_for_max_duration = 30,
            min_duration = 320,
            max_duration = 620,
          },
        },
      })

      -- Refresh buffers changed on disk on focus/idle events.
      -- checktime does not overwrite unsaved edits.
      vim.api.nvim_create_autocmd({ "FocusGained", "BufEnter", "CursorHold", "CursorHoldI" }, {
        pattern = "*",
        command = "checktime",
      })

      vim.api.nvim_create_autocmd("FileChangedShellPost", {
        pattern = "*",
        callback = function()
          vim.notify("File changed on disk. Buffer reloaded.", vim.log.levels.INFO)
        end,
      })

    '';

    keymaps = [
    {
      mode = "n";
      key = "<leader>pp";
      action = "<cmd>Telescope projects<cr>";
      options = { desc = "Pick project"; };
    }
    {
      mode = "n";
      key = "<leader>ff";
      action = "<cmd>Telescope find_files<cr>";
      options = { desc = "Find file"; };
    }
    {
      mode = "n";
      key = "<leader>e";
      action = "<cmd>Oil --float<cr>";
      options = { desc = "Open explorer"; };
    }
    {
      mode = "n";
      key = "<leader>fb";
      action = "<cmd>Telescope buffers<cr>";
      options = { desc = "Find buffer"; };
    }
    {
      mode = "n";
      key = "<leader>fg";
      action = "<cmd>Telescope live_grep<cr>";
      options = { desc = "Live grep"; };
    }
    {
      mode = "n";
      key = "<leader>gs";
      action = "<cmd>Neogit<cr>";
      options = { desc = "Git status"; };
    }
    {
      mode = "n";
      key = "<leader>gR";
      action = "<cmd>lua pcall(function() require('neogit').refresh() end)<cr>";
      options = { desc = "Refresh Neogit"; };
    }
    {
      mode = "n";
      key = "<leader>v";
      action = "<cmd>vsplit<cr>";
      options = { desc = "Vertical split"; };
    }
    {
      mode = "n";
      key = "<leader>s";
      action = "<cmd>split<cr>";
      options = { desc = "Horizontal split"; };
    }
    {
      mode = "n";
      key = "<A-v>";
      action = "<cmd>vsplit<cr>";
      options = { desc = "Vertical split (Alt+v)"; };
    }
    {
      mode = "n";
      key = "<A-s>";
      action = "<cmd>split<cr>";
      options = { desc = "Horizontal split (Alt+s)"; };
    }
    {
      mode = "n";
      key = "<A-q>";
      action = "<cmd>close<cr>";
      options = { desc = "Close split (Alt+q)"; };
    }
    ];

    plugins = {
      gitsigns.enable = true;
      lualine.enable = true;
      treesitter.enable = true;

      telescope = {
        enable = true;
        extensions.projects.enable = true;
      };

      project-nvim = {
        enable = true;
        detectionMethods = [ "lsp" "pattern" ];
        patterns = [ ".git" "flake.nix" "package.json" "pyproject.toml" ];
        manualMode = false;
      };

      neogit = {
        enable = true;
        kind = "split";
        disableHints = false;
        settings = {
          auto_close = false;
          auto_refresh = false;
          filewatcher = {
            enabled = false;
          };
          remember_settings = false;
          use_per_project_settings = false;
        };
      };
    };
  };
}
