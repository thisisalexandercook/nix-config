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

      globals.mapleader = " ";

    opts = {
      number = true;
      relativenumber = true;
      shiftwidth = 2;
      tabstop = 2;
      expandtab = true;
      smartindent = true;
      clipboard = "unnamedplus";
      background = "light";
      autoread = true;
    };

    extraPlugins = with pkgs.vimPlugins; [
      modus-themes-nvim
      vim-tmux-navigator
    ];

    extraConfigLua = ''
      vim.cmd.colorscheme("modus_operandi")

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

      -- Keep Neogit status fresh when returning focus or idling in that buffer.
      vim.api.nvim_create_autocmd({ "FocusGained", "BufEnter", "CursorHold" }, {
        callback = function(args)
          local ft = vim.bo[args.buf].filetype
          if ft == "NeogitStatus" then
            pcall(function()
              require("neogit").refresh()
            end)
          end
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
      action = "<cmd>lua require('neogit').refresh()<cr>";
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
        kind = "replace";
        disableHints = false;
        settings = {
          auto_close = true;
          auto_refresh = true;
          filewatcher = {
            enabled = true;
          };
          remember_settings = false;
          use_per_project_settings = false;
        };
      };
    };
  };
}
