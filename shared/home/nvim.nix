{ inputs, pkgs, ... }:

{
  xdg.configFile."nvim/lua/config".source = ./nvim/lua/config;

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
      vim-tmux-navigator
    ];

    extraConfigLua = ''
      vim.cmd.colorscheme("modus_operandi")
      require("config.gitsigns").setup()
      require("config.oil").setup()
      require("which-key").setup({})

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

      vim.api.nvim_create_autocmd("FileType", {
        pattern = "fugitive",
        callback = function(args)
          vim.bo[args.buf].buflisted = false
          vim.schedule(function()
            for _, win in ipairs(vim.api.nvim_tabpage_list_wins(0)) do
              pcall(vim.api.nvim_set_option_value, "winfixheight", false, { win = win })
              pcall(vim.api.nvim_set_option_value, "winfixwidth", false, { win = win })
            end
            vim.cmd("wincmd =")
          end)
          vim.keymap.set("n", "R", "<cmd>edit<cr>", {
            buffer = args.buf,
            silent = true,
            desc = "Refresh Fugitive buffer",
          })
        end,
      })

      -- Open PDFs externally in Zathura instead of editing binary content.
      vim.api.nvim_create_autocmd("BufReadPost", {
        pattern = "*.pdf",
        callback = function(args)
          local file = vim.api.nvim_buf_get_name(args.buf)
          if file == "" then
            return
          end

          vim.fn.jobstart({ "zathura", file }, { detach = true })
          vim.schedule(function()
            if vim.api.nvim_buf_is_valid(args.buf) then
              vim.api.nvim_buf_delete(args.buf, { force = true })
            end
          end)
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
      action = "<cmd>Oil<cr>";
      options = { desc = "Open explorer buffer"; };
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
      key = "<leader>gS";
      action = "<cmd>Git<cr>";
      options = { desc = "Fugitive status"; };
    }
    {
      mode = "n";
      key = "<leader>gl";
      action = "<cmd>Git log --graph --oneline --decorate --all<cr>";
      options = { desc = "Git log graph"; };
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
    {
      mode = "n";
      key = "<A-H>";
      action = "<cmd>vertical resize -5<cr>";
      options = { desc = "Resize split left (Alt+Shift+h)"; };
    }
    {
      mode = "n";
      key = "<A-L>";
      action = "<cmd>vertical resize +5<cr>";
      options = { desc = "Resize split right (Alt+Shift+l)"; };
    }
    {
      mode = "n";
      key = "<A-K>";
      action = "<cmd>resize +3<cr>";
      options = { desc = "Resize split up (Alt+Shift+k)"; };
    }
    {
      mode = "n";
      key = "<A-J>";
      action = "<cmd>resize -3<cr>";
      options = { desc = "Resize split down (Alt+Shift+j)"; };
    }
    ];

    plugins = {
      gitsigns = {
        enable = true;
        settings = {
          signcolumn = true;
          numhl = false;
          linehl = false;
          word_diff = false;
          current_line_blame = false;
        };
      };
      fugitive.enable = true;
      oil = {
        enable = true;
        settings = {
          default_file_explorer = true;
          view_options = {
            show_hidden = true;
          };
          win_options = {
            winbar = "%{v:lua.oil_winbar()}";
            winhighlight = "WinBar:Normal,WinBarNC:Normal";
          };
        };
      };
      tiny-glimmer = {
        enable = true;
        settings = {
          overwrite = {
            auto_map = true;
            yank = {
              enabled = true;
              default_animation = "pulse";
            };
            paste = {
              enabled = false;
            };
            search = {
              enabled = false;
            };
            undo = {
              enabled = false;
            };
            redo = {
              enabled = false;
            };
          };
          animations = {
            pulse = {
              from_color = "Visual";
              to_color = "Normal";
              pulse_count = 1;
              intensity = 1.06;
              chars_for_max_duration = 30;
              min_duration = 320;
              max_duration = 620;
            };
          };
        };
      };
      lualine = {
        enable = true;
        settings = {
          options = {
            section_separators = {
              left = "|";
              right = "|";
            };
            component_separators = {
              left = "|";
              right = "|";
            };
          };
        };
      };
      treesitter.enable = true;

      telescope = {
        enable = true;
        extensions = {
          projects.enable = true;
        };
        settings = {
          pickers = {
            find_files = {
              hidden = true;
            };
          };
        };
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
