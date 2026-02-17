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
      updatetime = 500;
    };

    autoCmd = [
      {
        event = [ "BufReadPost" ];
        pattern = "*.pdf";
        desc = "Open PDFs with selected viewer";
        command = "lua require('config.pdf').open_current_pdf()";
      }
    ];

    extraPlugins = with pkgs.vimPlugins; [
      modus-themes-nvim
      which-key-nvim
      vim-tmux-navigator
    ];

    extraConfigLua = ''
      vim.cmd.colorscheme("modus_operandi")
      require("config.gitsigns").setup()
      require("config.oil").setup()
      require("config.auto-refresh").setup()
      require("config.java").setup()
      require("which-key").setup({})

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
      key = "<leader>ss";
      action = "<cmd>Telescope lsp_document_symbols<cr>";
      options = { desc = "Document symbols"; };
    }
    {
      mode = "n";
      key = "<leader>sS";
      action = "<cmd>Telescope lsp_workspace_symbols<cr>";
      options = { desc = "Workspace symbols"; };
    }
    {
      mode = "n";
      key = "<leader>ur";
      action = "<cmd>lua require('config.refresh').run()<cr>";
      options = { desc = "Refresh current buffer"; };
    }
    {
      mode = "n";
      key = "gd";
      action = "<cmd>lua vim.lsp.buf.definition()<cr>";
      options = { desc = "LSP go to definition"; };
    }
    {
      mode = "n";
      key = "gD";
      action = "<cmd>lua vim.lsp.buf.declaration()<cr>";
      options = { desc = "LSP go to declaration"; };
    }
    {
      mode = "n";
      key = "gr";
      action = "<cmd>lua vim.lsp.buf.references()<cr>";
      options = { desc = "LSP references"; };
    }
    {
      mode = "n";
      key = "gi";
      action = "<cmd>lua vim.lsp.buf.implementation()<cr>";
      options = { desc = "LSP implementations"; };
    }
    {
      mode = "n";
      key = "K";
      action = "<cmd>lua vim.lsp.buf.hover()<cr>";
      options = { desc = "LSP hover"; };
    }
    {
      mode = "n";
      key = "<C-k>";
      action = "<cmd>lua vim.lsp.buf.signature_help()<cr>";
      options = { desc = "LSP signature help"; };
    }
    {
      mode = "n";
      key = "<leader>rn";
      action = "<cmd>lua vim.lsp.buf.rename()<cr>";
      options = { desc = "LSP rename"; };
    }
    {
      mode = "n";
      key = "<leader>ca";
      action = "<cmd>lua vim.lsp.buf.code_action()<cr>";
      options = { desc = "LSP code action"; };
    }
    {
      mode = "n";
      key = "]d";
      action = "<cmd>lua vim.diagnostic.goto_next()<cr>";
      options = { desc = "Next diagnostic"; };
    }
    {
      mode = "n";
      key = "[d";
      action = "<cmd>lua vim.diagnostic.goto_prev()<cr>";
      options = { desc = "Previous diagnostic"; };
    }
    {
      mode = "n";
      key = "<leader>ld";
      action = "<cmd>lua vim.diagnostic.open_float()<cr>";
      options = { desc = "Line diagnostics"; };
    }
    {
      mode = "n";
      key = "<leader>lf";
      action = "<cmd>lua vim.lsp.buf.format()<cr>";
      options = { desc = "LSP format"; };
    }
    {
      mode = "n";
      key = "<leader>db";
      action = "<cmd>lua require('dap').toggle_breakpoint()<cr>";
      options = { desc = "DAP toggle breakpoint"; };
    }
    {
      mode = "n";
      key = "<leader>dc";
      action = "<cmd>lua require('dap').continue()<cr>";
      options = { desc = "DAP continue"; };
    }
    {
      mode = "n";
      key = "<leader>di";
      action = "<cmd>lua require('dap').step_into()<cr>";
      options = { desc = "DAP step into"; };
    }
    {
      mode = "n";
      key = "<leader>do";
      action = "<cmd>lua require('dap').step_over()<cr>";
      options = { desc = "DAP step over"; };
    }
    {
      mode = "n";
      key = "<leader>dO";
      action = "<cmd>lua require('dap').step_out()<cr>";
      options = { desc = "DAP step out"; };
    }
    {
      mode = "n";
      key = "<leader>du";
      action = "<cmd>lua require('dapui').toggle()<cr>";
      options = { desc = "DAP UI toggle"; };
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
      cmp = {
        enable = true;
        settings = {
          snippet = {
            expand.__raw = ''
              function(args)
                require("luasnip").lsp_expand(args.body)
              end
            '';
          };
          mapping.__raw = ''
            (function()
              local luasnip = require("luasnip")
              return cmp.mapping.preset.insert({
                ["<C-b>"] = cmp.mapping.scroll_docs(-4),
                ["<C-f>"] = cmp.mapping.scroll_docs(4),
                ["<C-Space>"] = cmp.mapping.complete(),
                ["<C-@>"] = cmp.mapping.complete(),
                ["<C-l>"] = cmp.mapping.complete(),
                ["<C-e>"] = cmp.mapping.abort(),
                ["<CR>"] = cmp.mapping.confirm({ select = true }),
                ["<C-n>"] = cmp.mapping.select_next_item(),
                ["<C-p>"] = cmp.mapping.select_prev_item(),
                ["<Tab>"] = cmp.mapping(function(fallback)
                  if luasnip.expand_or_jumpable() then
                    luasnip.expand_or_jump()
                  elseif cmp.visible() then
                    cmp.select_next_item()
                  else
                    fallback()
                  end
                end, { "i", "s" }),
                ["<S-Tab>"] = cmp.mapping(function(fallback)
                  if cmp.visible() then
                    cmp.select_prev_item()
                  elseif luasnip.jumpable(-1) then
                    luasnip.jump(-1)
                  else
                    fallback()
                  end
                end, { "i", "s" }),
              })
            end)()
          '';
          completion = {
            autocomplete = false;
          };
          sources = [
            { name = "nvim_lsp"; }
            { name = "luasnip"; }
            { name = "path"; }
            { name = "buffer"; }
          ];
        };
      };
      friendly-snippets.enable = true;
      luasnip.enable = true;
      dap.enable = true;
      dap-ui.enable = true;
      jdtls = {
        enable = true;
        settings = {
          cmd = [
            "${pkgs.jdt-language-server}/bin/jdtls"
            "-data"
            {
              __raw = "vim.fn.stdpath('cache') .. '/jdtls-workspaces/' .. vim.fn.fnamemodify(vim.fn.getcwd(), ':p:h:t')";
            }
          ];
          capabilities.__raw = "require('cmp_nvim_lsp').default_capabilities()";
          root_dir.__raw = "require('jdtls.setup').find_root({'.git', 'mvnw', 'gradlew', 'pom.xml', 'build.gradle'})";
          init_options = {
            bundles.__raw = ''
                vim.list_extend(
                vim.split(vim.fn.glob("${pkgs.vscode-extensions.vscjava."vscode-java-debug"}/share/vscode/extensions/vscjava.vscode-java-debug/server/com.microsoft.java.debug.plugin-*.jar"), "\n", { trimempty = true }),
                vim.split(vim.fn.glob("${pkgs.vscode-extensions.vscjava."vscode-java-test"}/share/vscode/extensions/vscjava.vscode-java-test/server/*.jar"), "\n", { trimempty = true })
                )
            '';
          };
        };
      };
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
