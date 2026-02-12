{ inputs, ... }:

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
    };

    colorschemes.tokyonight.enable = true;

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
          remember_settings = false;
          use_per_project_settings = false;
        };
      };
    };
  };
}
