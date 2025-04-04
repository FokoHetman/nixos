{pkgs, lib, inputs, ...}:
{
  
  vim = {
    vimAlias = true;
    viAlias = true;
    #package = inputs.nixvim.packages.${pkgs.system}.default;

    theme = {
      enable = true;
      name = "catppuccin";
      style = "mocha";
    };
    options = {
      tabstop = 2;
      shiftwidth = 2;
      expandtab = true;
    };

    maps.terminal."<Esc>" = {
      action = "<C-\\><C-N>";
      desc = "make esc leave terminal mode";
    };
    
    keymaps = [
      
      {
        key = "<C-j>";
        desc = "Launch terminal in new buffer.";
        mode = "n";
        action = "<cmd>term<CR>";
      }
      {
        key = "<leader>t";
        desc = "Toggle Neotree.";
        mode = "n";
        action = "<cmd>Neotree toggle<CR>";
      }
      {
        key = "<leader>l";
        desc = "Compile Latex.";
        mode = "n";
        action = "<cmd>VimtexCompile<CR>";
      }

      {
        key = "<leader>s";
        desc = "Split screen horizontally.";
        mode = "n";
        action = "<cmd>split<CR>";
      }
      {
        key = "<leader>s";
        desc = "Split screen vertically.";
        mode = "n";
        action = "<cmd>vs<CR>";
      }

      {key="<S-Up>"; action = "<Esc>v<Up>"; mode = ["n"];}
      {key="<S-Down>"; action = "<Esc>v<Down>"; mode = ["n"];}
      {key="<S-Left>"; action = "<Esc>v<Left>"; mode = ["n"];}
      {key="<S-Right>"; action = "<Esc>v<Right>"; mode = ["n"];}

      {action = "<cmd>bnext<CR>"; key="<C-Right>"; mode = "n";}
      {action = "<cmd>bprev<CR>"; key="<C-Left>"; mode = "n";}
    ];


    languages = {
      enableLSP = true;
      enableTreesitter = true;

      markdown.enable = true;
      css.enable = true;

      rust.enable = true;
      haskell.enable = true;
      nix.enable = true;

      assembly.enable = true;
      clang.enable = true;

      lua.enable = true;
      ts.enable = true;
    };

    presence.neocord = {
      enable = true;
      setupOpts = {
        enable_line_number = true;
        blacklist = [".java"];
      };
    };
    filetree.neo-tree.enable = true;
    statusline.lualine.enable = true;
    telescope = {
      enable = true;
      mappings.findFiles = "<C-f>";
      mappings.liveGrep = "<C-g>";
    };
    autocomplete.nvim-cmp.enable = true;
    lsp.enable = true;
    mini.tabline.enable = true;



    startPlugins = [
      pkgs.vimPlugins.vimtex
    ];
    lazy.plugins.vimtex = {
      enabled = true;
      package = pkgs.vimPlugins.vimtex;
      lazy = true; # Changed this
      ft = "tex"; # Added this
      setupOpts = {
        init = ''
          vim.g.vimtex_view_method = "zathura"
          vim.g.vimtexd_view_forward_search_on_start = false
          vim.g.vimtexd_compiler_latexmk = {
            aux_dir = "/home/foko/.texfiles/",
            out_dir = "/home/foko/.texfiles/",
          }
        '';
      };
      # Added this
      after = ''
        vim.api.nvim_command('unlet b:did_ftplugin')
        vim.api.nvim_command('call vimtex#init()')
      '';
    };
  };
}
