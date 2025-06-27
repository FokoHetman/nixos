{pkgs, lib, inputs, ...}:
{
  
  vim = {
    vimAlias = true;
    viAlias = true;
    globals = {
      mapleader = " ";
      maplocalleader = ".";
    };
    #package = inputs.nixvim.packages.${pkgs.system}.default;

    /* THEME */

    theme = {
      enable = true;
      name = "catppuccin";
      style = "mocha";
    };

    /* END THEME */

    /* KEBINDS */
    maps.terminal."<Esc>" = {
      action = "<C-\\><C-N>";
      desc = "Make esc leave terminal mode.";
    };
    
    keymaps = [
      /*{
        key = "<k4>";
        desc = "TmuxNavigateLeft";
        mode = "n";
        action = "<C-U>TmuxNavigateLeft<CR>";
      }
      {
        key = "<k5>";
        desc = "TmuxNavigateDown";
        mode = "n";
        action = "<C-U>TmuxNavigateLeft<CR>";
      }
      {
        key = "<k6>";
        desc = "TmuxNavigateRight";
        mode = "n";
        action = "<C-U>TmuxNavigateLeft<CR>";
      }
      {
        key = "<k8>";
        desc = "TmuxNavigateUp";
        mode = "n";
        action = "<C-U>TmuxNavigateLeft<CR>";
      }*/


      {
        key = "<C-j>";
        unique = true;
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
        key = "<leader>w";
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
    /* END KEYBINDS */

    /* LANGUAGE */
    languages = {
      enableLSP = true;
      enableTreesitter = true;

      markdown.enable = true;
      css.enable = true;

      rust.enable = true;
      haskell.enable = true;
      nix.enable = true;
      zig.enable = true;

      assembly.enable = true;
      clang.enable = true;

      lua.enable = true;
      ts.enable = true;
    };
    /* END LANGUAGE */


    /* OPTIONS */
    options = {
      tabstop = 2;
      shiftwidth = 2;
      expandtab = true;
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
    
    /* moved to extraPlugins */
    /*mini.starter = let 
      headerFile = "../assets/header.txt";
    in {
      enable = true;
      setupOpts = {
        #inherit header;
        #footer = "                                                                                            ";
      };
    };*/
    /* END OPTIONS */


    /* NOTES */
    notes.neorg = {
      enable = true;
      treesitter.enable = true;
      treesitter.norgPackage = pkgs.tree-sitter-grammars.tree-sitter-norg;
      setupOpts = {
        load = {
          "core.defaults".enable = true;
          "core.completion".config.engine = "nvim-cmp";
          "core.concealer".config = {
            folds = true;
            init_open_folds = "auto";
          };
          "core.dirman".config = {
            workspaces = {
              notes = "~/Projects/Notes";
              #blog = "~/projects/blog";
              #dotfiles = "~/projects/dotfiles";
            };
            index = "index.norg";
          };
          "core.esupports.metagen" = {
            timezone = "local";
            type = "auto";
            update_date = true;
          };
          "core.export".config.export_dir = "${builtins.getEnv "PWD"}";
          "core.export.markdown".config.extensions = "all";
          # "core.integrations.image".enable = false;
          # "core.latex.renderer" = {
          #   enable = false;
          #   config = {
          #     conceal = true;
          #     render_on_enter = true;
          #     renderer = "core.integrations.image";
          #   };
          # };
          "core.presenter".config.zen_mode = "zen-mode";
          "core.summary" = {};
        };
      };
    };

    /* additional plugins */
    extraPlugins = with pkgs.vimPlugins; {
      nabla.package = nabla-nvim;
      nvim-treesitter.package = (nvim-treesitter.withPlugins (
          plugins: with pkgs.tree-sitter-grammars; [
            tree-sitter-c
            tree-sitter-cpp
            tree-sitter-html
            tree-sitter-latex
            tree-sitter-lua
            tree-sitter-nix
            tree-sitter-python
            tree-sitter-rust
            tree-sitter-toml
          ]
        ));
      mini-starter = let 
        headerFile = ../assets/header.txt;
      in {
        package = mini-starter;
        setup = ''
local template = ""
local file = io.open("${headerFile}")
if file then
  template = file:read("*all") or ""
  file:close()
end
local glyph_line = ""

local file = io.open("/tmp/glyphs.txt", "r")
if file then
  glyph_line = file:read("*l") or ""
  file:close()
end

-- Manually split UTF-8 characters
local glyphs = {}
for char in glyph_line:gmatch("[\194-\244][\128-\191]*") do
  table.insert(glyphs, char)
end

local glyph_index = 1
local output_line = template:gsub("A", function()
  local char = glyphs[glyph_index]
  glyph_index = glyph_index + 1
  return char or " "
end)

require("mini.starter").setup({
  header = output_line,
  footer = "                                                                                            ",
})
        '';
      };
    };

    startPlugins = with pkgs.vimPlugins; [
      vimtex
      haskell-tools-nvim
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
