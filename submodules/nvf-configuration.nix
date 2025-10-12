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
    ui.colorizer.enable = true;

    /* END THEME */

    /* KEBINDS */
    clipboard.enable = true;
    clipboard.providers.xclip.enable = true;
    clipboard.providers.wl-copy.enable = true;
    clipboard.registers = "unnamedplus";
    maps.terminal."<Esc>" = {
      action = "<C-\\><C-N>";
      desc = "Make esc leave terminal mode.";
    };

    diagnostics = {
      enable = true;
      config.virtual_text = true;
      nvim-lint.enable = true;
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

      {
        key  = "<leader>r";
        desc = "Toggle Linter";
        mode = "n";
        lua = true;
        action = ''toggle_lint'';
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

    utility.images.image-nvim.enable = true;
    utility.images.image-nvim.setupOpts.integrations.neorg.enable = true;
    utility.images.img-clip.enable = true;
    utility.images.image-nvim.setupOpts.backend = "kitty";
    utility.images.image-nvim.setupOpts.kitty_method = "normal";
    /* NOTES */
    #notes.obsidian.enable = true;
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
      telescope-undo.package = telescope-undo-nvim;
      vim-godot.package = vim-godot;
      obsidian = {
        package = obsidian-nvim;
        setup = ''
vim.o.conceallevel = 2
vim.o.concealcursor = 'nc'
require("obsidian").setup {
  legacy_commands = false,
  workspaces = {
    {
      name = "Master",
      path = "~/Projects/Vault",
    }
  },

  
  
  completion = {
    nvim_cmp = true,
    blink = false,
    min_chars = 2,
    create_new = true,
  },



  -- Either 'wiki' or 'markdown'.
  preferred_link_style = "wiki",

  -- Optional, boolean or a function that takes a filename and returns a boolean.
  -- `true` indicates that you don't want obsidian.nvim to manage frontmatter.
  disable_frontmatter = false,


  -- Sets how you follow URLs
  ---@param url string
  follow_url_func = function(url)
    vim.ui.open(url)
    -- vim.ui.open(url, { cmd = { "firefox" } })
  end,

  -- Sets how you follow images
  ---@param img string
  follow_img_func = function(img)
    vim.ui.open(img)
    -- vim.ui.open(img, { cmd = { "loupe" } })
  end,

  ---@class obsidian.config.OpenOpts
  ---
  ---Opens the file with current line number
  ---@field use_advanced_uri? boolean
  ---
  ---Function to do the opening, default to vim.ui.open
  ---@field func? fun(uri: string)
  open = {
    use_advanced_uri = false,
    func = vim.ui.open,
  },

  picker = {
    -- Set your preferred picker. Can be one of 'telescope.nvim', 'fzf-lua', 'mini.pick' or 'snacks.pick'.
    name = "telescope.nvim",
    -- Optional, configure key mappings for the picker. These are the defaults.
    -- Not all pickers support all mappings.
    note_mappings = {
      -- Create a new note from your query.
      new = "<C-x>",
      -- Insert a link to the selected note.
      insert_link = "<C-l>",
    },
    tag_mappings = {
      -- Add tag(s) to current note.
      tag_note = "<C-x>",
      -- Insert a tag at the current location.
      insert_tag = "<C-l>",
    },
  },

  -- Optional, by default, `:ObsidianBacklinks` parses the header under
  -- the cursor. Setting to `false` will get the backlinks for the current
  -- note instead. Doesn't affect other link behaviour.
  backlinks = {
    parse_headers = true,
  },


  sort_by = "modified",
  sort_reversed = true,

  -- Set the maximum number of lines to read from notes on disk when performing certain searches.
  search_max_lines = 1000,

  -- Optional, determines how certain commands open notes. The valid options are:
  -- 1. "current" (the default) - to always open in the current window
  -- 2. "vsplit" - only open in a vertical split if a vsplit does not exist.
  -- 3. "hsplit" - only open in a horizontal split if a hsplit does not exist.
  -- 4. "vsplit_force" - always open a new vertical split if the file is not in the adjacent vsplit.
  -- 5. "hsplit_force" - always open a new horizontal split if the file is not in the adjacent hsplit.
  open_notes_in = "current",

  ui = {
    enable = true, -- set to false to disable all additional syntax features
    ignore_conceal_warn = false, -- set to true to disable conceallevel specific warning
    update_debounce = 200, -- update delay after a text change (in milliseconds)
    max_file_length = 5000, -- disable UI features for files with more than this many lines
    -- Define how various check-boxes are displayed

    -- Use bullet marks for non-checkbox lists.
    bullets = { char = "•", hl_group = "ObsidianBullet" },
    external_link_icon = { char = "", hl_group = "ObsidianExtLinkIcon" },
    -- Replace the above with this if you don't have a patched font:
    -- external_link_icon = { char = "", hl_group = "ObsidianExtLinkIcon" },
    reference_text = { hl_group = "ObsidianRefText" },
    highlight_text = { hl_group = "ObsidianHighlightText" },
    tags = { hl_group = "ObsidianTag" },
    block_ids = { hl_group = "ObsidianBlockID" },
    hl_groups = {
      -- The options are passed directly to `vim.api.nvim_set_hl()`. See `:help nvim_set_hl`.
      ObsidianTodo = { bold = true, fg = "#f78c6c" },
      ObsidianDone = { bold = true, fg = "#89ddff" },
      ObsidianRightArrow = { bold = true, fg = "#f78c6c" },
      ObsidianTilde = { bold = true, fg = "#ff5370" },
      ObsidianImportant = { bold = true, fg = "#d73128" },
      ObsidianBullet = { bold = true, fg = "#89ddff" },
      ObsidianRefText = { underline = true, fg = "#c792ea" },
      ObsidianExtLinkIcon = { fg = "#c792ea" },
      ObsidianTag = { italic = true, fg = "#89ddff" },
      ObsidianBlockID = { italic = true, fg = "#89ddff" },
      ObsidianHighlightText = { bg = "#75662e" },
    },
  },

  ---@class obsidian.config.AttachmentsOpts
  ---
  ---Default folder to save images to, relative to the vault root.
  ---@field img_folder? string
  ---
  ---Default name for pasted images
  ---@field img_name_func? fun(): string
  ---
  ---Default text to insert for pasted images, for customizing, see: https://github.com/obsidian-nvim/obsidian.nvim/wiki/Images
  ---@field img_text_func? fun(path: obsidian.Path): string
  ---
  ---Whether to confirm the paste or not. Defaults to true.
  ---@field confirm_img_paste? boolean
  attachments = {
    img_folder = "assets/imgs",
    img_name_func = function()
      return string.format("Pasted image %s", os.date "%Y%m%d%H%M%S")
    end,
    confirm_img_paste = true,
  },

  ---@class obsidian.config.FooterOpts
  ---
  ---@field enabled? boolean
  ---@field format? string
  ---@field hl_group? string
  ---@field separator? string|false Set false to disable separator; set an empty string to insert a blank line separator.
  footer = {
    enabled = true,
    format = "{{backlinks}} backlinks  {{properties}} properties  {{words}} words  {{chars}} chars",
    hl_group = "Comment",
    separator = string.rep("-", 80),
  },
  ---@class obsidian.config.CheckboxOpts
  ---
  ---Order of checkbox state chars, e.g. { " ", "x" }
  ---@field order? string[]
  checkbox = {
    order = { " ", "~", "!", ">", "x" },
  },
}
        '';
      };
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
            tree-sitter-godot-resource
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
  vim.luaConfigPre = ''
    linting = true
    local toggle_lint = function()
      linting = not linting
      if linting then
        vim.diagnostic.show()
      else
        vim.diagnostic.hide()
      end
    end
  '';
}
