--[[
 ________                       _              __          _
/_  __/ /  ___ ___    ___ _  __(_)_ _    ___  / /_ _____ _(_)__
 / / / _ \/ -_) _ \  / _ \ |/ / /  ' \  / _ \/ / // / _ `/ / _ \
/_/ /_//_/\__/\___/ /_//_/___/_/_/_/_/ / .__/_/\_,_/\_, /_/_//_/
                                      /_/          /___/
--]]

--- {{{ Packer installation
local install_path = vim.fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  vim.fn.execute("!git clone https://github.com/wbthomason/packer.nvim " .. install_path)
end

local packer_group = vim.api.nvim_create_augroup("Packer", { clear = true })
vim.api.nvim_create_autocmd("BufWritePost", { command = "source <afile> | PackerCompile",
  group = packer_group, pattern = "init.lua" })
-- }}}

-- {{{ Packer Plugins
require("packer").startup(function(use)
  -- {{{ Dependencies
  use "wbthomason/packer.nvim" --> Irony of having to import itself
  use "nvim-lua/plenary.nvim" --> Lua function library for Neovim
  use "kyazdani42/nvim-web-devicons" --> Icons for barbar, Telescope, and more
  -- }}}

  -- {{{ Appearance
  use "navarasu/onedark.nvim" --> Pretty theme
  use "nvim-lualine/lualine.nvim" --> Status line plugin
  use "romgrk/barbar.nvim" --> Tab bar plugin
  use "glepnir/dashboard-nvim" --> Startup dashboard
  use {
    "folke/zen-mode.nvim", --> Centered view for focused text editing
    config = function() require("zen-mode").setup() end
  }
  use "rcarriga/nvim-notify" --> Prettier notification
  -- }}}

  -- {{{ File et Search
  use {
    "lewis6991/gitsigns.nvim", --> Git information
    config = function() require("gitsigns").setup() end
  }
  use {
    "lukas-reineke/indent-blankline.nvim", --> Indentation guide
    config = function() require("indent_blankline").setup() end
  }
  use "nvim-treesitter/nvim-treesitter" --> Incremental highlighting
  use "p00f/nvim-ts-rainbow" --> Rainbow color matching for parentheses
  use "kyazdani42/nvim-tree.lua" --> File tree
  use "nvim-telescope/telescope.nvim" --> Expendable fuzzy finder
  use "nvim-telescope/telescope-file-browser.nvim" --> File browser extension for Telescope
  use "folke/which-key.nvim" --> Pop-up dictionary for keybindings
  use {
    "ellisonleao/glow.nvim", --> Markdown file preview. Requires glow installed
    ft = { "markdown" },
  }
  use({
    "iamcco/markdown-preview.nvim", --> MarkdownPreview to toggle
    run = function() vim.fn["mkdp#util#install"]() end, --> Binary installation for markdown-preview
  })
  -- }}}

  -- {{{ LSP
  use "neovim/nvim-lspconfig" --> Neovim defult LSP engine
  use {
    "williamboman/mason.nvim", --> LSP Manager
    config = function() require("mason").setup() end
  }
  use "theopn/friendly-snippets" --> VS Code style snippet collection
  use {
    "L3MON4D3/LuaSnip", --> Snippet engine that accepts VS Code style snippets
    config = function() require("luasnip.loaders.from_vscode").lazy_load() end --> Load snippets from friendly snippets
  }
  use "saadparwaiz1/cmp_luasnip" --> nvim_cmp and LuaSnip bridge
  use "hrsh7th/cmp-nvim-lsp" --> nvim-cmp and lspconfig bridge
  use "hrsh7th/cmp-buffer" --> nvim-cmp source for buffer words
  use "hrsh7th/cmp-path" --> nvim-cmp source for file path
  use "hrsh7th/cmp-cmdline" --> nvim-cmp source for vim commands
  use "hrsh7th/nvim-cmp" --> Completion Engine
  use "folke/trouble.nvim" --> Pretty list of LSP error list
  use {
    "glepnir/lspsaga.nvim", --> LSP hover menu, code action, rename, etc
    branch = "main",
    config = function()
        require('lspsaga').setup({})
    end,
  }
  -- }}}

end)
-- }}}
