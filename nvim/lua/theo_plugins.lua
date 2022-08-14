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
  use "wbthomason/packer.nvim" --> Irony of having to import itself

  -- {{{
  use "nvim-treesitter/nvim-treesitter" --> Incremental highlighting
  use "p00f/nvim-ts-rainbow"
  -- }}}

  -- {{{  Appearance plugins
  use "navarasu/onedark.nvim" --> Pretty theme
  use "nvim-lualine/lualine.nvim" --> Status line plugin
  use "romgrk/barbar.nvim" --> Tab bar plugin
  use "kyazdani42/nvim-web-devicons" --> Icons for barbar, Telescope, and more
  use "glepnir/dashboard-nvim" --> Startup dashboard
  use "rcarriga/nvim-notify" --> Prettier notification
  use "MeF0504/vim-pets" --> Cats
  -- }}}

  -- {{{ File et Search
  use "kyazdani42/nvim-tree.lua" --> File tree
  use {
    "nvim-telescope/telescope.nvim", --> Expendable fuzzy finder
    requires = { "nvim-lua/plenary.nvim" } --> nvim-telescope Dependency
  }
  use "nvim-telescope/telescope-file-browser.nvim" --> File browser extension for Telescope
  -- }}}

  -- {{{ LSP plugins
  use "neovim/nvim-lspconfig" --> Neovim defult LSP engine
  use "williamboman/mason.nvim" --> LSP Manager
  use "onsails/lspkind.nvim" --> VS Code like pictograms for lsp completion
  use "L3MON4D3/LuaSnip" --> Snippet engine that accepts VS Code style snippets
  use "theopn/friendly-snippets" --> VS Code style snippet collection
  use "saadparwaiz1/cmp_luasnip" --> nvim_cmp and LuaSnip bridge
  use "hrsh7th/cmp-nvim-lsp" --> nvim-cmp and lspconfig bridge
  use "hrsh7th/cmp-buffer" --> nvim-cmp source for buffer words
  use "hrsh7th/cmp-path" --> nvim-cmp source for file path
  use "hrsh7th/cmp-cmdline" --> nvim-cmp source for vim commands
  use "hrsh7th/nvim-cmp" -- Completion Engine
  use "glepnir/lspsaga.nvim" --> LSP hover menu and much more
  -- }}}

  -- {{{ Note Taking
  use({
    "iamcco/markdown-preview.nvim", --> MarkdownPreview to toggle
    run = function() vim.fn["mkdp#util#install"]() end, --> Binary installation for markdown-preview
  })
  use "fadein/vim-figlet" --> ASCII art generator. Requires figlet installed
  -- }}}
end)
-- }}}
