--[[
 ________                       _              __          _
/_  __/ /  ___ ___    ___ _  __(_)_ _    ___  / /_ _____ _(_)__
 / / / _ \/ -_) _ \  / _ \ |/ / /  ' \  / _ \/ / // / _ `/ / _ \
/_/ /_//_/\__/\___/ /_//_/___/_/_/_/_/ / .__/_/\_,_/\_, /_/_//_/
                                      /_/          /___/
--]]

---[[ Packer installation
local install_path = vim.fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  vim.fn.execute("!git clone https://github.com/wbthomason/packer.nvim " .. install_path)
end

local packer_group = vim.api.nvim_create_augroup("Packer", { clear = true })
vim.api.nvim_create_autocmd("BufWritePost", { command = "source <afile> | PackerCompile",
                            group = packer_group, pattern = "init.lua" })
--]]


---[[ Packer Plugins
require("packer").startup(function(use)
  use "wbthomason/packer.nvim" --> Irony of having to import itself
  use "navarasu/onedark.nvim" --> Pretty theme
  use "nvim-lualine/lualine.nvim" --> Statusline plugin
  use "nvim-treesitter/nvim-treesitter" --> Highlighting focusing on one file
  use "kyazdani42/nvim-tree.lua" --> NvimTree
  use {
    "ms-jpq/coq_nvim", --> Complention program
    branch = "coq",
    event = "VimEnter", config = "vim.cmd[[COQnow]]", --> Autoexecute COQnow on startup
  }
  use "neovim/nvim-lspconfig" --> Neovim defult LSP engine
  use { "ms-jpq/coq.artifacts", branch = "artifacts" } --> Used by COQ
  use "nvim-lua/plenary.nvim" --> telescope dependency
  use {
    "nvim-telescope/telescope.nvim", --> Expendable fuzzy finder
    requires = { {"nvim-lua/plenary.nvim"} }
  }
  use "romgrk/barbar.nvim" --> Simple tabline plug in
end)
--]]

---[[ Theme Settings
require("onedark").setup {
  style = "deep",
  transparent = false,
  toggle_style_key = "<leader>od",
  toggle_style_list = {"dark", "darker", "cool", "deep", "warm", "warmer"},
  code_style = {
    comments = "italic",
    keywords = "bold",
    functions = 'none',
    strings = 'none',
    variables = 'none'
  },
}
require("onedark").load()
--]]

---[[ Lualine Settings
require("lualine").setup {
  options = {
    theme = "dracula"
  },
}
--]]

---[[ NvimTree Settings
require("nvim-tree").setup {
  auto_reload_on_write = true,
  -- auto_close = true, --> Auto close has been deprecated
  open_on_setup = true, --> Auto open when no files opened
  open_on_setup_file = true, --> Auto open when files opened
  open_on_tab = true,
  sort_by = "name",
  view = {
    width = 30,
    height = 30,
    hide_root_folder = false,
    side = "left",
    preserve_window_proportions = false,
    number = false,
    relativenumber = false,
    signcolumn = "yes",
  }
}
vim.api.nvim_set_keymap('n', "<C-n>", ":NvimTreeToggle<CR>", { noremap = true })
vim.api.nvim_create_autocmd('BufEnter', {
  command = "if winnr('$') == 1 && bufname() == 'NvimTree_' . tabpagenr() | quit | endif",
  nested = true,
})
--]]

---[[ nvim-cmp Settings
--]]

---[[ Barbar Settings
local map = vim.api.nvim_set_keymap
local opts = { noremap = false }
map('n', "<C-,>", ":BufferPrevious<CR>", opts)
map('n', "<C-.>", ":BufferNext<CR>", opts)
vim.g.bufferline = {
  icons = false,
  maximum_padding = 1,
  maximum_length = 30,
  icon_separator_active = '▎',
  icon_separator_inactive = '▎',
  icon_close_tab = '',
  icon_close_tab_modified = '●',
  icon_pinned = '車',
  no_name_title = "New Tab"
}
-- Compitability w/ nvim-tree --
require("nvim-tree.events").on_tree_open(function ()
  require("bufferline.state").set_offset(31, "File Tree")
end)
require("nvim-tree.events").on_tree_close(function ()
  require("bufferline.state").set_offset(0)
end)
--]]

