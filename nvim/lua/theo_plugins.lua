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
  ---[[ Appearance plugins
  use "navarasu/onedark.nvim" --> Pretty theme
  use "nvim-lualine/lualine.nvim" --> Status line plugin
  use { --> Tab bar plugin
    "romgrk/barbar.nvim",
    requires = { "kyazdani42/nvim-web-devicons" }
  }
  use "MeF0504/vim-pets" --> Cats.
  --]]
  ---[[ Text Edit plugins
  use "neovim/nvim-lspconfig" --> Neovim defult LSP engine
  use "nvim-treesitter/nvim-treesitter" --> Highlighting focusing on one file
  use { --> Complention program
    "ms-jpq/coq_nvim",
    branch = "coq",
    event = "VimEnter", config = "vim.cmd[[COQnow -s]]", --> Autoexecute COQnow on startup
  }
  use { "ms-jpq/coq.artifacts", branch = "artifacts" } --> Used by COQ
  --]]
  ---[[ File and search
  use "kyazdani42/nvim-tree.lua" --> NvimTree
  use "nvim-lua/plenary.nvim" --> telescope dependency
  use { --> Expendable fuzzy finder
    "nvim-telescope/telescope.nvim",
    requires = { {"nvim-lua/plenary.nvim"} }
  }
  --]]
  use({
    "iamcco/markdown-preview.nvim",
    run = function() vim.fn["mkdp#util#install"]() end,
  })
end)
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
--[[ Disabling NvimTree icons for no nerd fonts
vim.g.nvim_tree_show_icons = {
  git = 0,
  folders = 0,
  files = 0,
  folder_arrows = 0,
}
--]]
--]]

---[[ Markdown reader setting
vim.g["mkdp_browser"] = "usr/bin/firefox"
--]]

