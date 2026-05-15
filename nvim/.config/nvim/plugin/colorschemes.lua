vim.pack.add({
  "https://github.com/folke/tokyonight.nvim",
  "https://github.com/rebelot/kanagawa.nvim",
  "https://github.com/navarasu/onedark.nvim",
  "https://github.com/EdenEast/nightfox.nvim",
})

require("tokyonight").setup({
  transparent = vim.g.have_transparent_bg,
  styles = {
    sidebars = vim.g.have_transparent_bg and "transparent" or "dark",
    floats = vim.g.have_transparent_bg and "transparent" or "dark",
  },
  plugins = {
    mini_statusline = true,
    mini_starter = true,
  },
})
-- vim.cmd.colorscheme("tokyonight-night")


require("kanagawa").setup({
  transparent = vim.g.have_transparent_bg,
})
-- vim.cmd.colorscheme("kanagawa-lotus")

require("onedark").setup({
  style = "cool",
  transparent = vim.g.have_transparent_bg,
})
--vim.cmd.colorscheme("onedark")

require("nightfox").setup({
  options = {
    transparent = vim.g.have_transparent_bg,
  },
})
vim.cmd.colorscheme("nordfox")
