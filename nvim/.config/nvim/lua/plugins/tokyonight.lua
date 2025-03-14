local is_transparent = true --> To disable transparency, set this to false

return {
  "folke/tokyonight.nvim",
  lazy = false,
  priority = 1000, --> Higher priority over other plugins
  config = function()
    require("tokyonight").setup({
      transparent = is_transparent,
      styles = {
        sidebars = is_transparent and "transparent" or "dark",
        floats = is_transparent and "transparent" or "dark",
      },
      plugins = {
        mini_statusline = true,
        mini_starter = true,
      },
    })
    vim.cmd.colorscheme("tokyonight-night")
  end,
}
