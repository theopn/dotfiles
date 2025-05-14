return {
  {
    "folke/tokyonight.nvim",
    lazy = false,
    priority = 1000, --> Higher priority over other plugins
    config = function()
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
    end,
  },
  {
    "rebelot/kanagawa.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      require("kanagawa").setup({
        transparent = vim.g.have_transparent_bg,
      })
      -- vim.cmd.colorscheme("kanagawa-lotus")
    end,
  },
  {
    "navarasu/onedark.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      require("onedark").setup({
        style = "cool",
        transparent = vim.g.have_transparent_bg,
      })
      -- vim.cmd.colorscheme("onedark")
    end,
  },
  {
    "EdenEast/nightfox.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      require("nightfox").setup({
        options = {
          transparent = vim.g.have_transparent_bg,
        },
      })
      vim.cmd.colorscheme("nordfox")
    end,
  },
}
