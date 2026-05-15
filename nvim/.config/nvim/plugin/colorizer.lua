vim.pack.add({
  "https://github.com/catgoose/nvim-colorizer.lua",
})

require("colorizer").setup({
  options = {
    parsers = {
      css = true,  -- preset: enables names, hex, rgb, hsl, oklch
    },
    display = {
      mode = "virtualtext",
      virtualtext = { position = "after" },
    },
  },
})
