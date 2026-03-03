return {
  "catgoose/nvim-colorizer.lua",
  opts = {
    options = {
      parsers = {
        css = true,  -- preset: enables names, hex, rgb, hsl, oklch
      },
      display = {
        mode = "virtualtext",
        virtualtext = { position = "after" },
      },
    },
  },
}
