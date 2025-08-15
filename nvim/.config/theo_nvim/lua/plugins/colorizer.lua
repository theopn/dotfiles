return {
  "catgoose/nvim-colorizer.lua",
  config = function() require("colorizer").setup({}) end,   --> `opts` works iff module name == plugin name
}
