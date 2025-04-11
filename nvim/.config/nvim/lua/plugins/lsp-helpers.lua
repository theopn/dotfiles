return {
  { "williamboman/mason.nvim", config = true, },
  { "j-hui/fidget.nvim",       opts = {}, },
  {
    "folke/lazydev.nvim",
    ft = "lua",
    opts = {
      library = {
        { path = "${3rd}/luv/library", words = { "vim%.uv" } },
      },
    },
  }
}
