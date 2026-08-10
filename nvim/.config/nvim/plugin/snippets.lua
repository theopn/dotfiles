vim.pack.add({
  "https://github.com/garymjr/nvim-snippets",
  "https://github.com/rafamadriz/friendly-snippets",
})

require("snippets").setup({
  friendly_snippets = true,
  create_autocmd = true,     --> preload snippets on FileType
  create_cmp_source = false, --> not using nvim-cmp
})
