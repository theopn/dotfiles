return {
  "lervag/vimtex",
  config = function()
    vim.g.tex_flavor = "latex"
    vim.g.vimtex_view_method = "skim"   --> macOS
    --vim.g.vimtex_view_method = "zathura" --> Linux
  end,
  ft = { "plaintex", "tex" },
}
