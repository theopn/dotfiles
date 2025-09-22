return {
  "lervag/vimtex",
  config = function()
    vim.g.tex_flavor = "latex"

    -- vim.g.vimtex_view_method = "skim"   --> macOS
    vim.g.vimtex_view_method = "zathura" --> Linux

    -- Add --shell-escape flag when external tools are required
    -- e.g., minted package requiring `pygmentize`
    vim.g.vimtex_compiler_latexmk = {
      options = {
        "-shell-escape",
        "-verbose",
        "-file-line-error",
        "-synctex=1",
        "-interaction=nonstopmode"
      },
    }
  end,
  ft = { "plaintex", "tex" },
}
