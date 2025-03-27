---@type vim.lsp.Config
return {
  cmd = { "texlab" },
  filetypes = { "tex", "plaintex", "bib" },
  --root_dir = { ".git", ".latexmkrc", ".texlabroot", "texlabroot", "Tectonic.toml" },
  single_file_support = true,
  --settings = {
  --  texlab = {
  --    rootDirectory = nil,
  --    build = {
  --      executable = "latexmk",
  --      args = { "-pdf", "-interaction=nonstopmode", "-synctex=1", "%f" },
  --      onSave = false,
  --      forwardSearchAfter = false,
  --    },
  --    forwardSearch = {
  --      executable = nil,
  --      args = {},
  --    },
  --    chktex = {
  --      onOpenAndSave = false,
  --      onEdit = false,
  --    },
  --    diagnosticsDelay = 300,
  --    latexFormatter = "latexindent",
  --    latexindent = {
  --      ["local"] = nil,   -- local is a reserved keyword
  --      modifyLineBreaks = false,
  --    },
  --    bibtexFormatter = "texlab",
  --    formatterLineLength = 80,
  --  },
  --},
}
