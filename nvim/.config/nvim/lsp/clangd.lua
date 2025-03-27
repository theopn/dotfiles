---@type vim.lsp.Config
return {
  cmd = { 'clangd' },
  filetypes = { 'c', 'cpp', 'objc', 'objcpp', 'cuda', 'proto' },

  root_markers = { '.clangd',
    '.clang-tidy',
    '.clang-format',
    'compile_commands.json',
    'compile_flags.txt',
    'configure.ac' -- AutoTools
  },
  single_file_support = true,
  capabilities = {
    textDocument = {
      completion = {
        editsNearCursor = true,
      },
    },
    offsetEncoding = { 'utf-8', 'utf-16' },
  },
  commands = {
    ClangdSwitchSourceHeader = {
      function()
        switch_source_header(0)
      end,
      description = 'Switch between source/header',
    },
    ClangdShowSymbolInfo = {
      function()
        symbol_info()
      end,
      description = 'Show symbol info',
    },
  }
}
