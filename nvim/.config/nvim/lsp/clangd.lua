---@type vim.lsp.Config
return {
  cmd = { "clangd" },
  -- https://clangd.llvm.org/troubleshooting#cant-find-standard-library-headers-map-stdioh-etc
  -- If your system does not detect stadard library, set --query-driver flag for your compiler
  --cmd = { "clangd", "--query-driver=/usr/bin/c++" },
  filetypes = { "c", "cpp", "objc", "objcpp", "cuda", "proto" },

  root_markers = { ".clangd",
    ".clang-tidy",
    ".clang-format",
    "compile_commands.json",
    "compile_flags.txt",
    "configure.ac" -- AutoTools
  },
  single_file_support = true,
  capabilities = {
    textDocument = {
      completion = {
        editsNearCursor = true,
      },
    },
    offsetEncoding = { "utf-8", "utf-16" },
  },
}
