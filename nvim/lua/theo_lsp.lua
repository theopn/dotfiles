--[[
 ________              _  __     _         __
/_  __/ /  ___ ___    / |/ /  __(_)_ _    / /  ___ ___
 / / / _ \/ -_) _ \  /    / |/ / /  ' \  / /__(_-</ _ \
/_/ /_//_/\__/\___/ /_/|_/|___/_/_/_/_/ /____/___/ .__/
                                                /_/
--]]

local status, nvim_lsp = pcall(require, "lspconfig")
if (not status) then return end

local protocol = require("vim.lsp.protocol")

local on_attach = function(client, bufnr)
  if client.server_capabilities.documentFormattingProvider then
    vim.api.nvim_create_autocmd("BufWritePre", {
      group = vim.api.nvim_create_augroup("Format", { clear = true }),
      buffer = bufnr,
      callback = function() vim.lsp.buf.formatting_seq_sync() end
    })
  end
end

nvim_lsp.cmake.setup {}

nvim_lsp.sumneko_lua.setup {
  on_attach = on_attach,
  settings = {
    Lua = {
      diagnostircs = {
        globals = { "vim" }
      },
      workspace = {
        library = vim.api.nvim_get_runtime_file("", true)
      },
    },
  }
}


require("mason").setup()
