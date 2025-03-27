local servers = {
  "bashls",
  "clangd",
  "luals",
}

--vim.lsp.config()
local lspgroup = vim.api.nvim_create_augroup("TheovimLspAttach", { clear = true, })

vim.api.nvim_create_autocmd("LspAttach", {
  group = lspgroup,
  callback = function(event)
    local map = function(keys, func, desc)
      vim.keymap.set("n", keys, func, { buffer = event.buf, desc = "LSP: " .. desc })
    end
    -- Creates a keybinding to format code
    map("<leader>f", vim.lsp.buf.format, "[F]ormat buffer")
  end,
})

vim.lsp.enable(servers)
