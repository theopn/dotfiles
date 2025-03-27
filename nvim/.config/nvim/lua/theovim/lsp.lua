local servers = {
  "bashls",
  "clangd",
  "luals",
  "pylsp",
  "texlab",
}

--vim.lsp.config()
local lspgroup = vim.api.nvim_create_augroup("TheovimLspAttach", { clear = true, })

vim.api.nvim_create_autocmd("LspAttach", {
  group = lspgroup,
  callback = function(event)
    local map = function(keys, func, desc)
      vim.keymap.set("n", keys, func, { buffer = event.buf, desc = "LSP: " .. desc })
    end

    map("K", function()
      vim.lsp.buf.hover({
        border = "rounded",
      })
    end, "LSP Hover with rounded look")

    map("<leader>q", vim.diagnostic.setloclist, "Open diagnostic [Q]uickfix list")

    map("<leader>f", vim.lsp.buf.format, "[F]ormat buffer")

    -- Override default keybinding
    local status, fzf = pcall(require, "fzf-lua")
    if status then
      map("gra", fzf.lsp_code_actions, "gr Code [A]ctions")
      map("grr", fzf.lsp_references, "gr [R]eferences")
      map("gri", fzf.lsp_implementations, "gr [I]mplementation")
      map("gO", fzf.lsp_document_symbols, "g Document Symbols")
      map("<leader>sd", fzf.diagnostics_document, "[S]earch [D]iagnostics")
    end
  end,
})

vim.lsp.enable(servers)
