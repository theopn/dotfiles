--- keymaps.lua
---
---      \/       \/
---      /\_______/\
---     /   o   o   \
---    (  ==  ^  ==  )
---     )  [Ollie]  (
---    (             )
---    ( (  )   (  ) )
---   (__(__)___(__)__)
---  ___
---   | |_  _  _     o __
---   | | |(/_(_)\_/ | |||
---
--- LSP configuration using the built-in LSP framework
--- Server configurations are located at the $XDG_CONFIG_HOME/nvim/lsp/ directory

local servers = {
  "bashls",
  "clangd",
  "luals",
  "pylsp",
  "texlab",
}

-- Neovim will call config() for the merged tables in `nvim/lsp/<name>.lua` as well as explicit calls
vim.lsp.config("*", {
  root_markers = { ".git" },
})


-- Configuring keymaps and autocmd for LSP buffers
local lspgroup = vim.api.nvim_create_augroup("TheovimLspAttach", { clear = true, })
local lspattach = function(event)
  -- Create a shortcut for checkhealth
  vim.api.nvim_create_user_command("LspInfo", ":checkhealth vim.lsp", { nargs = 0 })

  -- Helper function for creating LSP keybindings
  local map = function(keys, func, desc)
    vim.keymap.set("n", keys, func, { buffer = event.buf, desc = "[LSP] " .. desc })
  end

  -- settings and mapping sfor the diagnostic framework
  vim.diagnostic.config({
    virtual_text = true,
    virtual_lines = { current_line = true },
    float = {
      border = "rounded",
    },
    underline = true,
    update_in_insert = false,
  })
  map("<leader>q", vim.diagnostic.setloclist, "Open diagnostic [Q]uickfix list")

  -- settings for the LSP framework
  map("K", function()
    vim.lsp.buf.hover({
      border = "rounded",
    })
  end, "LSP Hover with rounded look")
  map("<leader>f", vim.lsp.buf.format, "[F]ormat buffer")

  -- Override default keybinding
  local status, fzf = pcall(require, "fzf-lua")
  if status then
    map("gra", fzf.lsp_code_actions, "[G]oto Code [A]ctions")
    map("grr", fzf.lsp_references, "[G]oto [R]eferences")
    map("gri", fzf.lsp_implementations, "[G]oto [I]mplementation")
    map("gO", fzf.lsp_document_symbols, "[O]pen Document Symbols")
    map("<leader>sd", fzf.diagnostics_document, "[S]earch [D]iagnostics")
  end

  -- Symbol highlights
  -- Creates an autocmd to highlight the symbol under the cursor
  local client = vim.lsp.get_client_by_id(event.data.client_id)
  -- before Neovim 0.11, use client.supports_method(method, { bufnr = bufnr })
  if client and client:supports_method(vim.lsp.protocol.Methods.textDocument_documentHighlight, event.buf) then
    local theovim_lsp_hl_group = vim.api.nvim_create_augroup("TheovimLspHl", { clear = false, })
    vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
      buffer = event.buf,
      group = theovim_lsp_hl_group,
      callback = vim.lsp.buf.document_highlight,
    })

    vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
      buffer = event.buf,
      group = theovim_lsp_hl_group,
      callback = vim.lsp.buf.clear_references,
    })

    vim.api.nvim_create_autocmd("LspDetach", {
      group = vim.api.nvim_create_augroup("TheovimLspHlDetach", { clear = true, }),
      callback = function(event2)
        vim.lsp.buf.clear_references()
        vim.api.nvim_clear_autocmds({ group = "TheovimLspHl", buffer = event2.buf })
      end
    })
  end

  -- Auto-completion
  --vim.lsp.completion.enable(true, event.data.client_id, event.buf, { autotrigger = true })
end

vim.api.nvim_create_autocmd("LspAttach", {
  group = lspgroup,
  callback = lspattach
})


-- Enable the servers!
vim.lsp.enable(servers)
