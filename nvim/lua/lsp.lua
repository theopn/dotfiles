--[[
 ________              _  __     _         __
/_  __/ /  ___ ___    / |/ /  __(_)_ _    / /  ___ ___
 / / / _ \/ -_) _ \  /    / |/ / /  ' \  / /__(_-</ _ \
/_/ /_//_/\__/\___/ /_/|_/|___/_/_/_/_/ /____/___/ .__/
                                                /_/
--]]

-- List of LSP server used later
local server_list = {
  "bashls", "clangd", "cssls", "html", "ltex", "remark_ls",
}

-- {{{ Basic lspconfig settings
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
-- }}}

-- {{{ nvim-cmp setup
-- Table for icons
local kind_icons = {
  Text = "",
  Method = "",
  Function = "",
  Constructor = "",
  Field = "ﰠ",
  Variable = "",
  Class = "ﴯ",
  Interface = "",
  Module = "",
  Property = "ﰠ",
  Unit = "塞",
  Value = "",
  Enum = "",
  Keyword = "",
  Snippet = "",
  Color = "",
  File = "",
  Reference = "",
  Folder = "",
  EnumMember = "",
  Constant = "",
  Struct = "פּ",
  Event = "",
  Operator = "",
  TypeParameter = ""
}

-- Helper function for TAB completion
local check_backspace = function()
  local col = vim.fn.col "." - 1
  return col == 0 or vim.fn.getline("."):sub(col, col):match("%s")
end

local cmp = require "cmp"

cmp.setup({
  snippet = {
    -- REQUIRED - you must specify a snippet engine
    expand = function(args)
      require("luasnip").lsp_expand(args.body) -- For `luasnip` users.
    end,
  },
  window = {
    -- completion = cmp.config.window.bordered(),
    -- documentation = cmp.config.window.bordered(),
  },
  mapping = cmp.mapping.preset.insert({
    ["<C-j>"] = cmp.mapping.select_next_item(), --> <C-n>
    ["<C-k>"] = cmp.mapping.select_prev_item(), --> <C-p>
    ["<C-e>"] = cmp.mapping.abort(), --> Close the completion window
    ["<C-[>"] = cmp.mapping.scroll_docs(-4), --> Scroll through the information window next to the item
    ["<C-]>"] = cmp.mapping.scroll_docs(4), --> ^
    ["<C-Space>"] = cmp.mapping.complete(), --> Brings up completion window without
    ["<CR>"] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    ["<Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item({ behavior = cmp.SelectBehavior.Select }) --> <C-n> if completion window is open
      elseif check_backspace() then
        fallback() --> Default action (tab char or shiftwidth) if the line is empty
      else
        cmp.complete() --> Open completion window. Change it to fallback() if you want to insert tab in between lines
      end
    end, { 'i', 's' }),
    ["<S-Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
      else
        fallback()
      end
    end, { 'i', 's' }),
  }),
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'luasnip' },
  }, {
    { name = 'buffer' },
    { name = 'path' },
  }),
  formatting = {
    format = function(entry, vim_item)
      -- Kind icons
      vim_item.kind = string.format('%s %s', kind_icons[vim_item.kind], vim_item.kind)
      -- Source
      vim_item.menu = ({
        buffer = "[Buffer]",
        nvim_lsp = "[LSP]",
        luasnip = "[LuaSnip]",
        nvim_lua = "[Lua]",
        latex_symbols = "[LaTeX]",
      })[entry.source.name]
      return vim_item
    end
  },
})

-- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline('/', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = {
    { name = 'buffer' }
  }
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(':', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources({
    { name = 'path' }
  }, {
    { name = 'cmdline' }
  })
})

-- Setup lspconfig.
local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
-- }}}

-- {{{ Language Server Settings
for _, v in ipairs(server_list) do
  nvim_lsp[v].setup { capabilities = capabilities, on_attach = on_attach, }
end

nvim_lsp.sumneko_lua.setup {
  capabilities = capabilities,
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
  },
}
-- }}}

-- {{{ Trouble Settings
require("trouble").setup {
  mode = "document_diagnostics",
}
-- }}}

-- {{{ lspsaga Settings
local saga = require("lspsaga")
saga.init_lsp_saga()
-- }
