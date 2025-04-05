local M = { "hrsh7th/nvim-cmp" }

M.event = { "InsertEnter", "CmdlineEnter", }

M.enabled = true

M.dependencies = {
  {
    "L3MON4D3/LuaSnip", --> VS Code style snippet enginew
    build = (function()
      -- Build jsregexp unless in Windows or make is not available
      if vim.fn.has("win32") == 1 or vim.fn.executable("make") == 0 then
        return
      end
      return "make install_jsregexp"
    end)(),
    dependencies = {
      "rafamadriz/friendly-snippets", --> Snippet collections
    },
  },
  "saadparwaiz1/cmp_luasnip", --> Providing Luasnip as one of nvim-cmp source
  "hrsh7th/cmp-nvim-lsp",     --> nvim-cmp source for LSP engine
  "hrsh7th/cmp-buffer",       --> nvim-cmp source for buffer words
  "hrsh7th/cmp-cmdline",      --> nvim-cmp source for :commands
}

M.config = function()
  local cmp = require("cmp")
  local luasnip = require("luasnip")

  require("luasnip.loaders.from_vscode").lazy_load()
  luasnip.config.setup({})

  cmp.setup({
    snippet = {
      expand = function(args)
        luasnip.lsp_expand(args.body)
      end,
    },

    -- UI customization
    window = {
      completion = cmp.config.window.bordered(),
      documentation = cmp.config.window.bordered(),
    },

    mapping = cmp.mapping.preset.insert({
      -- Select the [n]ext item
      ["<C-n>"] = cmp.mapping.select_next_item(),
      ["<C-j>"] = cmp.mapping.select_next_item(),
      -- Select the [p]revious item
      ["<C-p>"] = cmp.mapping.select_prev_item(),
      ["<C-k>"] = cmp.mapping.select_prev_item(),

      ["<C-e>"] = cmp.mapping.abort(),

      -- Scoll the doc [b]ack / [f]orward
      ["<C-b>"] = cmp.mapping.scroll_docs(-4),
      ["<C-f>"] = cmp.mapping.scroll_docs(4),

      -- Accept ([y]es or <CR>) the completion
      ["<C-y>"] = cmp.mapping.confirm {
        behavior = cmp.ConfirmBehavior.Replace,
        select = false,
      },
      ["<CR>"] = cmp.mapping.confirm {
        behavior = cmp.ConfirmBehavior.Replace,
        select = false,
      },

      -- Manually trigger the completion
      ["<C-Space>"] = cmp.mapping.complete({}),

      -- Moving through the Luasnip expansion
      ["<C-l>"] = cmp.mapping(function()
        if luasnip.expand_or_locally_jumpable() then
          luasnip.expand_or_jump()
        end
      end, { "i", "s" }),
      ["<C-h>"] = cmp.mapping(function()
        if luasnip.locally_jumpable(-1) then
          luasnip.jump(-1)
        end
      end, { "i", "s" }),

      -- Tab completion
      -- Cycle through the completion items if completion menu is visible
      -- Cycle through the Luasnip entries if applicable
      -- Else insert <TAB>
      ["<Tab>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_next_item()
        elseif luasnip.expand_or_locally_jumpable() then
          luasnip.expand_or_jump() --> use tab to jump completed function param
        else
          fallback()
        end
      end, { "i", "s" }),
      ["<S-Tab>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_prev_item()
        elseif luasnip.locally_jumpable(-1) then
          luasnip.jump(-1)
        else
          fallback()
        end
      end, { "i", "s" }),

    }),

    sources = cmp.config.sources(
      {
        { name = "nvim_lsp" },
        { name = "luasnip" },
      },
      {
        { name = "buffer" },
      }
    ),

  })

  -- cmp-cmdline setup
  -- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline({ '/', '?' }, {
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
end

return M
