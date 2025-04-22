local M = { "hrsh7th/nvim-cmp" }

M.event = { "InsertEnter", "CmdlineEnter", }

M.enabled = true

M.dependencies = {
  "hrsh7th/cmp-nvim-lsp",
  "hrsh7th/cmp-buffer",
  "hrsh7th/cmp-path",
  "hrsh7th/cmp-cmdline",
  "rafamadriz/friendly-snippets",
  {
    "garymjr/nvim-snippets", --> for expanding friendly_snippets
    opts = {
      friendly_snippets = true,
      create_cmp_source = true,
    }
  },
}

M.config = function()
  local cmp = require("cmp")

  cmp.setup({
    snippet = {
      expand = function(args)
        vim.snippet.expand(args.body)
      end,
    },

    -- UI customization
    window = {
      completion = cmp.config.window.bordered(),
      documentation = cmp.config.window.bordered(),
    },

    mapping = cmp.mapping.preset.insert({
      ["<C-n>"] = cmp.mapping.select_next_item(),
      ["<C-p>"] = cmp.mapping.select_prev_item(),
      ["<C-e>"] = cmp.mapping.abort(),

      -- select = false: does nothing if nothing is selected (true would insert the first item)
      --[[ cmp.ConfirmBehavior.Replace:
      ("|" is the cursor)
      ```
                    |uire
                    ╭─────────────────────────────────╮
                    │ require(modname)  Function [LSP]│
                    │ req~           Snippet [LuaSnip]│

                    ... confirming the first item ...

                    require|
      ```
      If `behavior = cmp.confirmBehavior.Insert`, it would insert "requireuire"
      --]]
      ["<C-y>"] = cmp.mapping.confirm {
        select = false,
        behavior = cmp.ConfirmBehavior.Replace,
      },
      ["<CR>"] = function(fallback)
        if cmp.visible() then
          cmp.confirm({
            select = false,
            behavior = cmp.ConfirmBehavior.Replace,
          })
        else
          fallback()
        end
      end,

      -- Scoll the doc [b]ack (down) / [f]orward (up)
      ["<C-b>"] = cmp.mapping.scroll_docs(-4),
      ["<C-f>"] = cmp.mapping.scroll_docs(4),

      -- Manually trigger the completion
      ["<C-Space>"] = cmp.mapping.complete({}),

      -- Tab completion
      -- Cycle through the completion items if completion menu is visible
      -- Cycle through the snippet entries if applicable
      -- Else insert <TAB>
      ["<Tab>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_next_item()
        elseif vim.snippet.active({ direction = 1 }) then
          vim.snippet.jump(1)
        else
          fallback()
        end
      end, { "i", "s" }),
      ["<S-Tab>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_prev_item()
        elseif vim.snippet.active({ direction = -1 }) then
          vim.snippet.jump(-1)
        else
          fallback()
        end
      end, { "i", "s" }),

    }),

    sources = cmp.config.sources({
      { name = "nvim_lsp" },
      { name = "snippets" },
      { name = "buffer" },
      { name = "path" },
    }),
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
      { name = 'cmdline' }
    })
  })
end

return M
