-- You can add your own plugins here or in other files in this directory!
--  I promise not to create any merge conflicts in this directory :)
--
-- See the kickstart.nvim README for more information
-- return {}

return {
  {
    'folke/noice.nvim',
    event = 'VeryLazy',
    opts = {
      -- add any options here
      presets = {
        command_palette = true,
      },
      routes = {
        {
          filter = {
            event = 'notify',
            find = 'No information available',
          },
          opts = { skip = true },
        },
        {
          filter = {
            event = 'msg_show',
            find = '^$',
          },
          opts = { skip = true },
        },
      },
    },
    dependencies = {
      -- if you lazy-load any plugin below, make sure to add proper `module="..."` entries
      'MunifTanjim/nui.nvim',
      -- OPTIONAL:
      --   `nvim-notify` is only needed, if you want to use the notification view.
      --   If not available, we use `mini` as the fallback
      'rcarriga/nvim-notify',
    },
  },
  {
    'smjonas/inc-rename.nvim',
    cmd = 'IncRename',
    keys = {
      {
        '<leader>rn',
        function()
          return ':IncRename ' .. vim.fn.expand '<cword>'
        end,
        desc = 'Incremental rename',
        mode = 'n',
        noremap = true,
        expr = true,
      },
    },
    config = true,
  },
  {
    'nvimtools/none-ls.nvim',
    config = function()
      local null_ls = require 'null-ls'
      null_ls.setup {
        sources = {
          null_ls.builtins.formatting.prettier.with {
            filetypes = { 'javascript', 'javascriptreact', 'typescript', 'typescriptreact', 'json', 'markdown', 'yaml' },
            -- command = 'node_modules/.bin/prettier',
            -- extra_args = { '--single-quote', '--jsx-single-quote' },
          },
          null_ls.builtins.formatting.biome.with {
            args = {
              'check',
              '--apply-unsafe',
              '--formatter-enabled=true',
              '--organize-imports-enabled=true',
              '--skip-errors',
              '$FILENAME',
            },
          },
          null_ls.builtins.formatting.black.with {
            extra_args = { '--line-length=120', '--skip-string-normalization' },
          },
          null_ls.builtins.formatting.isort,
          -- null_ls.builtins.diagnostics.eslint_d,
          -- null_ls.builtins.code_actions.eslint_d,
          null_ls.builtins.formatting.stylua.with {
            filetypes = { 'lua' },
          },
          -- null_ls.builtins.diagnostics.shellcheck,
          null_ls.builtins.formatting.goimports,
        },
      }
    end,
  },
  {
    'Pocco81/auto-save.nvim',
    event = 'VeryLazy',
    config = function()
      require('auto-save').setup {
        enabled = true, -- start auto-save when the plugin is loaded (i.e. on `require('auto-save')`)
        execution_message = {
          message = function()
            return 'AutoSave: saved at ' .. vim.fn.strftime '%H:%M:%S'
          end,
          dim = 0.18,
          cleaning_interval = 200,
        },
        debounce_delay = 10000,
        trigger_events = { 'InsertLeave', 'TextChanged' },
        write_all_buffers = false,
      }
      vim.keymap.set({ 'n', 'i' }, '<C-s>', '<Esc>:w<CR>', { desc = 'Save buffer' })
      vim.keymap.set('n', '<leader>ta', ':ASToggle<CR>', { desc = 'Toggle AutoSave' })
    end,
  },
}
