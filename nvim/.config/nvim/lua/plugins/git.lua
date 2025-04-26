return {
  "lewis6991/gitsigns.nvim",
  opts = {
    signs = {
      add = { text = "+" },
      change = { text = "~" },
      delete = { text = "_" },
      topdelete = { text = "‾" },
      changedelete = { text = "~" },
    },

    on_attach = function(bufnr)
      local gitsigns = require("gitsigns")

      local function map(mode, l, r, opts)
        opts = opts or { desc = "" }
        opts.buffer = bufnr
        opts.desc = "[G]it " .. opts.desc
        vim.keymap.set(mode, l, r, opts)
      end

      -- Navigation
      map('n', ']c', function()
        if vim.wo.diff then
          vim.cmd.normal({ ']c', bang = true })
        else
          gitsigns.nav_hunk('next')
        end
      end, { desc = "Next hunk" })

      map('n', '[c', function()
        if vim.wo.diff then
          vim.cmd.normal({ '[c', bang = true })
        else
          gitsigns.nav_hunk('prev')
        end
      end, { desc = "Previous hunk" })

      -- Actions
      map('n', '<leader>gd', gitsigns.diffthis, { desc = "[D]iff current buffer" })

      map('n', '<leader>gb', function()
        gitsigns.blame_line({ full = true })
      end, { desc = "[B]lame" })
    end
  },
}
