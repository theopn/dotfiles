return {
  "gbprod/yanky.nvim",
  opts = {
    ring = {
      history_length = 20,
    },
    highlight = {
      on_put = true,
      on_yank = true,
      timer = 150,
    },
  },
  keys = {
    { "<leader>p", "<cmd>YankyRingHistory<cr>",              mode = { "n", "x" },                                desc = "Open Yank History" },
    -- { "y",         "<Plug>(YankyYank)",                      mode = { "n", "x" },                                desc = "Yank text" },
    { "p",         "<Plug>(YankyPutAfter)",                  mode = { "n", "x" },                                desc = "Put yanked text after cursor" },
    { "P",         "<Plug>(YankyPutBefore)",                 mode = { "n", "x" },                                desc = "Put yanked text before cursor" },
    { "gp",        "<Plug>(YankyGPutAfter)",                 mode = { "n", "x" },                                desc = "Put yanked text after selection" },
    { "gP",        "<Plug>(YankyGPutBefore)",                mode = { "n", "x" },                                desc = "Put yanked text before selection" },
    { "<c-p>",     "<Plug>(YankyPreviousEntry)",             desc = "Select previous entry through yank history" },
    { "<c-n>",     "<Plug>(YankyNextEntry)",                 desc = "Select next entry through yank history" },
  },
}

