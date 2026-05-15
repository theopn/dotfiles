vim.pack.add({
  "https://github.com/gbprod/yanky.nvim",
})

require("yanky").setup({
  ring = {
    history_length = 20,
  },
  highlight = {
    on_put = true,
    on_yank = true,
    timer = 150,
  },
})

vim.keymap.set({ "n" }, "<leader>p", "<cmd>YankyRingHistory<cr>", { desc = "Open Yank History" })
vim.keymap.set({ "n", "x" }, "p", "<Plug>(YankyPutAfter)", { desc = "Put yanked text after cursor" })
vim.keymap.set({ "n", "x" }, "P", "<Plug>(YankyPutBefore)", { desc = "Put yanked text before cursor" })
vim.keymap.set({ "n", "x" }, "gp", "<Plug>(YankyGPutAfter)", { desc = "Put yanked text after selection" })
vim.keymap.set({ "n", "x" }, "gP", "<Plug>(YankyGPutBefore)", { desc = "Put yanked text before selection" })
vim.keymap.set({ "n" }, "<C-p>", "<Plug>(YankyPreviousEntry)", { desc = "Select previous entry through yank history" })
vim.keymap.set({ "n" }, "<C-n>", "<Plug>(YankyNextEntry)", { desc = "Select next entry through yank history" })
