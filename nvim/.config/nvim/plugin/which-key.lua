vim.pack.add({
  "https://github.com/folke/which-key.nvim",
})


require("which-key").setup()
require("which-key").add({
  { "<leader>d", group = "[D]irectory" },
  { "<leader>g", group = "[G]it" },
  { "<leader>h", group = "[H]unk" },
  { "<leader>o", group = "[O]rg-mode" },
  { "<leader>s", group = "[S]earch" },
  { "<leader>t", group = "[T]erminal" },
})

vim.keymap.set("n", "<leader>?",
  function()
    require("which-key").show({ global = false })
  end,
  { desc = "Buffer Local Keymaps (which-key)", }
)
