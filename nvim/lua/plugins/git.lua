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
      vim.keymap.set("n", "<leader>gd", require("gitsigns").diffthis,
        { buffer = bufnr, desc = "[G]it [D]iff current buffer" })
    end
  },
}
