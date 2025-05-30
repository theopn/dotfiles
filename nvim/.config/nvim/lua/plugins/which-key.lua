return {
  "folke/which-key.nvim",
  event = "VimEnter",
  config = function()
    require("which-key").setup()
    require("which-key").add({
      { "<leader>d", group = "[D]irectory" },
      { "<leader>g", group = "[G]it" },
      { "<leader>h", group = "[H]unk" },
      { "<leader>o", group = "[O]rg-mode" },
      { "<leader>s", group = "[S]earch" },
      { "<leader>t", group = "[T]erminal" },
    })
  end
}
