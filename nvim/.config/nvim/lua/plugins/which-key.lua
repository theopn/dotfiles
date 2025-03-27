return {
  "folke/which-key.nvim",
  event = "VimEnter",
  config = function()
    require("which-key").setup()
    require("which-key").add({
      { "<leader>g", group = "[G]it" },
      { "<leader>o", group = "[O]rg-mode" },
      { "<leader>s", group = "[S]earch" },
      { "<leader>t", group = "[T]erminal" },
    })
  end
}
