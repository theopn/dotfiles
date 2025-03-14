return {
  "folke/which-key.nvim",
  event = "VimEnter",
  config = function()
    require("which-key").setup()
    require("which-key").add({
        { "<leader>c",  group = "[C]ode" },
        { "<leader>d",  group = "[D]ocument" },
        { "<leader>f",  group = "[F]ile" },
        { "<leader>g",  group = "[G]it" },
        { "<leader>r",  group = "[R]ename" },
        { "<leader>s",  group = "[S]earch" },
        { "<leader>w",  group = "[W]orkspace" },
        { "<leader>t",  group = "[T]oggle / [T]erminal" },
      })
  end
}
