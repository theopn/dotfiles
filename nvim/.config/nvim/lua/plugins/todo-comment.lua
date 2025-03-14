return {
  "folke/todo-comments.nvim",
  event = "VimEnter",
  dependencies = { "nvim-lua/plenary.nvim" },
  config = function()
    local todo = require("todo-comments")
    todo.setup({
      signs = false,
    })
    vim.keymap.set("n", "]t", function() todo.jump_next() end, { desc = "Next todo comment" })
    vim.keymap.set("n", "[t", function() todo.jump_prev() end, { desc = "Previous todo comment" })
  end
}
