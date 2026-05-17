vim.pack.add({
  "https://github.com/stevearc/oil.nvim",
})

local oil = require("oil")

oil.setup({
  delete_to_trash = true,
  view_options = {
    show_hidden = true,
  },
  win_options = {
    winbar = nil,
  },
})

vim.keymap.set("n", "<leader>n", function()
  oil.open_float(nil, { preview = {} })
end, { desc = "Toggle [N]etrw... <<<<< Oil.nvim" })
