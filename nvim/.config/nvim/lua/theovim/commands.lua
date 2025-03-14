-- Theovim user commands

--- Trims trailing whitespace
--- \s: white space char, \+ :one or more, $: end of the line, e: suppresses warning when no match found, c: confirm
local function trim_whitespace()
  local win_save = vim.fn.winsaveview()
  vim.cmd([[keeppatterns %s/\s\+$//ec]])
  vim.fn.winrestview(win_save)
end
vim.api.nvim_create_user_command("TrimWhitespace", trim_whitespace, { nargs = 0 })

-- Changes current working directory
vim.api.nvim_create_user_command("CD", ":lcd %:h", { nargs = 0 })
