-- Theovim user commands

local cmd = vim.api.nvim_create_user_command

--- Trims trailing whitespace
--- \s: white space char, \+ :one or more, $: end of the line, e: suppresses warning when no match found, c: confirm
local function trim_whitespace()
  local win_save = vim.fn.winsaveview()
  vim.cmd([[keeppatterns %s/\s\+$//ec]])
  vim.fn.winrestview(win_save)
end
cmd("TrimWhitespace", trim_whitespace, { nargs = 0 })

-- Changes current working directory
cmd("CD", ":lcd %:h", { nargs = 0 })

-- Sets the buffer to readonly and not modifiable state
cmd("RO", ":setlocal readonly nomodifiable", { nargs = 0 })
