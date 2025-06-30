-- Vim filetype plugin file
--
-- It is in Lua because Lua!
--
-- Language:	Lua
-- Maintainer:	Theo P.
-- Last Change:	2025-04-13

local indent = 2

vim.opt_local.expandtab = true
vim.opt_local.tabstop = indent
vim.opt_local.softtabstop = indent
vim.opt_local.shiftwidth = indent
vim.opt_local.colorcolumn = "120"
vim.opt_local.textwidth = 119

--- Run Lua command with
local function run_lua()
  local cmd = "lua " .. vim.api.nvim_buf_get_name(0)

  local height = math.ceil(vim.api.nvim_win_get_height(0) * 0.5)
  local width = math.ceil(vim.api.nvim_win_get_width(0) * 0.5)
  local x_pos = math.ceil((vim.api.nvim_win_get_width(0) - width) * 0.5) --> Center
  local y_pos = 1                                                        --> Top

  local win_config = {
    border = "shadow", --> sigle, double, rounded, solid, shadow
    relative = "editor",
    style = "minimal", --> No number, cursorline, etc.
    width = width,
    height = height,
    row = y_pos,
    col = x_pos,
  }
  local buf = vim.api.nvim_create_buf(false, true)
  local win = vim.api.nvim_open_win(buf, true, win_config)

  -- scratch buffers are "hide" by default,
  vim.api.nvim_set_option_value("bufhidden", "wipe", { buf = buf })
  vim.api.nvim_set_option_value("winblend", 10, { win = win })

  -- Execute the command
  vim.fn.jobstart(cmd, { term = true })
end

vim.api.nvim_create_user_command("RunLua", run_lua, { nargs = 0 })
