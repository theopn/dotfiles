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

  local win_opts = {
    border = "shadow", --> sigle, double, rounded, solid, shadow
    relative = "editor",
    style = "minimal", --> No number, cursorline, etc.
    width = width,
    height = height,
    row = y_pos,
    col = x_pos,
  }
  local buf = vim.api.nvim_create_buf(false, true)
  local win = vim.api.nvim_open_win(buf, true, win_opts)

  -- options
  vim.api.nvim_buf_set_option(buf, "bufhidden", "wipe") --> Kill the buffer when hidden
  vim.api.nvim_buf_set_option(buf, "modifiable", false)
  vim.api.nvim_win_set_option(win, "winblend", 50)      --> 80 for transparency

  -- Execute the command
  vim.fn.termopen(cmd)
end

vim.api.nvim_create_user_command("RunLua", run_lua, { nargs = 0 })


-- Add snippets with the custom function in /lua/theovim/completion.lua
-- https://github.com/rafamadriz/friendly-snippets/blob/main/snippets/lua/lua.json
_G.addSnippet({
  prefix = "foreach",
  body = { "for i, ${1:x} in pairs(${2:table}) do", "\t$0", "end" },
})

_G.addSnippet({
  prefix = "for",
  body = { "for ${1:i}=${2:1},${3:10} do", "\t$0", "end" },
})
