--[[
 ________              _  __     _         _____
/_  __/ /  ___ ___    / |/ /  __(_)_ _    / ___/______ __  _____
 / / / _ \/ -_) _ \  /    / |/ / /  ' \  / (_ / __/ _ `/ |/ / -_)
/_/ /_//_/\__/\___/ /_/|_/|___/_/_/_/_/  \___/_/  \_,_/|___/\__/

--]]

--[[ Status Bar Related Settings
-- https://nuxsh.is-a.dev/blog/custom-nvim-statusline.html --
local modes = {
  ["n"] = "NORMAL",
  ["no"] = "NORMAL",
  ["v"] = "VISUAL",
  ["V"] = "VISUAL LINE",
  [""] = "VISUAL BLOCK",
  ["s"] = "SELECT",
  ["S"] = "SELECT LINE",
  [""] = "SELECT BLOCK",
  ["i"] = "INSERT",
  ["ic"] = "INSERT",
  ["R"] = "REPLACE",
  ["Rv"] = "VISUAL REPLACE",
  ["c"] = "COMMAND",
  ["cv"] = "VIM EX",
  ["ce"] = "EX",
  ["r"] = "PROMPT",
  ["rm"] = "MOAR",
  ["r?"] = "CONFIRM",
  ["!"] = "SHELL",
  ["t"] = "TERMINAL",
}
-- Func that gets current mode --
local function get_mode()
  local current_mode = vim.api.nvim_get_mode().mode
  return string.format(" %s ", modes[current_mode]):upper()
end
-- Editor info func --
local function get_filepath()
  local filepath = vim.fn.fnamemodify(vim.fn.expand "%", ":~:.:h")
  if filepath == "" or filepath == "." then
    return " "
  end
  return string.format(" %%<%s/", filepath)
end
local function get_filename()
  local filename = vim.fn.expand "%:t"
  if filename == "" then
    return ""
  end
  return filename .. " "
end
local function get_filetype()
  return string.format(" %s ", vim.bo.filetype):upper()
end
local function get_lineinfo()
  if vim.bo.filetype == "alpha" then
    return ""
  end
  return " %P %l:%c "
end
-- Build statusline --
Statusline = {}
Statusline.active = function()
  return table.concat {
    "%#Statusline#",
    get_mode(),
    "%#Normal# ",
    get_filepath(),
    get_filename(),
    "%#Normal#",
    "%=%#StatusLineExtra#",
    get_filetype(),
    get_lineinfo(),
  }
end
function Statusline.inactive()
  return " %F"
end
-- Deploy statusline --
vim.api.nvim_exec([[
augroup Statusline
au!
au WinEnter,BufEnter * setlocal statusline=%!v:lua.Statusline.active()
au WinLeave,BufLeave * setlocal statusline=%!v:lua.Statusline.inactive()
augroup END]],false)
--]]


