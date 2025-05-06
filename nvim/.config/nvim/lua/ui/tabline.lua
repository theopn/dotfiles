--- *tabline.lua* Theovim Tabline
--- $ figlet -f tinker-joy theovim
---  o  o
---  |  |                  o
--- -o- O--o o-o o-o o   o   o-O-o
---  |  |  | |-' | |  \ /  | | | |
---  o  o  o o-o o-o   o   | o o o
---
--- Initialize tabline with:
--- - Theovim logo
--- - Clickable tabs
--- - Number of window iff there is more than one
--- - Clickable close button that changes when curr buf is modified
--- - # of buffer and # of win

local M = {}

local theovimlogo = vim.g.have_nerd_font and "Theo  " or "Theovim"

--- Returns the Lua list of listed buffers
---@return table listed_buf list of buffers that are loaded, valid, and listed
local function get_listed_bufs()
  local listed_buf = {}
  local len = 0 --> direct insertion is faster than table.insert
  for buf = 1, vim.fn.bufnr("$") do
    if vim.fn.buflisted(buf) ~= 0 then
      len = len + 1
      listed_buf[len] = buf
    end
  end
  return listed_buf
end

--- Format a string for Vim tabline based on tabs and current buffer information
---
--- @return string s Formatted string to be used as a Vim tabline
M.build = function()
  -- Init + %< to have truncation start after the logo
  local s = "%#TabLineFill#" .. theovimlogo .. " %<"

  local curr_tabnum = vim.fn.tabpagenr()
  for i = 1, vim.fn.tabpagenr("$") do
    -- Variables
    local winnum = vim.fn.tabpagewinnr(i)
    local buflist = vim.fn.tabpagebuflist(i)
    local curr_bufnum = buflist[winnum]
    local curr_bufname = vim.fn.bufname(curr_bufnum)
    local is_curr_buff_modified = vim.fn.getbufvar(curr_bufnum, "&modified")

    -- Basic setup
    s = s .. ((i == curr_tabnum) and "%#TabLineSel#" or "%#TabLine#") --> diff hl for active and inactive tabs
    s = s .. " "                                                      --> Left margin/separator
    s = s .. "%" .. i .. "T"                                          --> make tab clickable (%nT)
    s = s .. i .. " "                                                 --> Tab index

    -- Current name of the tab
    local display_curr_bufname = vim.fn.fnamemodify(curr_bufname, ":t")
    -- Limiting inactive tab name to n character + 3 (... that will be appended)
    local bufname_len_limit = 24
    if i ~= curr_tabnum and string.len(display_curr_bufname) > bufname_len_limit + 3 then
      display_curr_bufname = string.sub(display_curr_bufname, 1, 10) .. "..."
    end
    -- Append formatted bufname
    if display_curr_bufname ~= "" then
      s = s .. display_curr_bufname
    else
      s = s .. "[No Name]"
    end

    -- Number of windows in the tab
    if #buflist > 1 then s = s .. " [" .. (#buflist) .. " Win]" end

    -- Make close button clickable ("%nX", %999X closes the current tab)
    local curr_tab_close_btn = "%" .. i .. "X"
    s = s .. curr_tab_close_btn
    -- Functional close button or modified indicator
    s = s .. ((is_curr_buff_modified == 1) and " +" or " X")

    -- Reset button (%T)
    s = s .. "%T"
    -- BG highlight and left spacing
    s = s .. " %#TabLineFill# "
  end

  -- Number of buffer and tab on the far right
  s = s .. "%="                                           --> spacer
  s = s .. "%#TabLineSel#"                                --> highlight
  s = s .. string.format(" #Tab: %i", vim.fn.tabpagenr("$"))  --> num tabs
  s = s .. " |"
  s = s .. string.format(" #Buf: %i", #get_listed_bufs()) --> num buffers
  s = s .. " "                                            --> right margin
  return s
end

--- Evaluate user options, devicons presence and assign a newly created global function based on build() to tabline
--- Inspired by https://github.com/crispgm/nvim-tabline
function M.setup()
  _G.nvim_tabline = function()
    return M.build()
  end

  vim.go.tabline = "%!v:lua.nvim_tabline()"
end

return M
