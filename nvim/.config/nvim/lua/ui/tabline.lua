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
--- - List of buffers in the current tab (up to the first 3 windows)

local M = {}

local theovimlogo = vim.g.have_nerd_font and "Theo  " or "Theovim"

---Given a list of |window-ID|, filters out abnormal (i.e., float) windows.
---The mechanism relies on checking the `relative` field of the window config (|api-win_config|),
---as |api-floatwin| says to "check whether a window is floating, check whether `relative` option ... is non-empty."
---Example:
---```lua
---local curr_tab_non_float_wins = filter_float_windows(vim.api.nvim_tabpage_list_wins(0))
---```
---@param winids table List of window-iD
---@return table non_floats List of window-ID of non-floating windows
local function filter_float_windows(winids)
  local non_floats = {}
  for _, winid in pairs(winids) do
    if vim.api.nvim_win_get_config(winid).relative == "" then
      non_floats[#non_floats + 1] = winid
    end
  end
  return non_floats
end

---Given a list of buffer numbers (e.g., return value of |bufnr()|), filters out duplicates AND unlisted buffers.
---Example
---```lua
---local curr_tab_listed_buffers = filter_listed_buf(vim.fn.tabpagebuflist())
---````
---@param bufnums table List of buffer numbers
---@return table listed List of listed buffers
local function filter_listed_buf(bufnums)
  local listed = {}
  local hash = {}
  for _, buf in pairs(bufnums) do
    if vim.fn.buflisted(buf) == 1 and (not hash[buf]) then
      listed[#listed + 1] = buf
      hash[buf] = true
    end
  end
  return listed
end

---Format a string for Vim tabline based on tabs and current buffer information
---@return string s Formatted string to be used as a Vim tabline
M.build = function()
  -- Init + %< to have truncation start after the logo
  local s = "%#TabLineFill#" .. theovimlogo .. " %<"

  local curr_tab_id = vim.api.nvim_get_current_tabpage()
  for _, tab_id in pairs(vim.api.nvim_list_tabpages()) do
    local tab_num = vim.api.nvim_tabpage_get_number(tab_id)
    local winlist = filter_float_windows(vim.api.nvim_tabpage_list_wins(tab_id))

    -- Basic setup
    s = s .. ((tab_id == curr_tab_id) and "%#TabLineSel#" or "%#TabLine#") --> diff hl for active and inactive tabs
    s = s .. " "                                                           --> Left margin/separator
    s = s .. "%" .. tab_num .. "T"                                         --> make tab clickable (%nT)
    s = s .. tab_num .. " "                                                --> Tab index

    -- Number of windows in the tab
    if #winlist > 1 then s = s .. "[" .. (#winlist) .. " Win]" end

    -- Make close button clickable ("%nX", %999X closes the current tab)
    s = s .. "%" .. tab_num .. "X"
    s = s .. "X"

    -- Reset button (%T)
    s = s .. "%T"
    -- BG highlight and left spacing
    s = s .. " %#TabLineFill# "
  end

  s = s .. "%="            --> spacer
  s = s .. "%#TabLineSel#" --> highlight

  -- List of buffers in the current tab
  local buflist = filter_listed_buf(vim.fn.tabpagebuflist(vim.api.nvim_tabpage_get_number(curr_tab_id)))

  local curr_bufname = vim.fn.bufname()
  --local is_curr_buff_modified = vim.fn.getbufvar(curr_bufname, "&modified")


  for _, buf in pairs(buflist) do
    local display_curr_bufname = vim.fn.fnamemodify(vim.fn.bufname(buf), ":t")

    -- Limiting inactive tab name to n character + 3 (... that will be appended)
    local bufname_len_limit = 10
    if string.len(display_curr_bufname) > bufname_len_limit + 3 then
      display_curr_bufname = string.sub(display_curr_bufname, 1, 10) .. "..."
    end

    -- Empty buffer handling
    if display_curr_bufname ~= "" then
      s = s .. display_curr_bufname
    else
      s = s .. "[No Name]"
    end

    -- Append formatted bufname
  end

  s = s .. " " --> right margin
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
