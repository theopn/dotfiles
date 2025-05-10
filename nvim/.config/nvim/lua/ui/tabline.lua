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
  local s = "%#TabLineFill#" .. theovimlogo

  -- Configure left side of the tabline (list of tabs)
  local curr_tab_id = vim.api.nvim_get_current_tabpage()
  for _, tab_id in pairs(vim.api.nvim_list_tabpages()) do
    -- should not happen
    if not vim.api.nvim_tabpage_is_valid(tab_id) then break end

    local tab_num = vim.api.nvim_tabpage_get_number(tab_id)
    local winids = filter_float_windows(vim.api.nvim_tabpage_list_wins(tab_id))

    -- Basic setup
    s = s .. ((tab_id == curr_tab_id) and "%#TabLineSel#" or "%#TabLine#") --> diff hl for active and inactive tabs
    s = s .. " "                                                           --> Left margin/separator
    s = s .. "%" .. tab_num .. "T"                                         --> make tab clickable (%nT)
    s = s .. tab_num .. " "                                                --> Tab index

    -- Number of windows in the tab, if applicable
    if #winids > 1 then
      if vim.g.have_nerd_font then
        s = s .. "[ " .. (#winids) .. "]"
      else
        s = s .. "[" .. (#winids) .. " WINS]"
      end
    end

    -- Make close button clickable ("%nX", %999X closes the current tab)
    s = s .. "%" .. tab_num .. "X"
    s = s .. (vim.g.have_nerd_font and "" or "X")

    -- Reset button (%T)
    s = s .. "%T"
    -- BG highlight and left spacing
    s = s .. " %#TabLineFill# "
  end

  -- Add an empty space in the middle
  s = s .. "%="            --> spacer
  s = s .. "%#TabLineSel#" --> highlight

  -- Configure right side of the tabline (list of buffers in the current tab)
  -- List of buffers in the current tab
  local bufnums = filter_listed_buf(vim.fn.tabpagebuflist(vim.api.nvim_tabpage_get_number(curr_tab_id)))
  for _, bufnum in pairs(bufnums) do
    local bufname = vim.fn.fnamemodify(vim.fn.bufname(bufnum), ":t")

    -- Empty buffer handling
    if bufname == "" then
      bufname = "[No Name]"
    end

    -- Limiting bufname to n character + 3 (accounting for "..." to be appended)
    local bufname_len_limit = 18
    if string.len(bufname) > bufname_len_limit + 3 then
      bufname = string.sub(bufname, 1, bufname_len_limit) .. "..."
    end

    -- Modified buffer
    if vim.fn.getbufvar(bufnum, "&modified") == 1 then
      bufname = bufname .. "[+]"
    end

    local hl = "%#TabLine#"
    if vim.fn.bufnr() == bufnum then
      hl = "%#TabLineSel#"
    end

    -- Append formatted bufname
    s = s .. hl .. " " .. bufname .. " "
  end

  -- Truncate buffer information first
  s = s .. "%<"
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
