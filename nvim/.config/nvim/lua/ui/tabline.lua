--- *tabline.lua* Theovim Tabline
--- $ figlet -f tinker-joy theovim
---  o  o
---  |  |                  o
--- -o- O--o o-o o-o o   o   o-O-o
---  |  |  | |-' | |  \ /  | | | |
---  o  o  o o-o o-o   o   | o o o
---
--- Initialize tabline with:
--- - Clickable tab list
--- - Number of window iff there is more than one
--- - List of buffers in the current tab

Tabline = {}

local logo = vim.g.have_nerd_font and " 󰬛  " or " <Theovim> "


---Given a list of |window-ID|, filters out abnormal (i.e., float) windows.
---The mechanism relies on checking the `relative` field of the window config (|api-win_config|),
---as |api-floatwin| states "To check whether a window is floating, check whether `relative` option ... is non-empty."
---Example:
---
---```lua
---local currTabNonFloats = filterFloatWins(vim.api.nvim_tabpage_list_wins(0))
---```
---@param winids table List of window-iD
---@return table nonFloats List of window-ID of non-floating windows
local function filterFloatWins(winids)
  local nonFloats = {}
  for _, winid in pairs(winids) do
    if vim.api.nvim_win_get_config(winid).relative == "" then
      nonFloats[#nonFloats + 1] = winid
    end
  end
  return nonFloats
end


---Given a list of buffer numbers (e.g., return value of |tabpagebuflist()|),
---filters out duplicates AND unlisted buffers.
---Example:
---
---```lua
---local currTabListedBuf = filterUnlistedBuffers(vim.fn.tabpagebuflist())
---````
---@param bufnums table List of buffer numbers
---@return table listed List of listed buffers
local function filterUnlistedBuffers(bufnums)
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


---Format a string for Vim tabline based on tabs and buffers on the current tab
---@return string s Formatted string to be used as a Vim tabline
Tabline.build = function()
  local s = "%#TabLineFill#" .. logo

  -- ========== Left ==========
  -- List of tabs
  -- ==========================

  local currTabID = vim.api.nvim_get_current_tabpage()
  for _, tabID in pairs(vim.api.nvim_list_tabpages()) do
    -- This should not happen
    if not vim.api.nvim_tabpage_is_valid(tabID) then break end

    local tabNum = vim.api.nvim_tabpage_get_number(tabID)
    local winids = filterFloatWins(vim.api.nvim_tabpage_list_wins(tabID))

    -- Basic setup
    s = s .. ((tabID == currTabID) and "%#TabLineSel#" or "%#TabLine#") --> diff hl for active and inactive tabs
    s = s .. " "                                                        --> Left margin/separator
    s = s .. "%" .. tabNum .. "T"                                       --> make tab clickable (%nT)
    s = s .. tabNum .. " "                                              --> Tab index

    -- Add a number of windows in the tab, if applicable
    if #winids > 1 then
      if vim.g.have_nerd_font then
        s = s .. "[ " .. (#winids) .. "]"
      else
        s = s .. "[" .. (#winids) .. " WINS]"
      end
    end

    -- Make close button clickable ("%nX", %999X closes the current tab)
    s = s .. "%" .. tabNum .. "X"
    s = s .. (vim.g.have_nerd_font and "" or "X")

    -- Add a reset button (%T)
    s = s .. "%T"
    -- Add a BG highlight and left spacing
    s = s .. " %#TabLineFill# "
  end

  -- ========== Middle ==========
  -- Empty space
  -- ============================
  s = s .. "%="            --> spacer
  s = s .. "%#TabLineSel#" --> highlight

  -- ========== Right ==========
  -- List of LISTED buffers in the current tab
  -- ===========================
  local bufnums = filterUnlistedBuffers(vim.fn.tabpagebuflist(vim.api.nvim_tabpage_get_number(currTabID)))
  for _, bufnum in pairs(bufnums) do
    local bufname = vim.fn.fnamemodify(vim.fn.bufname(bufnum), ":t")

    -- Give a name to an empty buffer
    if bufname == "" then
      bufname = "[No Name:" .. bufnum .. "]"
    end

    -- Limit bufname to n character + 3 (accounting for "..." to be appended)
    local bufnameLimits = 18
    if string.len(bufname) > bufnameLimits + 3 then
      bufname = string.sub(bufname, 1, bufnameLimits) .. "..."
    end

    -- Add a flag to a modified buffer
    if vim.fn.getbufvar(bufnum, "&modified") == 1 then
      bufname = bufname .. "[+]"
    end

    -- Determine highlight to use
    local hl = "%#TabLine#"
    if vim.fn.bufnr() == bufnum then
      hl = "%#TabLineSel#"
    end

    -- Append formatted bufname
    s = s .. hl .. " " .. bufname .. " "
  end

  -- Add a truncation starting point: truncate buffer information first
  s = s .. "%<"
  return s
end


-- Set tabline. The Lua function called must be globally accessible
Tabline.setup = function()
  vim.go.tabline = "%!v:lua.Tabline.build()"
end

return Tabline
