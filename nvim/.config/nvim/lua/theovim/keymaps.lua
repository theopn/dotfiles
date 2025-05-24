-- Theovim keymaps
local set = vim.keymap.set

-- Space as the leader
set({ "n", "v" }, "<Space>", "<Nop>")
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Default overrides
set("n", "<ESC>", "<CMD>nohlsearch<CR>")
set("t", "<ESC><ESC>", "<C-\\><C-n>", { desc = "Exit terminal mode" })

set("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true })
set("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true })

set("n", "n", "nzz")
set("n", "N", "Nzz")
--set("n", "<C-u>", "<C-u>zz")
--set("n", "<C-d>", "<C-d>zz")

-- Custom keymaps
set("i", "jk", "<ESC>", { desc = "Better ESC" })
set("i", "<C-s>", "<C-g>u<ESC>[s1z=`]a<C-g>u", { desc = "Fix nearest [S]pelling error and put the cursor back" })

-- Copy and paste
set({ "n", "x" }, "<leader>a", "gg<S-v>G", { desc = "Select [A]ll" })
set("x", "<leader>y", '"+y', { desc = "[Y]ank to the system clipboard (+)" })
set("x",
  "<leader>p",
  '"_dP', --> [d]elete the selection and send content to _ void reg then [P]aste (b4 cursor unlike small p)
  { desc = "[P]aste the current selection without overriding the register" })

-- Buffer
set("n", "[b", "<CMD>bprevious<CR>", { desc = "Go to previous [B]uffer" })
set("n", "]b", "<CMD>bnext<CR>", { desc = "Go to next [B]uffer" })
set("n",
  "<leader>k",
  ":echo 'Choose a buf to delete (blank to choose curr)'<CR>" .. ":ls<CR>" .. ":bdelete<SPACE>",
  { silent = true, desc = "[K]ill a buffer" })

-- Terminal
-- Toggle-able floating terminal based on TJ DeVries's video
local state = {
  floating = {
    buf = -1,
    win = -1,
  }
}

local function create_floating_window(opts)
  opts = opts or {}
  local width = opts.width or math.floor(vim.o.columns * 0.8)
  local height = opts.height or math.floor(vim.o.lines * 0.8)
  -- Calculate the position to center the window
  local col = math.floor((vim.o.columns - width) / 2)
  local row = math.floor((vim.o.lines - height) / 2)
  -- Create a buffer
  local buf = nil
  if vim.api.nvim_buf_is_valid(opts.buf) then
    buf = opts.buf
  else
    buf = vim.api.nvim_create_buf(false, true) -- No file, scratch buffer
  end
  -- Create the floating window
  local win_config = {
    relative = "editor",
    width = width,
    height = height,
    col = col,
    row = row,
    style = "minimal",
    border = "rounded",
  }
  local win = vim.api.nvim_open_win(buf, true, win_config)

  return { buf = buf, win = win }
end

local toggle_terminal = function()
  if not vim.api.nvim_win_is_valid(state.floating.win) then
    state.floating = create_floating_window { buf = state.floating.buf }
    if vim.bo[state.floating.buf].buftype ~= "terminal" then
      vim.cmd.terminal()
    end
  else
    vim.api.nvim_win_hide(state.floating.win)
  end
end

vim.api.nvim_create_user_command("Floaterminal", toggle_terminal, {})
set({ "n", "t" }, "<leader>tt", toggle_terminal, { desc = "[T]oggle floating [T]erminal" })

--           | :top sp |
-- |:top vs| |:abo| cu | |:bot vs |
-- |       | |:bel| rr | |        |
--           | :bot sp |
-- botright == bot
set("n",
  "<leader>tb",
  function()
    vim.cmd("botright " .. math.ceil(vim.fn.winheight(0) * 0.3) .. "sp | term")
  end,
  { desc = "Launch a [T]erminal in the [B]ottom" })

set("n",
  "<leader>tr",
  function()
    vim.cmd("bot " .. math.ceil(vim.fn.winwidth(0) * 0.3) .. "vs | term")
  end,
  { desc = "Launch a [T]erminal to the [R]ight" })

--- Move to a window (one of hjkl) or create a split if a window does not exist in the direction.
--- Lua translation of:
--- https://www.reddit.com/r/vim/comments/166a3ij/comment/jyivcnl/?utm_source=share&utm_medium=web2x&context=3
--- Usage: vim.keymap("n", "<C-h>", function() move_or_create_win("h") end, {})
--
---@param key string One of h, j, k, l, a direction to move or create a split
local function smarter_win_nav(key)
  local fn = vim.fn
  local curr_win = fn.winnr()
  vim.cmd("wincmd " .. key)        --> attempt to move

  if (curr_win == fn.winnr()) then --> didn't move, so create a split
    if key == "h" or key == "l" then
      vim.cmd("wincmd v")
    else
      vim.cmd("wincmd s")
    end

    vim.cmd("wincmd " .. key) --> move again
  end
end

set("n", "<C-h>", function() smarter_win_nav("h") end,
  { desc = "Move focus to the left window or create a horizontal split" })
set("n", "<C-j>", function() smarter_win_nav("j") end,
  { desc = "Move focus to the lower window or create a vertical split" })
set("n", "<C-k>", function() smarter_win_nav("k") end,
  { desc = "Move focus to the upper window or create a vertical split" })
set("n", "<C-l>", function() smarter_win_nav("l") end,
  { desc = "Move focus to the right window or create a horizontal split" })
