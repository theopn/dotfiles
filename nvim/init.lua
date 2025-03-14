--- init.lua
---
---      \/       \/
---      /\_______/\
---     /   o   o   \
---    (  ==  ^  ==  )
---     )  [Ollie]  (
---    (             )
---    ( (  )   (  ) )
---   (__(__)___(__)__)
---  ___
---   | |_  _  _     o __
---   | | |(/_(_)\_/ | |||
---  Oliver : https://www.asciiart.eu/animals/cats + Jonathan added a few layers of belly because Oliver is a chunky boi
---

vim.g.have_nerd_font = true

-- {{{ options
-- Excludes default Neovim options:
-- https://neovim.io/doc/user/vim_diff.html#nvim-defaults
local opt            = vim.opt

-- Tab
opt.softtabstop      = 0    --> How many chracters the /cursor moves/ with <TAB> and <BS> -- 0 to disable
opt.expandtab        = true --> Use space instead of tab
opt.shiftwidth       = 2    --> Number of spaces to use for auto-indentation, <<, >>, etc.
opt.shiftround       = true --> Make the indentation to a multiple of shiftwidth when using < or >

-- Location in the buffer
opt.number           = true
opt.relativenumber   = true
opt.cursorline       = true
opt.cursorlineopt    = "number" --> line, screenline, both (i.e., "number,line")
opt.cursorcolumn     = true

-- Search and replace
opt.ignorecase       = true    --> Ignore case in search
opt.smartcase        = true    --> /smartcase -> apply ignorecase | /sMartcase -> do not apply ignorecase
opt.inccommand       = "split" --> show the substitution in a split window

-- Split
opt.splitright       = true --> Vertical split created right
opt.splitbelow       = true --> Horizontal split created below

-- UI
opt.signcolumn       = "yes" --> Render signcolumn always to prevent text shifting
opt.scrolloff        = 7     --> Keep minimum x number of screen lines above and below the cursor
opt.termguicolors    = true  --> Enables 24-bit RGB color in the TUI
opt.showtabline      = 2     --> 0: never, 1: >= 2 tabs, 2: always
opt.laststatus       = 2     --> 0: never, 1: >= 2 windows, 2: always, 3: always and have one global statusline

-- Char rendering
opt.list             = true --> Render special char in listchars
opt.listchars        = { tab = "⇥ ", trail = "␣", nbsp = "⍽", leadmultispace = "┊ ", }
opt.showbreak        = "↪" --> Render beginning of wrapped lines
opt.breakindent      = true --> Wrapped line will have the same indentation level as the beginning of the line

-- Spell
opt.spell            = false    --> autocmd will enable spellcheck in Tex or markdown
opt.spelllang        = { "en" }
opt.spellsuggest     = "best,8" --> Suggest 8 words for spell suggestion
opt.spelloptions     = "camel"  --> Consider CamelCase when checking spelling

-- Fold
opt.foldenable       = false                        --> Open all folds until I close them using zc/zC or update using zx
opt.foldmethod       = "expr"                       --> Use `foldexpr` function for folding
opt.foldexpr         = "nvim_treesitter#foldexpr()" --> Treesitter folding
--foldlevel      = 2                            --> Ignore n - 1 level fold

-- Update time
opt.updatetime       = 250
opt.timeoutlen       = 300

-- Window size
opt.winminwidth      = 3

-- Others
opt.mouse            = "a"
opt.confirm          = true --> Confirm before exiting with unsaved bufffer(s)
-- }}}


-- {{{ keymaps
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
set("n", "<C-u>", "<C-u>zz")
set("n", "<C-d>", "<C-d>zz")

set("n", "<C-w>+", "<C-w>+<CMD>call feedkeys('<C-w>')<CR>", { desc = "Increase the window height (press + to repeat)" })
set("n", "<C-w>-", "<C-w>-<CMD>call feedkeys('<C-w>')<CR>", { desc = "Decrease the window height (press - to repeat)" })
set("n", "<C-w>>", "<C-w>><CMD>call feedkeys('<C-w>')<CR>", { desc = "Increase the window width (press > to repeat)" })
set("n", "<C-w><", "<C-w><<CMD>call feedkeys('<C-w>')<CR>", { desc = "Decrease the window width (press < to repeat)" })

-- Custom keymaps
set("i", "jk", "<ESC>", { desc = "Better ESC" })
set("i", "<C-s>", "<C-g>u<ESC>[s1z=`]a<C-g>u", { desc = "Fix nearest [S]pelling error and put the cursor back" })

-- Copy and paste
set({ "n", "x" }, "<leader>a", "gg<S-v>G", { desc = "Select [A]ll" })
set("x", "<leader>y", '"+y', { desc = "[Y]ank to the system clipboard (+)" })
set("n",
  "<leader>p",
  ":echo '[Theovim] e.g.: `:normal \"3p` to paste the content of the register 3'<CR>" .. ':reg<CR>:normal "',
  { silent = true, desc = "[P]aste from one of the registers" })
set("x",
  "<leader>p",
  '"_dP', --> [d]elete the selection and send content to _ void reg then [P]aste (b4 cursor unlike small p)
  { desc = "[P]aste the current selection without overriding the register" })

-- Buffer
set("n", "[b", "<CMD>bprevious<CR>", { desc = "Go to previous [B]uffer" })
set("n", "]b", "<CMD>bnext<CR>", { desc = "Go to next [B]uffer" })
set("n",
  "<leader>b",
  ":echo '[Theovim] Choose a buffer'<CR>" .. ":ls<CR>" .. ":b<SPACE>",
  { silent = true, desc = "Open [B]uffer list" })
set("n",
  "<leader>k",
  ":echo '[Theovim] Choose a buf to delete (blank to choose curr)'<CR>" .. ":ls<CR>" .. ":bdelete<SPACE>",
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
    vim.cmd("botright " .. math.ceil(vim.fn.winheight(0) * (1 / 3)) .. "sp | term")
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
-- }}}


-- {{{ commands
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
-- }}}


-- {{{ autocmds
-- Highlight on yank
vim.api.nvim_create_autocmd("TextYankPost", {
  group = vim.api.nvim_create_augroup("TheovimYankHighlight", { clear = true }),
  pattern = "*",
  callback = function() vim.highlight.on_yank() end,
  desc = "Highlight yanked text",
})

-- Switch to insert mode when terminal is open
vim.api.nvim_create_autocmd({ "TermOpen", "BufEnter" }, {
  -- TermOpen: for when terminal is opened for the first time
  -- BufEnter: when you navigate to an existing terminal buffer
  group = vim.api.nvim_create_augroup("TheovimTerminal", { clear = true }),
  pattern = "term://*", --> only applicable for "BufEnter", an ignored Lua table key when evaluating TermOpen
  callback = function() vim.cmd("startinsert") end
})

-- Update indentation guide dynamically
local update_leadmultispace_group = vim.api.nvim_create_augroup("UpdateLeadmultispace", { clear = true })

--- Dynamically adjust `leadmultispace` in `listchars` (buffer level) based on `shiftwidth`
local function update_leadmultispace()
  local lead = "┊"
  for _ = 1, vim.bo.shiftwidth - 1 do
    lead = lead .. " "
  end
  vim.opt_local.listchars:append({ leadmultispace = lead })
end

-- When `shiftwidth` was manually changed
vim.api.nvim_create_autocmd("OptionSet", {
  group = update_leadmultispace_group,
  pattern = { "shiftwidth", "filetype" },
  callback = update_leadmultispace,
})

-- When shiftwidth was changed by ftplugin
vim.api.nvim_create_autocmd("BufEnter", {
  group = update_leadmultispace_group,
  pattern = "*",
  callback = update_leadmultispace,
  once = true,
})
-- }}}


-- {{{ netrw
vim.g.netrw_banner = 0
vim.g.netrw_liststyle = 0    --> 0: Simple, 1: Detailed, 2: Thick, 3: Tree
vim.g.netrw_browse_split = 3 --> Open file in 0: Reuse the same win, 1: Horizontal split, 2: Vertical split, 3: New tab
vim.g.netrw_winsize = 25     --> seems to be in percentage

vim.g.is_netrw_open = false
--- Toggles netrw
--- Requires vim.g.is_netrw_open global variable to function properly
local function toggle_netrw()
  local fn = vim.fn
  if vim.g.is_netrw_open then
    for i = 1, fn.bufnr("$") do
      if fn.getbufvar(i, "&filetype") == "netrw" then
        vim.cmd("bwipeout " .. i)
      end
    end
    vim.g.is_netrw_open = false
  else
    vim.cmd("Lex")
    vim.g.is_netrw_open = true
  end
end
vim.keymap.set("n", "<leader>n", toggle_netrw,
  { silent = true, noremap = true, desc = "Toggle [N]etrw" })
-- }}}


-- {{{ diagnostic
-- Diagnostic appearance
vim.diagnostic.config({
  float = {
    border = "rounded",
    format = function(diagnostic)
      -- "ERROR (line n): message"
      return string.format("%s (line %i): %s",
        vim.diagnostic.severity[diagnostic.severity],
        diagnostic.lnum + 1,
        diagnostic.message)
    end
  },
  update_in_insert = false,
})

-- Keymaps
vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Open diagnostic [Q]uickfix list" })
-- }}}


-- {{{ lazy.nvim
-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out,                            "WarningMsg" },
      --{ "\nPress any key to exit..." },
    }, true, {})
    --vim.fn.getchar()
    --os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

-- Either pass the table (containing the plugin info) or string (a directory containing Lua modules)
-- Following will import all Lua files in lua/plugins
require("lazy").setup("plugins")
-- }}}

-- {{{ UI module calls
require("ui.statusline").setup()
require("ui.tabline").setup()
require("ui.dashboard").setup()
-- }}}

-- {{{ Neovide settings
if vim.g.neovide then
  local padding = 10
  vim.g.neovide_padding_top = padding
  vim.g.neovide_padding_bottom = padding
  vim.g.neovide_padding_right = padding
  vim.g.neovide_padding_left = padding

  vim.g.neovide_hide_mouse_when_typing = true
  vim.g.neovide_cursor_vfx_mode = "railgun"

  vim.g.neovide_transparency = 0.69
  vim.g.neovide_floating_blur_amount_x = 2.0
  vim.g.neovide_floating_blur_amount_y = 2.0
end
-- }}}

-- vim: foldmethod=marker
