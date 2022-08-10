--[[
 ________                       _         _      _ __
/_  __/ /  ___ ___    ___ _  __(_)_ _    (_)__  (_) /_
 / / / _ \/ -_) _ \  / _ \ |/ / /  ' \  / / _ \/ / __/
/_/ /_//_/\__/\___/ /_//_/___/_/_/_/_/ /_/_//_/_/\__/

--]]

-- {{{ Functions for easier setting
local GLOBAL = vim.o
local WINDOW = vim.wo
local function vim_set(opt, scope, val)
  scope[opt] = val
end
-- vim.api.nvim_set_keymap is old way of doing it
local function vim_map(mode, shortcut, target)
  vim.keymap.set(mode, shortcut, target, { noremap = true, silent = true })
end
-- }}}

-- {{{ Syntax Related Settings
do
  local syntax_opt = {
    { opt = "filetype", val = 'on' },
    { opt = "scrolloff", val = 7 }, --> Keep at least 7 lines visible above and below the cursor
    { opt = "hlsearch", val = true }, --> Highlight search result
    { opt = "incsearch", val = true }, --> Should be enabled by default
    { opt = "ignorecase", val = true }, --> Needed for smartcase
    { opt = "smartcase", val = true }, --> Ignore case iff search input was all lowercase
    { opt = "foldmethod", val = "marker" },
    { opt = "foldlevel", val = 0 },
    { opt = "foldenable", val = false }, --> No fold enabled until I hit zc.
    { opt = "list", val = true }, --> Needed for listchars
    { opt = "splitright", val = false }, --> Vertical split default to left
    { opt = "splitbelow", val = false },
  }
  for i,v in ipairs(syntax_opt) do
    vim_set(v.opt, GLOBAL, v.val)
  end
end
-- Trailing white space --
vim.opt.listchars = { tab = "!>", trail = "␣", nbsp = "+" }
-- Highlight on yank --
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})
-- }}}

-- {{{ Text Edit Related Settings
do
  local edit_opt = {
    { opt = "shiftwidth", val = 2 }, --> Indentation width
    { opt = "tabstop", val = 2 }, --> Backslash t width
    { opt = "softtabstop", val = 2 }, --> Tab key width
    { opt = "expandtab", val = true }, --> Tab as spaces
    { opt = "mouse", val = 'a' },
    { opt = "spell", val = true },
    { opt = "spelllang", val = "en" },
    { opt = "spellsuggest", val = "best,8" }, --> 8 suggestions for spell check
  }
  for i,v in ipairs(edit_opt) do
    vim_set(v.opt, GLOBAL, v.val)
  end
end
-- }}}

-- {{{ Visual Related Settings
do
  local vis_opt = {
    { opt = "number", val = true },
    { opt = "relativenumber", val = true },
    { opt = "colorcolumn", val = '120' },
    { opt = "cursorline", val = true },
    { opt = "cursorcolumn", val = true },
  }
  for i,v in pairs(vis_opt) do
    vim_set(v.opt, WINDOW, v.val)
  end
end
-- }}}

-- {{{ Key Binding Related Settings
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { noremap = true }) --> Unbind space
vim.g.mapleader = " " --> Space as the leader key
do
  local key_opt = {
    { mode = 'n', shortcut = "<leader>n", target = "<CMD>NvimTreeToggle<CR>" }, --> Tree toggle
    { mode = 'n', shortcut = "<leader>q", target = "<CMD>vsplit term://zsh<CR>" }, --> Quick terminal launch
    { mode = 'n', shortcut = "<leader>/", target = "<CMD>let @/=''<CR>" }, --> Clear search highlighting
    { mode = 'i', shortcut = "jk", target = "<ESC><CMD>write<CR>" }, --> "joke", get it? Ha ha
    { mode = 'v', shortcut = "<leader>y", target = '"+y' }, --> Copy to the system clipboard
    { mode = 't', shortcut = "<ESC>", target = "<C-\\><C-n>" }, --> ESC for term
    -- Auto bracket closers --
    { mode = 'i', shortcut = "(", target = "()<LEFT>" },
    { mode = 'i', shortcut = "[", target = "[]<LEFT>" },
    { mode = 'i', shortcut = "{<CR>", target = "{<CR>}<ESC>ko" }, --> Add <TAB> after ko if needed
    -- Split pane navigation --
    { mode = 'n', shortcut = "<leader>h", target = "<C-W>h" },
    { mode = 'n', shortcut = "<leader>j", target = "<C-W>j" },
    { mode = 'n', shortcut = "<leader>k", target = "<C-W>k" },
    { mode = 'n', shortcut = "<leader>l", target = "<C-W>l" },
    -- Tab navigation --
    { mode = 'n', shortcut = "<leader>t", target = "<CMD>tabnew<CR>" }, --> Open a new buffer
    { mode = 'n', shortcut = "<leader>,", target = "<CMD>BufferPrevious<CR>" }, --> Barbar plugin overrides "gT"
    { mode = 'n', shortcut = "<leader>.", target = "<CMD>BufferNext<CR>" }, --> Barbar plugin overrides "gt"
    -- Search auto center --
    { mode = 'n', shortcut = "n", target = "nzz" },
    { mode = 'n', shortcut = "N", target = "Nzz" },
    -- Spell check --
    { mode = 'n', shortcut = "<leader>s", target = "z=" }, --> Correct spelling error
    { mode = 'n', shortcut = "<leader>cs", target = "<CMD>set spell!<CR>" }, --> Toggle spellcheck
    -- Telescope --
    { mode = 'n', shortcut = "<leader>ff", target = "<CMD>Telescope find_files<CR>" },
    { mode = 'n', shortcut = "<leader>f/", target = "<CMD>Telescope current_buffer_fuzzy_find<CR>" }, --> Better search
  }
  for i,v in ipairs(key_opt) do
    vim_map(v.mode, v.shortcut, v.target)
  end
end
-- }}}

