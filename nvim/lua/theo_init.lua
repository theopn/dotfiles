--[[
 ________                       _         _      _ __
/_  __/ /  ___ ___    ___ _  __(_)_ _    (_)__  (_) /_
 / / / _ \/ -_) _ \  / _ \ |/ / /  ' \  / / _ \/ / __/
/_/ /_//_/\__/\___/ /_//_/___/_/_/_/_/ /_/_//_/_/\__/

--]]

---[[ Func for easier setting
local GLOBAL = vim.o
local WINDOW = vim.wo
local function vim_set(opt, scope, val)
  scope[opt] = val
end
local function vim_map(mode, shortcut, target)
  vim.api.nvim_set_keymap(mode, shortcut, target, { noremap = true })
end
--]]

---[[ Syntax Related Settings
do
  local syntax_opt = {
    { opt = "filetype", val = 'on' },
    { opt = "so", val = 7 },
    { opt = "hlsearch", val = true },
    { opt = "incsearch", val = true },
    { opt = "ignorecase", val = true },
    { opt = "smartcase", val = true },
    { opt = "foldmethod", val = "indent" },
    { opt = "foldlevel", val = 1 },
    { opt = "list", val = true },
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
--]]

---[[ Text Edit Related Settings
do
  local edit_opt = {
    { opt = "shiftwidth", val = 2 },
    { opt = "tabstop", val = 2 },
    { opt = "softtabstop", val = 2 },
    { opt = "expandtab", val = true },
    { opt = "mouse", val = 'a' },
    { opt = "spelllang", val = "en" },
    { opt = "spellsuggest", val = "best,8" },
    { opt = "spell", val = true },
  }
  for i,v in ipairs(edit_opt) do
    vim_set(v.opt, GLOBAL, v.val)
  end
end
--]

---[[ Visual Related Settings
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
--]]

---[[ Key Binding Related Settings
vim.api.nvim_set_keymap('n', '<Space>', '<Nop>', { noremap = true }) --> Unbind space
vim.g.mapleader = " " --> Space as the leader key
do
  local key_opt = {
    { mode = 'n', shortcut = "<C-t>", target = ":tabnew<CR>" }, --> Open a new buffer
    { mode = 'i', shortcut = "jk", target = "<ESC>:write<CR>" }, --> "joke", get it? Ha ha I'm so funny
    { mode = 'v', shortcut = "<leader>y", target = '"+y' }, --> Copy to the system clipboard
    { mode = 't', shortcut = "<ESC>", target = ":<C-\\><C-n>" }, --> ESC for term
    -- Auto closers --
    { mode = 'i', shortcut = "(", target = "()<LEFT>" },
    { mode = 'i', shortcut = "[", target = "[]<LEFT>" },
    { mode = 'i', shortcut = "{<CR>", target = "{<CR>}<ESC>ko" }, --> Add <TAB> after ko if needed
    -- Split pane navigation --
    { mode = 'n', shortcut = "<leader>h", target = "<C-W>h" },
    { mode = 'n', shortcut = "<leader>j", target = "<C-W>j" },
    { mode = 'n', shortcut = "<leader>k", target = "<C-W>k" },
    { mode = 'n', shortcut = "<leader>l", target = "<C-W>l" },
    -- Search auto center --
    { mode = 'n', shortcut = "n", target = "nzz" },
    { mode = 'n', shortcut = "N", target = "Nzz" },
    -- Spell check --
    { mode = 'n', shortcut = "<leader>s", target = "z=" }, --> Correct spelling error
    { mode = 'n', shortcut = "<leader>cs", target = ":set spell!<CR>" }, --> Turn off spellcheck
  }
  for i,v in ipairs(key_opt) do
    vim_map(v.mode, v.shortcut, v.target)
  end
end
--]]

