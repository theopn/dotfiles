--- completion.lua
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
---
--- Configurations for built-in completion
--- References:
---   - Overall native insert completion framework: :h ins-completion
---   - How native LSP assigns Omni completion: :h lsp-completion
---   - Multi-source completion introduced in 0.12: :h 'complete'

-- Completion behavior
-- no pre-insert or pre-select, show menu even w/ one item, use popup doc
vim.o.completeopt = "noselect,menu,menuone,popup"
-- behavior of <TAB> in cmd completion. Do not pre-select, match full completion
-- (obviously irrelevant to insert completion, but I thought it was fitting here)
vim.o.wildmode = "noselect:full"
-- o: Omnifunc, .: curr buf, w: other windows, b: other loaded buffers
vim.o.complete = "o,.,w"
-- Neovim 0.12 option, automatically trigger completion.
-- Back in the day... I would have to create an autocmd to trigger C-x C-n for each keystroke:
-- https://github.com/theopn/dotfiles/commit/62d22a174ac6d44fe027fcaadda72733512e7f0c#diff-5148363aa126ba5c07771546e2e27979606cc84c727f26eeadc264feb92321b7
-- Neovim has come a long way...
vim.o.autocomplete = true

-- completion popup apperance
vim.o.pumblend = 10;
vim.o.pumborder = "rounded";
vim.o.pumheight = 5;


-- Tab completion and <CR> to select

-- Function to check if the cursor is located after a word
-- https://github.com/hrsh7th/nvim-cmp/wiki/Example-mappings
local has_words_before = function()
  unpack = unpack or table.unpack
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

local tab_complete = function()
  if vim.fn.pumvisible() ~= 0 then --> completion is visible, cycle through it
    return "<C-n>"
    -- order matters, since when snippet is active, you likely have a word before
  elseif vim.snippet.active({ direction = 1 }) then --> from :h vim.snippet.jump()
    -- you must accept the completion item with <C-y> in order to expand
    vim.snippet.jump(1)
  elseif has_words_before() then --> has a word before, probably want a completion
    -- Starting in 0.12, C-n triggers the multi-source completion set by |'completion'|
    -- So instead of <C-x><C-o> or other vintage single-source completion, we can simply use C-n for everything
    return "<C-n>"
  else
    return "<TAB>"
  end
end

local s_tab_complete = function()
  if vim.fn.pumvisible() ~= 0 then
    return "<C-p>"
  elseif vim.snippet.active({ direction = -1 }) then
    vim.snippet.jump(-1)
  else
    return "<S-TAB>"
  end
end

local cr_complete = function()
  if vim.fn.pumvisible() ~= 0 then
    return "<C-y>"
  else
    return "<CR>"
  end
end

vim.keymap.set({ "i", "s" }, "<Tab>", tab_complete, { expr = true, silent = true, remap = false })
vim.keymap.set({ "i", "s" }, "<S-Tab>", s_tab_complete, { expr = true, silent = true, remap = false })
-- nvim-autopairs overrides <CR> for some reason, so warp this in schedule
vim.schedule(function()
  vim.keymap.set({ "i", "s" }, "<CR>", cr_complete, { expr = true, silent = true, remap = false })
end)
