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
--- Configurations for built-in completion (:h ins-completion)

-- WARN: This config is currently not in use
-- This was my attempt to replace nvim-cmp with the native ins-completion, but nvim-cmp offers far too many
-- functionalities for the native completion engine to replace.
-- I am saving it for the future Neovim updates when ins-completion gets better.

-- https://github.com/hrsh7th/nvim-cmp/wiki/Example-mappings
local has_words_before = function()
  unpack = unpack or table.unpack
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

local has_lsp_completion = function()
  return vim.bo.omnifunc == "v:lua.vim.lsp.omnifunc"
end

-- https://gist.github.com/MariaSolOs/2e44a86f569323c478e5a078d0cf98cc
local tab_complete = function()
  if vim.fn.pumvisible() ~= 0 then                  --> completion is visible, cycle through it
    return "<C-n>"
  elseif vim.snippet.active({ direction = 1 }) then --> from :h vim.snippet.jump()
    -- you must accept the completion item with <C-y> in order to expand
    vim.snippet.jump(1)
  elseif has_words_before() then --> has a word before, probably want a completion
    if has_lsp_completion() then
      return "<C-x><C-o>"
    else
      return "<C-x><C-n>"
    end
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

vim.keymap.set({ "i", "s" }, "<Tab>", tab_complete, { expr = true, silent = true, remap = false })
vim.keymap.set({ "i", "s" }, "<S-Tab>", s_tab_complete, { expr = true, silent = true, remap = false })


-- NOTE: Experimental
-- Trigger completion in `triggers` character insertion
vim.api.nvim_create_autocmd("InsertCharPre", {
  pattern = "*",
  callback = function()
    --local triggers = { "a", "b", "c", "d" }
    local char = vim.api.nvim_get_vvar("char")
    --if vim.tbl_contains(triggers, char) then
    if char:match("%a") then  --> any alphanumeric
      if vim.bo.omnifunc ~= "" then
        vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-x><C-o>", true, false, true), "n", false)
      else
        vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-x><C-n>", true, false, true), "n", false)
      end
    end
  end,
})

-- NOTE: Experimental
-- 1. Adds a friendly-snippet like snippets to the global abbreviation list
-- 2. Trigger abbreviation only when C-] is received
--[[ Usage:
-- https://github.com/rafamadriz/friendly-snippets/blob/main/snippets/lua/lua.json
_G.addSnippet({
 prefix = "foreach",
 body = { "for i, ${1:x} in pairs(${2:table}) do", "\t$0", "end" },
})

_G.addSnippet({
 prefix = "for",
 body = { "for ${1:i}=${2:1},${3:10} do", "\t$0", "end" },
})
--]]
vim.g.abbrev_list = {}
-- Modified https://boltless.me/posts/neovim-config-without-plugins-2025/
function _G.addSnippet(opts)
  local tmp = vim.g.abbrev_list
  table.insert(tmp, opts.prefix)
  vim.g.abbrev_list = tmp

  vim.keymap.set("ia", opts.prefix, function()
    -- Only expand the snippet with C-]
    local c = vim.fn.nr2char(vim.fn.getchar(0))
    if c ~= "" then
      vim.api.nvim_feedkeys(opts.prefix .. c, "i", true)
      return
    end

    vim.snippet.expand(table.concat(opts.body))
  end, { buffer = 0 })
end

-- NOTE: Experimental
-- Set the user-defined completefunc (C-x C-l) to list of abbreviation added by the _G.addSnippet function above
-- https://dev.to/cherryramatis/how-to-create-your-own-completion-for-vim-31ip
vim.cmd [[
set completefunc=CompleteAbbrev
function! CompleteAbbrev(findstart, base) abort
    if a:findstart
      let s:startcol = col('.') - 1
      while s:startcol > 0 && getline('.')[s:startcol - 1] =~ '\a'
        let s:startcol -= 1
      endwhile
      return s:startcol
    endif

    if a:base->len() == 0
      return g:abbrev_list
    endif

    return g:abbrev_list->matchfuzzy(a:base)
endfunction
]]
