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

-- noinsert: when a completion is triggered (e.g., <C-x><C-o>), do not pre-select anything.
--          It will still select the first item
-- noselect: do not select anything upon the completion trigger. Precedes noinsert
-- menu: show the popup menu for suggestions
-- menuone: show the popup menu even where there is only one suggestion
--         kind of necessary with noselect since you want a visual guide that a completion is available
-- popup: show information for the completion item. Do not expect the level of popup from plugins like nvim-cmp.
--        They are often rendering a new hover window to give a detailed view
vim.o.completeopt = "noselect,menu,menuone,popup"

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

-- Trigger completion in `triggers` character insertion
-- vim.api.nvim_create_autocmd("InsertCharPre", {
--   pattern = "*",
--   callback = function()
--     --local triggers = { "a", "b", "c", "d" }
--     local char = vim.api.nvim_get_vvar("char")
--     --if vim.tbl_contains(triggers, char) then
--     if char:match("%a") then
--       if vim.bo.omnifunc ~= "" then
--         vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-x><C-o>", true, false, true), "n", false)
--       else
--         vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-x><C-n>", true, false, true), "n", false)
--       end
--     end
--   end,
-- })

-- Trigger hover documentation when cycling through completion
-- vim.api.nvim_create_autocmd("CompleteChanged", {
--   --group = vim.api.nvim_create_augroup("TheovimYankHighlight", { clear = true }),
--   pattern = "*",
--   callback = function()
--     vim.lsp.buf.hover({
--       border = "rounded",
--       close_events = {
--         "CompleteChanged",
--         "CompleteDonePre",
--         "InsertChange",
--         "InsertCharPre",
--         "InsertLeavePre",
--       },
--     })
--   end,
--   --desc = "Highlight yanked text",
-- })

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
