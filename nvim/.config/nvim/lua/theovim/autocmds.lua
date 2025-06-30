-- Theovim autocmds

local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup

-- Highlight on yank
autocmd("TextYankPost", {
  group = augroup("TheovimYankHighlight", { clear = true }),
  pattern = "*",
  callback = function() vim.hl.on_yank({ timeout = 300, }) end,
  desc = "Highlight yanked text",
})

-- Switch to insert mode when terminal is open
autocmd({ "TermOpen", "BufEnter" }, {
  -- TermOpen: for when terminal is opened for the first time
  -- BufEnter: when you navigate to an existing terminal buffer
  group = augroup("TheovimTerminal", { clear = true }),
  pattern = "term://*", --> only applicable for "BufEnter", an ignored Lua table key when evaluating TermOpen
  callback = function() vim.cmd("startinsert") end
})

-- Update indentation guide dynamically
local update_leadmultispace_group = augroup("UpdateLeadmultispace", { clear = true })

--- Dynamically adjust `leadmultispace` in `listchars` (buffer level) based on `shiftwidth`
local function update_leadmultispace()
  local lead = "┊"
  for _ = 1, vim.bo.shiftwidth - 1 do
    lead = lead .. " "
  end
  vim.opt_local.listchars:append({ leadmultispace = lead })
end

-- When `shiftwidth` was manually changed
autocmd("OptionSet", {
  group = update_leadmultispace_group,
  pattern = { "shiftwidth", "filetype" },
  callback = update_leadmultispace,
})

-- When shiftwidth was changed by ftplugin
autocmd({ "BufWinEnter", }, {
  group = update_leadmultispace_group,
  callback = update_leadmultispace,
  --once = true,
})

-- Disable the autocmd disabling swap warning in starting in 0.10
vim.cmd [[au! nvim.swapfile]]
