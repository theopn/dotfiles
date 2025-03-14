-- Theovim autocmds

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
