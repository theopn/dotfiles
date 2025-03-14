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
--- Global variable initialization and module calls

-- Global variables
vim.g.have_nerd_font = true
-- Neovide
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

-- Core modules
require("theovim.opt")
require("theovim.keymaps")
require("theovim.commands")
require("theovim.autocmds")
require("theovim.plugin-manager-bootstrap")

-- Custom UI modules
require("ui.statusline").setup()
require("ui.tabline").setup()

