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
vim.g.have_transparent_bg = false

-- Core modules
require("theovim.opt")
require("theovim.keymaps")
require("theovim.commands")
require("theovim.autocmds")
require("theovim.lsp")
--require("theovim.completion")

require("theovim.lazy")

-- Custom UI modules
require("ui.tabline").setup()
require("ui.statusline-global").setup()
require("ui.winbar").setup()

-- Neovide
if vim.g.neovide then
  vim.o.guifont = "UbuntuMono Nerd Font:h15"

  local padding = 10
  vim.g.neovide_padding_top = padding
  vim.g.neovide_padding_bottom = padding
  vim.g.neovide_padding_right = padding
  vim.g.neovide_padding_left = padding

  vim.g.neovide_window_blurred = true
  vim.g.neovide_floating_blur_amount_x = 2.0
  vim.g.neovide_floating_blur_amount_y = 2.0
  vim.g.neovide_floating_shadow = true

  vim.g.neovide_opacity = 0.8
  vim.g.neovide_normal_opacity = 0.8

  vim.g.neovide_hide_mouse_when_typing = true

  vim.g.neovide_input_macos_option_key_is_meta = "only_left"

  vim.g.neovide_cursor_vfx_mode = "railgun"

  -- remap CMD Q
  vim.keymap.set({ "n", "i", "c", "v", "o", "t", "l" }, "<D-q>", "<NOP>")
end
