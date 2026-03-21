--  _   _                     _     _
-- | | | |                   (_)   | |
-- | |_| |__   ___  _____   ___  __| | ___
-- | __| '_ \ / _ \/ _ \ \ / / |/ _` |/ _ \
-- | |_| | | |  __/ (_) \ V /| | (_| |  __/
--  \__|_| |_|\___|\___/ \_/ |_|\__,_|\___|
-- Theo's Neovide variables and settings

-- Colorschemes' "transparency" is essentially turning off background highlight and using terminal's background
-- Since Neovide's default background is plain black, do not invoke transparency via colorschemes, set it using
-- `vim.g.neovide_opacity` variable
vim.g.have_transparent_bg = false

-- I prefer to set font in $XDG_CONFIG_HOME/neovide/config.toml
--vim.o.guifont = "UbuntuMono Nerd Font:h16"

local padding = 10
vim.g.neovide_padding_top = padding
vim.g.neovide_padding_bottom = padding
vim.g.neovide_padding_right = padding
vim.g.neovide_padding_left = padding

vim.g.neovide_window_blurred = true
local blur_radius = 2.4
vim.g.neovide_floating_blur_amount_x = blur_radius
vim.g.neovide_floating_blur_amount_y = blur_radius
vim.g.neovide_floating_shadow = true

local opacity = 0.85
vim.g.neovide_opacity = opacity
vim.g.neovide_normal_opacity = opacity

-- Because I use opt/meta as a modifer for Aerospace,
-- I use left opt key for Aerospace and the right one for terminal input
vim.g.neovide_input_macos_option_key_is_meta = "only_right"

vim.g.neovide_cursor_vfx_mode = "pixiedust"

-- Keymaps
vim.keymap.set({ "n", "i", "c", "v", "o", "t", "l" }, "<D-q>", "<NOP>")
vim.keymap.set("n", "<D-v>", '"+P')         -- Paste normal mode
vim.keymap.set("v", "<D-v>", '"+P')         -- Paste visual mode
vim.keymap.set("c", "<D-v>", "<C-R>+")      -- Paste command mode
vim.keymap.set("i", "<D-v>", '<ESC>"+pa')   -- Paste insert mode
vim.keymap.set("n", "<C-S-v>", '"+P')       -- Paste normal mode
vim.keymap.set("v", "<C-S-v>", '"+P')       -- Paste visual mode
vim.keymap.set("c", "<C-S-v>", "<C-R>+")    -- Paste command mode
vim.keymap.set("i", "<C-S-v>", '<ESC>"+pa') -- Paste insert mode

-- Changing scale factor with <CMD> + -/=/0
vim.g.neovide_scale_factor = 1.0
local change_scale_factor = function(delta)
  vim.g.neovide_scale_factor = vim.g.neovide_scale_factor * delta
end
-- macOS (opt)
vim.keymap.set("n", "<D-=>", function()
  change_scale_factor(1.25)
end)
vim.keymap.set("n", "<D--", function()
  change_scale_factor(1 / 1.25)
end)
vim.keymap.set("n", "<D-0>", function()
  vim.g.neovide_scale_factor = 1.0
end)
-- Linux (alt)
vim.keymap.set("n", "<M-=>", function()
  change_scale_factor(1.25)
end)
vim.keymap.set("n", "<M--", function()
  change_scale_factor(1 / 1.25)
end)
vim.keymap.set("n", "<M-0>", function()
  vim.g.neovide_scale_factor = 1.0
end)
