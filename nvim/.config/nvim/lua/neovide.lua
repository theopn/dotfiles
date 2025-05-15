-- Neovide variables and settings

vim.g.have_transparent_bg = false

--vim.o.guifont = "UbuntuMono Nerd Font:h16"

local padding = 10
vim.g.neovide_padding_top = padding
vim.g.neovide_padding_bottom = padding
vim.g.neovide_padding_right = padding
vim.g.neovide_padding_left = padding

local blur_radius = 2.0
vim.g.neovide_window_blurred = true
vim.g.neovide_floating_blur_amount_x = blur_radius
vim.g.neovide_floating_blur_amount_y = blur_radius
vim.g.neovide_floating_shadow = true

local opacity = 0.8
vim.g.neovide_opacity = opacity
vim.g.neovide_normal_opacity = opacity

-- Because I use 
vim.g.neovide_input_macos_option_key_is_meta = "only_right" --> because I use opt/meta as a modifier for Aerospace

vim.g.neovide_cursor_vfx_mode = "pixiedust"

-- Keymaps
vim.keymap.set({ "n", "i", "c", "v", "o", "t", "l" }, "<D-q>", "<NOP>")
vim.keymap.set("n", "<D-v>", '"+P')         -- Paste normal mode
vim.keymap.set("v", "<D-v>", '"+P')         -- Paste visual mode
vim.keymap.set("c", "<D-v>", "<C-R>+")      -- Paste command mode
vim.keymap.set("i", "<D-v>", '<ESC>l"+Pli') -- Paste insert mode

-- Changing scale factor with <CMD> + -/=/0
vim.g.neovide_scale_factor = 1.0
local change_scale_factor = function(delta)
  vim.g.neovide_scale_factor = vim.g.neovide_scale_factor * delta
end
vim.keymap.set("n", "<D-=>", function()
  change_scale_factor(1.25)
end)
vim.keymap.set("n", "<D-->", function()
  change_scale_factor(1 / 1.25)
end)
vim.keymap.set("n", "<D-0>", function()
  vim.g.neovide_scale_factor = 1.0
end)
