--[[
 ________              _  __     _         ___
/_  __/ /  ___ ___    / |/ /  __(_)_ _    / _ | ___  ___  ___ ___ ________ ____  _______
 / / / _ \/ -_) _ \  /    / |/ / /  ' \  / __ |/ _ \/ _ \/ -_) _ `/ __/ _ `/ _ \/ __/ -_)
/_/ /_//_/\__/\___/ /_/|_/|___/_/_/_/_/ /_/ |_/ .__/ .__/\__/\_,_/_/  \_,_/_//_/\__/\__/
                                             /_/  /_/
--]]

-- {{{ Theme Settings
require("onedark").setup({
  style = "cool",
  transparent = false,
  toggle_style_key = "<leader>od",
  toggle_style_list = {"dark", "darker", "cool", "deep", "warm", "warmer"},
  code_style = {
    comments = "italic",
    keywords = "bold",
    functions = 'none',
    strings = 'none',
    variables = 'none'
  },
})
require("onedark").load()
-- }}}

-- {{{ Lualine (Status bar) Settings
require("lualine").setup({
  selection = {
  },
})
-- }}}

-- {{{ Barbar (Tab bar) Settings
vim.api.nvim_set_keymap('n', "<C-,>", ":BufferPrevious<CR>", { noremap = true, silent = true } )
vim.g.bufferline = { icons = true, maximum_padding = 1,
  maximum_length = 30,
  icon_separator_active = '▎',
  icon_separator_inactive = '▎',
  icon_close_tab = '',
  icon_close_tab_modified = '●',
  icon_pinned = '車',
  no_name_title = "New Tab"
}
-- Compitability w/ nvim-tree --
require("nvim-tree.events").on_tree_open(function ()
  require("bufferline.state").set_offset(31, "File Tree")
end)
require("nvim-tree.events").on_tree_close(function ()
  require("bufferline.state").set_offset(0)
end)
-- }}}

-- {{{ Vim Pets Settings
vim.g["pets_garden_width"] = 25
vim.g["pets_garden_height"] = 10
vim.g["pets_default_pet"] = "cat"
-- }}}

