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
  sections = {
    lualine_a = {'mode'},
    lualine_b = {'branch', 'diff', 'diagnostics'},
    lualine_c = { 'filename', { 'filename', path = 2 } }, --> 0 (default) file name, 1 relative path, 2 abs path
    lualine_x = {'encoding', 'fileformat', 'filetype'},
    lualine_y = {'progress'},
    lualine_z = {'location'}
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

-- {{{ Dashboard Settings
local db = require("dashboard")
db.custom_header = {
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "████████╗██╗  ██╗███████╗ ██████╗ ██╗   ██╗██╗███╗   ███╗",
  "╚══██╔══╝██║  ██║██╔════╝██╔═══██╗██║   ██║██║████╗ ████║",
  "   ██║   ███████║█████╗  ██║   ██║██║   ██║██║██╔████╔██║",
  "   ██║   ██╔══██║██╔══╝  ██║   ██║╚██╗ ██╔╝██║██║╚██╔╝██║",
  "   ██║   ██║  ██║███████╗╚██████╔╝ ╚████╔╝ ██║██║ ╚═╝ ██║",
  "   ╚═╝   ╚═╝  ╚═╝╚══════╝ ╚═════╝   ╚═══╝  ╚═╝╚═╝     ╚═╝",
  "",
  "",
  "",
  "",
}
db.custom_center = {
  {
    icon = "  ",
    desc = "Browse Files        ",
    action = "Telescope file_browser",
    shortcut = "SPC n",
  },
  {
    icon = "  ",
    desc = "New File            ",
    action = "DashboardNewFile",
    shortcut = "SPC t",
  },
  {
    icon = "  ",
    desc = "Find File          ",
    action = "Telescope find_files",
    shortcut = "SPC ff",
  },
  {
    icon = "  ",
    desc = "Configure Dotfiles       ",
    action = "edit ~/dotfiles/",
  },
  {
    icon = "  ",
    desc = "Exit Neovim              ",
    action = "quit",
  },
}
-- }}}

-- {{{ Vim Pets Settings
vim.g["pets_garden_width"] = 25
vim.g["pets_garden_height"] = 10
vim.g["pets_default_pet"] = "cat"
-- }}}

