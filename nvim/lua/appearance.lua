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
  transparent = true,
  toggle_style_key = "<leader>od",
  toggle_style_list = { "dark", "darker", "cool", "deep", "warm", "warmer" },
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
-- Emacs Doom Mode line style, heavily inspired examples/evil_lualine.lua in the plug-in repository
local lualine = require('lualine')

local colors = {
  bg       = '#202328',
  fg       = '#bbc2cf',
  yellow   = '#ECBE7B',
  cyan     = '#008080',
  darkblue = '#081633',
  green    = '#98be65',
  orange   = '#FF8800',
  violet   = '#a9a1e1',
  magenta  = '#c678dd',
  blue     = '#51afef',
  red      = '#ec5f67',
}

local conditions = {
  buffer_not_empty = function()
    return vim.fn.empty(vim.fn.expand('%:t')) ~= 1
  end,
  hide_in_width = function()
    return vim.fn.winwidth(0) > 80
  end,
  check_git_workspace = function()
    local filepath = vim.fn.expand('%:p:h')
    local gitdir = vim.fn.finddir('.git', filepath .. ';')
    return gitdir and #gitdir > 0 and #gitdir < #filepath
  end,
}

-- Default Lualine config table to remove defaults
local config = {
  options = {
    component_separators = '',
    section_separators = '',
  },
  sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_y = {},
    lualine_z = {},
    lualine_c = {},
    lualine_x = {},
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_y = {},
    lualine_z = {},
    lualine_c = { { 'filename', file_status = true, path = 2 } }, --> 0 (default) file name, 1 relative path, 2 abs path
    lualine_x = {
      { 'diagnostics',
        sources = { 'nvim_diagnostic' },
        symbols = { error = ' ', warn = ' ', info = ' ', hint = ' ' },
      },
    },
  },
}

local function ins_left(component)
  table.insert(config.sections.lualine_c, component)
end

local function ins_right(component)
  table.insert(config.sections.lualine_x, component)
end

ins_left {
  function()
    return '▊'
  end,
  color = { fg = colors.blue },
  padding = { left = 0, right = 1 },
}

ins_left {
  -- mode component
  function()
    return ' ' .. vim.fn.mode()
  end,
  color = function()
    local mode_color = {
      n = colors.red,
      i = colors.green,
      v = colors.blue,
      [''] = colors.blue,
      V = colors.blue,
      c = colors.magenta,
      no = colors.red,
      s = colors.orange,
      S = colors.orange,
      [''] = colors.orange,
      ic = colors.yellow,
      R = colors.violet,
      Rv = colors.violet,
      cv = colors.red,
      ce = colors.red,
      r = colors.cyan,
      rm = colors.cyan,
      ['r?'] = colors.cyan,
      ['!'] = colors.red,
      t = colors.red,
    }
    return { fg = mode_color[vim.fn.mode()] }
  end,
  padding = { right = 1 },
}

ins_left {
  'branch',
  icon = '',
  color = { fg = colors.violet, gui = 'bold' },
}

ins_left {
  'diff',
  -- Is it me or the symbol for modified us really weird
  symbols = { added = ' ', modified = ' ', removed = ' ' },
  diff_color = {
    added = { fg = colors.green },
    modified = { fg = colors.orange },
    removed = { fg = colors.red },
  },
  cond = conditions.hide_in_width,
}

ins_left {
  'filename',
  cond = conditions.buffer_not_empty,
  color = { fg = colors.magenta, gui = 'bold' },
}

ins_left {
  -- filesize component
  'filesize',
  cond = conditions.buffer_not_empty,
}

-- Insert mid section. You can make any number of sections in neovim :)
-- for lualine it's any number greater then 2
ins_left {
  function()
    return '%='
  end,
}

ins_left {
  -- Lsp server name .
  function()
    local msg = 'No Active Lsp'
    local buf_ft = vim.api.nvim_buf_get_option(0, 'filetype')
    local clients = vim.lsp.get_active_clients()
    if next(clients) == nil then
      return msg
    end
    for _, client in ipairs(clients) do
      local filetypes = client.config.filetypes
      if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
        return client.name
      end
    end
    return msg
  end,
  icon = ' LSP:',
  color = { fg = '#ffffff', gui = 'bold' },
}

ins_left {
  'diagnostics',
  sources = { 'nvim_diagnostic' },
  symbols = { error = ' ', warn = ' ', info = ' ', hint = ' ' },
  diagnostics_color = {
    color_error = { fg = colors.red },
    color_warn = { fg = colors.yellow },
    color_info = { fg = colors.cyan },
  },
}

-- Add components to right sections
ins_right {
  'o:encoding', -- option component same as &encoding in viml
  fmt = string.upper, -- I'm not sure why it's upper case either ;)
  cond = conditions.hide_in_width,
  color = { fg = colors.green, gui = 'bold' },
}

ins_right {
  'fileformat',
  fmt = string.upper,
  icons_enabled = false, -- I think icons are cool but Eviline doesn't have them. sigh
  color = { fg = colors.green, gui = 'bold' },
}

ins_right { 'location' }

ins_right { 'progress', color = { fg = colors.fg, gui = 'bold' } }

ins_right {
  function()
    return '▊'
  end,
  color = { fg = colors.blue },
  padding = { left = 1 },
}

lualine.setup(config)
-- }}}

-- {{{ Barbar (Tab bar) Settings
vim.api.nvim_set_keymap('n', "<C-,>", ":BufferPrevious<CR>", { noremap = true, silent = true })
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
require("nvim-tree.events").on_tree_open(function()
  require("bufferline.state").set_offset(31, "File Tree")
end)
require("nvim-tree.events").on_tree_close(function()
  require("bufferline.state").set_offset(0)
end)
-- }}}

-- {{{ Dashboard Settings
local db = require("dashboard")
db.custom_header = {
  "",
  "",
  "",
  "            __..--''``---....___   _..._    __               ",
  "  /// //_.-'    .-/';  `        ``<._  ``.''_ `. / // /      ",
  " ///_.-' _..--.'_    ;                    `( ) ) // //       ",
  " / (_..-' // (< _     ;_..__               ; `' / ///        ",
  "  / // // //  `-._,_)' // / ``--...____..-' /// / //         ",
  "",
  "",
  ",--------.,--.                            ,--.               ",
  "'--.  .--'|  ,---.  ,---.  ,---.,--.  ,--.`--',--,--,--.     ",
  "   |  |   |  .-.  || .-. :| .-. |\\  `'  / ,--.|        |     ",
  "   |  |   |  | |  |\\   --.' '-' ' \\    /  |  ||  |  |  |     ",
  "   `--'   `--' `--' `----' `---'   `--'   `--'`--`--`--'     ",
  "",
  "",
  "",
}
db.custom_center = {
  {
    icon = "  ",
    desc = "Find File           ",
    action = "Telescope find_files",
    shortcut = "SPC ff",
  },
  {
    icon = "  ",
    desc = "Browse Files        ",
    action = "Telescope file_browser",
    shortcut = "SPC fb",
  },
  {
    icon = "  ",
    desc = "New File             ",
    action = "DashboardNewFile",
    shortcut = "SPC t",
  },
  {
    icon = "  ",
    desc = "Configure Dotfiles        ",
    action = "edit ~/dotfiles/",
  },
  {
    icon = "  ",
    desc = "Exit Neovim               ",
    action = "quit",
  },
}
-- }}}

-- {{{ Notification Settings
require("notify").setup()
vim.notify = require("notify")
-- }
