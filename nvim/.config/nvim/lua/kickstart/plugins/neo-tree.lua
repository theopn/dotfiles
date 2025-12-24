-- Neo-tree is a Neovim plugin to browse the file system
-- https://github.com/nvim-neo-tree/neo-tree.nvim

---@module 'lazy'
---@type LazySpec
return {
  'nvim-neo-tree/neo-tree.nvim',
  version = '*',
  dependencies = {
    'nvim-lua/plenary.nvim',
    'nvim-tree/nvim-web-devicons', -- not strictly required, but recommended
    'MunifTanjim/nui.nvim',
  },
  lazy = false,
  keys = {
    { '\\', ':Neotree reveal<CR>', desc = 'NeoTree reveal', silent = true },
  },
  ---@module 'neo-tree'
  ---@type neotree.Config
  opts = {
    window = {
      position = 'left', -- Position of the NeoTree window
      width = 30, -- Width of the NeoTree window
      mappings = {
        ['\\'] = 'close_window',
      },
    },
    filesystem = {
      hijack_netrw_behavior = 'open_default',
      close_if_last_window = true, -- Close NeoTree if it's the last window
      use_libuv_file_watcher = true,
      filtered_items = {
        visible = true, -- Show hidden files by default
        hide_dotfiles = true,
        hide_gitignored = true,
        hide_by_name = {
          'node_modules', -- Hides the folder by name
          '.DS_Store',
          'thumbs.db',
          '.nx',
          '.venv',
        },
        never_show = {
          'node_modules', -- Ensures it's never shown even when toggling
          '.DS_Store',
          'thumbs.db',
          '.nx',
          '.venv',
        },
      },
      follow_current_file = {
        enabled = true,
        leave_dirs_open = true,
      },
    },
  },
}
