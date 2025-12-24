--- plugins/treesitter.lua
--- $ figlet -f threepoint theovim
--- _|_|_  _  _   . _ _
---  | | |(/_(_)\/|| | |
---
--- Configure Neovim built-in Treesitter engine

local M = { "nvim-treesitter/nvim-treesitter" }

M.build = ":TSUpdate"

M.main = "nvim-treesitter.configs"

M.opts = {
  --ensure_installed = { "bash", "c", "cpp", "latex", "lua", "markdown", "python", },
  auto_install = false,

  highlight = {
    enable = true,
    disable = function(lang, buf)
      local max_filesize = 100 * 1024 -- 100 KB
      local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
      if ok and stats and stats.size > max_filesize then
        return true
      end
    end,
  },

  indent = {
    enable = true
  },

  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "<C-space>",
      node_incremental = "<C-space>",
      scope_incremental = "<C-s>",
      node_decremental = "<M-space>",
    },
  },
}

return M
