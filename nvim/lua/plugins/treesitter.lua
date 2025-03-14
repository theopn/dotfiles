--- plugins/treesitter.lua
--- $ figlet -f threepoint theovim
--- _|_|_  _  _   . _ _
---  | | |(/_(_)\/|| | |
---
--- Configure Neovim built-in Treesitter engine

local M = { "nvim-treesitter/nvim-treesitter" }

M.build = ":TSUpdate"

M.opts = {
  ensure_installed = { "bash", "c", "cpp", "latex", "lua", "markdown", "python", },
  auto_install = false,

  highlight = { enable = true, },
  indent = { enable = true },
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

M.config = function(_, opts)
  require("nvim-treesitter.configs").setup(opts)
end

return M
