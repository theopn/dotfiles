--- plugins/telescope.lua
--- $ figlet -f fuzzy theovim
---  .-. .-.                      _
--- .' `.: :                     :_;
--- `. .': `-.  .--.  .--. .-..-..-.,-.,-.,-.
---  : : : .. :' '_.'' .; :: `; :: :: ,. ,. :
---  :_; :_;:_;`.__.'`.__.'`.__.':_;:_;:_;:_;
---
--- Telescope settings
---@require ripgrep: https://github.com/BurntSushi/ripgrep
---@require C compiler
---@require Make

local M = { "nvim-telescope/telescope.nvim" }

M.event = "VimEnter"

M.branch = "0.1.x"

M.dependencies = {
  "nvim-lua/plenary.nvim",                      --> Lua function library for Neovim
  {
    "nvim-telescope/telescope-fzf-native.nvim", --> Natively compiled C version of fzf for better sorting perf
    build = "make",
    cond = function()
      return vim.fn.executable("make") == 1 --> only build when `make` is available
    end,
  },
  "nvim-telescope/telescope-ui-select.nvim",    --> replace vim.ui.select with Telescope
  "nvim-telescope/telescope-file-browser.nvim", --> File browser extension
}

M.config = function()
  local telescope = require("telescope")

  telescope.setup({
    defaults = {
      mappings = {
        i = {
          ["<C-j>"] = "move_selection_next",
          ["<C-k>"] = "move_selection_previous",
        },
      },
    },
    extensions = {
      ["ui-select"] = {
        require("telescope.themes").get_dropdown()
      },
      file_browser = { hidden = true }
    },
  })

  -- Load extension after telescope is initialized
  pcall(telescope.load_extension, "fzf")
  pcall(telescope.load_extension, "ui-select")
  pcall(telescope.load_extension, "file_browser")

  -- Set [S]earch keymaps
  local builtin = require("telescope.builtin")
  vim.keymap.set("n", "<leader>sh", builtin.help_tags, { desc = "[S]earch [H]elp" })
  vim.keymap.set("n", "<leader>sk", builtin.keymaps, { desc = "[S]earch [K]eymaps" })
  vim.keymap.set("n", "<leader>sf", builtin.find_files, { desc = "[S]earch [F]iles" })
  vim.keymap.set("n", "<leader>ss", builtin.builtin, { desc = "[S]earch [S]elect Telescope" })
  vim.keymap.set("n", "<leader>sw", builtin.grep_string, { desc = "[S]earch current [W]ord" })
  vim.keymap.set("n", "<leader>sg", builtin.live_grep, { desc = "[S]earch by [G]rep" })
  vim.keymap.set("n", "<leader>sd", builtin.diagnostics, { desc = "[S]earch [D]iagnostics" })
  vim.keymap.set("n", "<leader>sr", builtin.resume, { desc = "[S]earch [R]esume" })
  vim.keymap.set("n", "<leader>s.", builtin.oldfiles, { desc = "[S]earch Recent Files ('.' for repeat)" })
  vim.keymap.set("n", "<leader><leader>", builtin.buffers, { desc = "[ ] Find existing buffers" })

  vim.keymap.set("n", "<leader>/", function()
    builtin.current_buffer_fuzzy_find(require("telescope.themes").get_dropdown({
      winblend = 10,
      previewer = false,
    }))
  end, { desc = "[/] Fuzzily search in current buffer" })

  vim.keymap.set("n", "<leader>s/", function()
    builtin.live_grep({
      grep_open_files = true,
      prompt_title = "Live Grep in Open Files",
    })
  end, { desc = "[S]earch [/] in Open Files" })

  vim.keymap.set("n", "<leader>sn", function()
    builtin.find_files({ cwd = vim.fn.stdpath("config") })
  end, { desc = "[S]earch [N]eovim files" })

  -- Set Git keymaps
  vim.keymap.set("n", "<leader>gf", builtin.git_files, { desc = "Search [G]it [F]iles" })
  vim.keymap.set("n", "<leader>gc", builtin.git_commits, { desc = "Search [G]it [C]ommits" })
  vim.keymap.set("n", "<leader>gs", builtin.git_status, { desc = "Search [G]it [S]tatus" })

  -- Make keymap for the file browser extension
  vim.keymap.set("n", "<leader>fb", telescope.extensions.file_browser.file_browser, { desc = "[F]ile [B]rowser" })
end

return M
