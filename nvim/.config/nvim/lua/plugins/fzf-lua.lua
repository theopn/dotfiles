--- $ figlet -f fuzzy theovim
---  .-. .-.                      _
--- .' `.: :                     :_;
--- `. .': `-.  .--.  .--. .-..-..-.,-.,-.,-.
---  : : : .. :' '_.'' .; :: `; :: :: ,. ,. :
---  :_; :_;:_;`.__.'`.__.'`.__.':_;:_;:_;:_;
---

local M = { "ibhagwan/fzf-lua" }

M.config = function()
  local fzf = require("fzf-lua")

  fzf.setup({
    keymap = {
      builtin = {
        ["<C-b>"] = "preview-page-down",
        ["<C-f>"] = "preview-page-up",
      },
    },
  })

  -- Finding a file
  vim.keymap.set("n", "<leader>sf", fzf.files, { desc = "[S]earch [F]iles" })
  vim.keymap.set("n", "<leader>s.", function() fzf.files({ cwd = ".." }) end, { desc = "[S]earch the Parent Dir [..]" })
  vim.keymap.set("n", "<leader>so", fzf.oldfiles, { desc = "[S]earch [O]ldfiles" })
  vim.keymap.set("n", "<leader><leader>", fzf.buffers, { desc = "[ ] Find existing buffers" })

  -- Finding a word
  vim.keymap.set("n", "<leader>sg", fzf.live_grep, { desc = "[S]earch by Live [G]rep (current directory)" })
  vim.keymap.set("n", "<leader>/", fzf.blines, { desc = "[/] Fuzzily search in current buffer" })

  -- Others
  vim.keymap.set("n", "<leader>sh", fzf.command_history, { desc = "[S]earch Command [H]istory" })
  vim.keymap.set("n", "<leader>ss", fzf.builtin, { desc = "[S]earch [S]earch" })
  vim.keymap.set("n", "<leader>sr", fzf.resume, { desc = "[S]earch [R]esume" })

  -- Git
  vim.keymap.set("n", "<leader>gc", fzf.git_commits, { desc = "[G]it [C]ommits" })
  vim.keymap.set("n", "<leader>gs", fzf.git_status, { desc = "[G]it [S]tatus" })

  -- Override Vim defaults
  vim.keymap.set({ "n", "v", "i" }, "<C-x><C-f>",
    function() fzf.complete_path() end,
    { silent = true, desc = "Fuzzy complete path" })
  fzf.register_ui_select() --> register fzf-lua as the UI interface for vim.ui.select
end

return M
