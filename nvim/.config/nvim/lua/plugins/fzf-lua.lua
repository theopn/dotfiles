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

  -- Finding a file
  vim.keymap.set("n", "<leader>sf", fzf.files, { desc = "[S]earch [F]iles" })
  vim.keymap.set("n", "<leader>s.", function() fzf.files({ cwd = ".." }) end, { desc = "[S]earch the Parent Dir [..]" })
  vim.keymap.set("n", "<leader>so", fzf.oldfiles, { desc = "[S]earch [O]ldfiles" })
  vim.keymap.set("n", "<leader><leader>", fzf.buffers, { desc = "[ ] Find existing buffers" })

  -- Finding a word
  vim.keymap.set("n", "<leader>sg", fzf.live_grep, { desc = "[S]earch by Live [G]rep (current directory)" })
  vim.keymap.set("n", "<leader>/", fzf.blines, { desc = "[/] Fuzzily search in current buffer" })

  -- Git
  vim.keymap.set("n", "<leader>gc", fzf.git_commits, { desc = "[G]it [C]ommits" })
  vim.keymap.set("n", "<leader>gs", fzf.git_status, { desc = "[G]it [S]tatus" })

  -- Others

  fzf.register_ui_select() --> register fzf-lua as the UI interface for vim.ui.select
end

return M
