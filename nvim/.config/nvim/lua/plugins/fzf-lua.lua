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
  vim.keymap.set("n", "<leader><leader>", fzf.buffers, { desc = "[ ] Search buffers" })
  vim.keymap.set("n", "<leader>.", fzf.oldfiles, { desc = "[.] Search oldfiles (dot repeat)" })
  vim.keymap.set("n", "<leader>sf", fzf.files, { desc = "[S]earch [F]iles" })
  vim.keymap.set("n", "<leader>s.", function()
    -- process CWD
    local cwd = vim.uv.cwd()
    cwd = vim.fs.normalize(cwd)

    -- validate the path
    local stat = vim.uv.fs_stat(cwd)
    if not stat or stat.type ~= "directory" then
      print("Invalid directory!")
      return
    end

    -- Given the path, fill the dirs table with parant directories
    -- For example, if path = "/Users/someone/dotfiles/nvim"
    -- then dirs = { "/", "/Users", "/Users/someone", "/Users/someone/dotfiles" }
    local dirs = {}
    for dir in vim.fs.parents(cwd) do
      table.insert(dirs, dir)
    end

    -- Open a custom fzf to select a directory and launch fzf-files
    fzf.fzf_exec(dirs, {
      prompt = "Parent Directories❯ ",
      actions = {
        ["default"] = function(selected)
          fzf.files({ cwd = selected[1] })
        end
      }
    })
  end, { desc = "[S]earch Parent Directories [..]" })

  -- Finding a word
  vim.keymap.set("n", "<leader>/", fzf.blines, { desc = "[/] Search words in the buffer" })
  vim.keymap.set("n", "<leader>sg", fzf.live_grep, { desc = "[S]earch by Live rip[G]rep (current directory)" })

  -- Others
  vim.keymap.set("n", "<leader>sh", fzf.command_history, { desc = "[S]earch Command [H]istory" })
  vim.keymap.set("n", "<leader>ss", fzf.builtin, { desc = "[S]earch [S]earch (builtin)" })
  vim.keymap.set("n", "<leader>sr", fzf.resume, { desc = "[S]earch [R]esume" })

  -- Git
  vim.keymap.set("n", "<leader>gc", fzf.git_commits, { desc = "Search [G]it [C]ommits" })
  vim.keymap.set("n", "<leader>gs", fzf.git_status, { desc = "Search [G]it [S]tatus" })

  -- Override Vim defaults
  vim.keymap.set({ "n", "i" }, "<C-x><C-f>",
    function()
      fzf.complete_file({
        cmd = "fd --hidden",
        winopts = { preview = { hidden = false } }
      })
    end, { silent = true, desc = "Fuzzy complete file" })

  fzf.register_ui_select() --> register fzf-lua as the UI interface for vim.ui.select
end

return M
