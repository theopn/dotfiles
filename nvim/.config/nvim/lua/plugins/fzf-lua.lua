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
    -- https://github.com/ibhagwan/fzf-lua/discussions/1488#discussioncomment-11727981
    grep = {
      -- default flags (scroll down in :h fzf-lua-customization) + --hidden
      -- since -e is a flag for supplying a pattern, it must be located in the end
      rg_opts = "--column --line-number --no-heading --color=always --smart-case --max-columns=4096 --hidden -e",
      actions = {
        ["ctrl-a"] = {
          fn = function(_, opts)
            fzf.actions.toggle_flag(
              _,
              vim.tbl_extend("force", opts, {
                toggle_flag = "--smart-case",
              })
            )
          end,
          desc = "toggle-flags",
          header = function(o)
            local flag = o.toggle_smart_case_flag or "--smart-case"
            if o.cmd and o.cmd:match(fzf.utils.lua_regex_escape(flag)) then
              return "Disable smart case"
            else
              return "Enable smart case"
            end
          end,
        },
      },
    },
  })

  -- Finding a file
  vim.keymap.set("n", "<leader><leader>", fzf.buffers, { desc = "[ ] Search buffers" })
  vim.keymap.set("n", "<leader>.", fzf.oldfiles, { desc = "[.] Search oldfiles (dot repeat)" })
  vim.keymap.set("n", "<leader>sf", fzf.files, { desc = "[S]earch [F]iles" })

  -- Finding a word
  vim.keymap.set("n", "<leader>/", fzf.blines, { desc = "[/] Search words in the buffer" })
  vim.keymap.set("n", "<leader>sg", fzf.live_grep, { desc = "[S]earch by Live rip[G]rep (current directory)" })

  -- Navigation
  vim.keymap.set("n", "<leader>sj", fzf.jumps, { desc = "[S]earch [J]umplist" })
  vim.keymap.set("n", "<leader>st", fzf.tabs, { desc = "[S]earch [T]abs" })

  -- Others
  vim.keymap.set("n", "<leader>sh", fzf.command_history, { desc = "[S]earch Command [H]istory" })
  vim.keymap.set("n", "<leader>sc", fzf.colorschemes, { desc = "[S]earch [C]olorschemes" })
  vim.keymap.set("n", "<leader>ss", fzf.builtin, { desc = "[S]earch [S]earch (builtin)" })
  vim.keymap.set("n", "<leader>sr", fzf.resume, { desc = "[S]earch [R]esume" })

  -- Git
  vim.keymap.set("n", "<leader>gc", fzf.git_commits, { desc = "Search [G]it [C]ommits" })
  vim.keymap.set("n", "<leader>gs", fzf.git_status, { desc = "Search [G]it [S]tatus" })

  -- Directory related custom functions
  vim.keymap.set("n", "<leader>d-", function()
    -- Fill the table with parent directories
    local dirs = {}
    for dir in vim.fs.parents(vim.uv.cwd()) do
      dirs[#dirs + 1] = dir
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
  end, { desc = "Select & Search Parent([-]) [D]irectories" })

  vim.keymap.set("n", "<leader>di", function()
    vim.ui.input({
      prompt = "Enter a directory: ",
      completion = "dir",
    }, function(input)
      if input then
        local dir = vim.fs.normalize(input)
        local stat = vim.uv.fs_stat(dir)
        if stat and stat.type == "directory" then
          fzf.files({ cwd = dir })
        else
          print("Invalid directory!")
        end
      end
    end)
  end, { desc = "Search the [D]irectory of your [I]nput" })

  vim.keymap.set("n", "<leader>dc", function()
    fzf.fzf_exec("fd --hidden --type d", {
      prompt = "Directories> ",
      actions = {
        ["default"] = function(selected)
          vim.cmd("cd " .. selected[1])
        end,
      }
    })
  end, { desc = "Search and [C]hange CW[D]" })

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
