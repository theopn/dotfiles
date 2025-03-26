-- Default config reference:
-- https://github.com/nvim-orgmode/orgmode/blob/master/lua/orgmode/config/defaults.lua

local caos_dir = "~/My Drive/l1-cache/caos/"

return {
  "nvim-orgmode/orgmode",
  event = "VeryLazy",
  ft = { "org" },
  config = function()
    -- Setup orgmode
    require("orgmode").setup({
      org_agenda_files = { caos_dir .. "projects/active/*", caos_dir .. "capture/refile.org" },
      org_default_notes_file = caos_dir .. "capture/refile.org",

      org_todo_keywords = { "TODO", "|", "DONE", "CANC" },
      org_deadline_warning_days = 3,

      org_capture_templates = {
        t = {
          description = "Task",
          template = "* TODO %?\n  %u",
        },
        c = {
          description = "Capture Idea",
          template = "* "
        }
      },

      org_agenda_skip_scheduled_if_done = true,
      org_agenda_skip_deadline_if_done = true,

      mappings = {
        agenda = {
          org_agenda_later = { "f", "]" },
          org_agenda_earlier = { "b", "[" },
        },
        capture = {
          org_capture_finalize = { "<C-c>", "<C-c><C-c>" },
          org_capture_kill = { "<prefix>k", "<C-c><C-k>" },
        },
        org = {
          org_priority = { "<C-c>,", "<prefix>," },
          org_todo = { "cit", "<S-RIGHT>" },
          org_todo_prev = { "ciT", "<S-LEFT>" },
          org_insert_todo_heading_respect_content = { "<prefix>it", "<C-CR>" }, -- Add new todo heading after current heading block (same level)
          org_deadline = { "<prefix>id", "<C-c><C-d>" },
          org_schedule = { "<prefix>is", "<C-c><C-s>" },
        },
      }
    })
  end,
}
