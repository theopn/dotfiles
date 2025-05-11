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
      org_agenda_files = { caos_dir .. "projects/active/*", caos_dir .. "capture.org" },
      org_default_notes_file = caos_dir .. "capture.org",

      org_todo_keywords = { "TODO", "|", "DONE", "CANC" },
      org_deadline_warning_days = 3,

      org_capture_templates = {
        c = {
          description = "Capture Thoughts",
          template = "* %?"
        },
        i = {
          description = "Task Idea",
          template = "* %? :IDEA:",
        },
        t = {
          description = "Task",
          template = "* TODO %? :IDEA: \n  SCHEDULED: %^t",
        },
      },

      org_agenda_skip_scheduled_if_done = true,
      org_agenda_skip_deadline_if_done = true,
      org_agenda_custom_commands = {
        t = {
          description = "[T]heo's Personal agenda + IDEA (captured tasks)",
          types = {
            {
              -- Use "tags_todo" if you do not want to see DONE or CANC tasks, but I like to view everything since
              -- stuff in capture.org has to be refiled somewhere at one point anyway
              type = "tags",
              --match = 'TODO="IDEA"',  --> use this in case I decide to make IDEA into an org_todo_keywords
              match = "IDEA",
              org_agenda_overriding_header = "IDEA (tasks & task ideas in ~capture.org~)",
            },
            {
              type = "agenda",
            },
          },
        },
      },

      mappings = {
        agenda = {
          org_agenda_later = { "f", "]" },
          org_agenda_earlier = { "b", "[" },
          org_agenda_todo = { "<S-RIGHT>" }, --> Remove the default `t` keybinding
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
