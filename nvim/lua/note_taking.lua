--[[
 ________              _  __     _         _  __     __
/_  __/ /  ___ ___    / |/ /  __(_)_ _    / |/ /__  / /____ ___
 / / / _ \/ -_) _ \  /    / |/ / /  ' \  /    / _ \/ __/ -_|_-<
/_/ /_//_/\__/\___/ /_/|_/|___/_/_/_/_/ /_/|_/\___/\__/\__/___/
--]]

-- {{{ Custom function for quickly opening a file
local notes_directory = "~/Documents/vim_notes/"
function new_note(class_name, note_name)
  date = os.date("%Y-%m-%d_")
  note_name = notes_directory .. class_name .. "/" .. date .. class_name .. "_" .. note_name .. ".md"
  print("Opening " .. note_name .. " ...")
  vim.cmd("e" .. note_name)
  vim.cmd("Pets cat Oliver")
end

-- }}}

-- {{{ Neorg Setttings
require("neorg").setup {
  load = {
    ["core.defaults"] = {},
    ["core.norg.dirman"] = {
      config = {
        workspaces = {
          work = "~/Documents/neorg/",
        }
      }
    },
    ["core.norg.completion"] = {
      config = {
        engine = "nvim-cmp",
      },
    },
  }
}
-- }}}
