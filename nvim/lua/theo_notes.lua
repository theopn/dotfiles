--[[
 _____ _                   _   _       _         _____     _
|_   _| |__   ___  ___    | \ | | ___ | |_ ___  |_   _|_ _| | _____ _ __ 
  | | | '_ \ / _ \/ _ \   |  \| |/ _ \| __/ _ \   | |/ _` | |/ / _ \ '__|
  | | | | | |  __/ (_) |  | |\  | (_) | ||  __/   | | (_| |   <  __/ |
  |_| |_| |_|\___|\___/___|_| \_|\___/ \__\___|___|_|\__,_|_|\_\___|_|
                     |_____|                 |_____|
--]]

-- {{{ Markdown Reader Settings
vim.g["mkdp_browser"] = "firefox"
-- }}}

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
