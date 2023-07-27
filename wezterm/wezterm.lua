--[[ wezterm.lua
-- $ figlet -f small Wezterm
-- __      __      _
-- \ \    / /__ __| |_ ___ _ _ _ __
--  \ \/\/ / -_)_ /  _/ -_) '_| '  \
--   \_/\_/\___/__|\__\___|_| |_|_|_|
--
-- My Wezterm config file
--]]
local wezterm = require("wezterm")
local act = require("wezterm").action

local config = {}
if wezterm.config_builder then config = wezterm.config_builder() end

-- {{{ Settings
config.window_close_confirmation = "AlwaysPrompt"
config.default_workspace = "home"
config.tab_and_split_indices_are_zero_based = false
-- }}}

-- {{{ Colors & Appearance
local fav_colorschemes = {
  "Catppuccin Frappe",
  "Dracula",
  "Tokyo Night",
  "Tokyo Night Moon",
}
config.color_scheme = fav_colorschemes[math.random(#fav_colorschemes)]
wezterm.on("window-config-reloaded", function(window, pane)
  if not window:get_config_overrides() then --> if there is no override, time to assign a colorscheme
    local colorscheme = fav_colorschemes[math.random(#fav_colorschemes)]
    window:set_config_overrides { color_scheme = colorscheme, }
  end
end)


config.colors = {
  tab_bar = {
    background = "#282A36",
    active_tab = {
      bg_color = "#FF79C6",
      fg_color = "#F8F8F2",
      italic = false,
    },
    inactive_tab = {
      bg_color = "#282A36",
      fg_color = "#F8F8F2",
      italic = false,
    },
    inactive_tab_hover = {
      bg_color = "#6272A4",
      fg_color = "#F8F8F2",
      italic = true,
    },
  },
}

config.font = wezterm.font_with_fallback({
  { family = "CaskaydiaCove Nerd Font",     scale = 1.24 },
  { family = "FantasqueSansMono Nerd Font", scale = 1.24 },
})

config.window_background_opacity = 0.9
config.window_decorations = "RESIZE"
config.inactive_pane_hsb = {
  saturation = 0.24,
  brightness = 0.5,
}
-- }}}

-- {{{ Tab bar
config.tab_bar_at_bottom = false
config.use_fancy_tab_bar = false
config.show_tabs_in_tab_bar = true
config.show_new_tab_button_in_tab_bar = false

config.status_update_interval = 1000
wezterm.on("update-right-status", function(window, pane)
  -- Workspace name
  local ws = window:active_workspace()
  -- Current window colorscheme
  local colorscheme = config.color_scheme
  local win_override = window:get_config_overrides()
  if win_override and win_override.color_scheme then colorscheme = win_override.color_scheme end

  -- leader or active key table
  local mode = "N"
  local name = window:active_key_table()
  if name then mode = name end
  if window:leader_is_active() then mode = "LDR" end

  -- cwd
  local cwd = pane:get_current_working_dir()
  cwd = cwd:sub(cwd:match("^.*()/")) --> Strip everything but last folder name. Alt: /[^/]*$
  -- current command
  local cmd = pane:get_title()

  -- time
  local time = wezterm.strftime(" %H:%M")

  -- https://wezfurlong.org/wezterm/config/lua/wezterm/nerdfonts.html
  window:set_right_status(wezterm.format({
    { Text = wezterm.nerdfonts.oct_table .. "  " .. ws },
    { Text = " | " },
    { Text = wezterm.nerdfonts.fa_paint_brush .. "  " .. colorscheme },
    { Text = " | " },
    { Text = wezterm.nerdfonts.md_folder .. " " .. cwd },
    { Text = " | " },
    { Foreground = { Color = "#FFB86C" } },
    { Text = wezterm.nerdfonts.fa_code .. " " .. cmd },
    "ResetAttributes",
    { Text = " | " },
    { Text = wezterm.nerdfonts.md_creation .. " Mode: " .. mode },
    { Text = " | " },
    { Text = wezterm.nerdfonts.md_clock_alert .. time },
    "ResetAttributes",
    { Foreground = { Color = "#6272a4" } },
    { Text = " " },
  }))
end)


-- }}}

-- {{{ Launching Programs
--config.default_prog = { "/opt/homebrew/bin/fish", "-l" }
-- }}}

-- {{{ Keys
-- USe `wezterm show-keys --lua` to get alll thedefault keybindings
config.disable_default_key_bindings = false
config.leader = { key = "a", mods = "CTRL", timeout_milliseconds = 1000 }
config.keys = {
  -- Send "CTRL-A" to the terminal when pressing CTRL-A, CTRL-A
  { key = "a", mods = "LEADER|CTRL",  action = act.SendKey { key = "a", mods = "CTRL" }, },
  { key = "c", mods = "LEADER",       action = act.ActivateCopyMode, },
  -- Pane bindings
  { key = "h", mods = "LEADER",       action = act.ActivatePaneDirection("Left"), },
  { key = "j", mods = "LEADER",       action = act.ActivatePaneDirection("Down"), },
  { key = "k", mods = "LEADER",       action = act.ActivatePaneDirection("Up"), },
  { key = "l", mods = "LEADER",       action = act.ActivatePaneDirection("Right"), },
  { key = "|", mods = "LEADER|SHIFT", action = act.SplitHorizontal { domain = "CurrentPaneDomain" }, },
  { key = "-", mods = "LEADER",       action = act.SplitVertical { domain = "CurrentPaneDomain" }, },
  { key = "x", mods = "LEADER",       action = act.CloseCurrentPane { confirm = true } },
  { key = "r", mods = "LEADER",       action = act.ActivateKeyTable { name = "resize_pane", one_shot = false, }, },
  -- Tab bindings
  { key = "t", mods = "LEADER",       action = act.ShowTabNavigator },
  { key = "n", mods = "LEADER",       action = act.SpawnTab("CurrentPaneDomain"), },
  { key = "w", mods = "CMD",          action = act.CloseCurrentTab { confirm = true }, },
  { key = "[", mods = "LEADER",       action = act.ActivateTabRelative(-1) },
  { key = "]", mods = "LEADER",       action = act.ActivateTabRelative(1) },
  { key = "m", mods = "LEADER",       action = act.ActivateKeyTable { name = "move_tab", one_shot = false, }, },
  {
    key = ",",
    mods = "LEADER",
    action = act.PromptInputLine {
      description = wezterm.format {
        { Attribute = { Intensity = "Bold" } },
        { Foreground = { AnsiColor = "Fuchsia" } },
        { Text = "Renaming Tab Title...:" },
      },
      action = wezterm.action_callback(function(window, pane, line)
        if line then
          window:active_tab():set_title(line)
        end
      end)
    }
  },
  { key = "u", mods = "LEADER", action = act.SpawnCommandInNewTab { args = { "top" }, } },
  -- Workspace bindings
  { key = "w", mods = "LEADER", action = act.ShowLauncherArgs { flags = "FUZZY|WORKSPACES", }, },
  { key = "?", mods = "LEADER", action = act.ShowLauncherArgs { flags = "FUZZY|KEY_ASSIGNMENTS", }, },
}
-- Tab navigation
for i = 1, 9 do
  table.insert(config.keys, {
    key = tostring(i),
    mods = "LEADER",
    action = wezterm.action.ActivateTab(i - 1),
  })
end

config.key_tables = {
  resize_pane = {
    { key = "h", action = act.AdjustPaneSize { "Left", 1 } },
    { key = "j", action = act.AdjustPaneSize { "Down", 1 } },
    { key = "k", action = act.AdjustPaneSize { "Up", 1 } },
    { key = "l", action = act.AdjustPaneSize { "Right", 1 } },
    {
      key = "r",
      mods = "LEADER",
      action = "PopKeyTable"
    },
    { key = "Enter",  action = "PopKeyTable" },
    { key = "Escape", action = "PopKeyTable" },
  },
  move_tab = {
    { key = "h", action = act.MoveTabRelative(-1) },
    { key = "j", action = act.MoveTabRelative(-1) },
    { key = "k", action = act.MoveTabRelative(1) },
    { key = "l", action = act.MoveTabRelative(1) },
    {
      key = "m",
      mods = "LEADER",
      action = "PopKeyTable"
    },
    { key = "Enter",  action = "PopKeyTable" },
    { key = "Escape", action = "PopKeyTable" },
  },
}
-- }}}

return config
