--- wezterm.lua
--- $ figlet -f small Wezterm
--- __      __      _
--- \ \    / /__ __| |_ ___ _ _ _ __
---  \ \/\/ / -_)_ /  _/ -_) '_| '  \
---   \_/\_/\___/__|\__\___|_| |_|_|_|
---
--- My Wezterm config file

local wezterm = require("wezterm")
local act = wezterm.action

local fish_path = "/opt/homebrew/bin/fish"

local config = {}
-- Use config builder object if possible
if wezterm.config_builder then config = wezterm.config_builder() end

-- Settings
config.default_prog = { fish_path, "-l" }

--config.color_scheme = "Tokyo Night"
config.color_scheme = "nordfox"
local custom_colors = {
  red = "#D06F79",     --> Tokyonight: "#F7768e"
  cyan = "#88C0D0",    --> Tokyonight: "#7DCFFF"
  magenta = "#B48EAD", --> Tokyonight: "#BB9AF7"
  yellow = "#EBCB8B",  --> Tokyonight: "#E0AF68"

}
config.font = wezterm.font_with_fallback({
  { family = "UbuntuMono Nerd Font", scale = 1.35, },
})
config.window_background_opacity = 0.9
config.macos_window_background_blur = 24
config.window_decorations = "RESIZE"
config.window_close_confirmation = "AlwaysPrompt"
config.scrollback_lines = 3000
config.default_workspace = "main"

-- Dim inactive panes
config.inactive_pane_hsb = {
  saturation = 0.24,
  brightness = 0.5
}

-- Keys
config.leader = { key = "a", mods = "CTRL", timeout_milliseconds = 1000 }
config.keys = {
  -- Send C-a when pressing C-a twice
  { key = "a", mods = "LEADER|CTRL", action = act.SendKey { key = "a", mods = "CTRL" } },
  { key = "[", mods = "LEADER",      action = act.ActivateCopyMode },
  { key = ":", mods = "LEADER",      action = act.ActivateCommandPalette },

  -- Workspace (similar to session in Tmux)
  { key = "s", mods = "LEADER",      action = act.ShowLauncherArgs { flags = "FUZZY|WORKSPACES" } },

  -- Tab (similar to window in Tmux)
  { key = "w", mods = "LEADER",      action = act.ShowTabNavigator },
  { key = "c", mods = "LEADER",      action = act.SpawnTab("CurrentPaneDomain") },
  { key = "p", mods = "LEADER",      action = act.ActivateTabRelative(-1) },
  { key = "n", mods = "LEADER",      action = act.ActivateTabRelative(1) },
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
  -- Key table for moving tabs around
  { key = ".",          mods = "LEADER", action = act.ActivateKeyTable { name = "move_tab", one_shot = false } },
  -- Or shortcuts to move tab w/o move_tab table. SHIFT is for when caps lock is on
  --{ key = "{", mods = "LEADER|SHIFT", action = act.MoveTabRelative(-1) },
  --{ key = "}", mods = "LEADER|SHIFT", action = act.MoveTabRelative(1) },

  -- Pane
  { key = "\"",         mods = "LEADER", action = act.SplitVertical { domain = "CurrentPaneDomain" } },
  { key = "%",          mods = "LEADER", action = act.SplitHorizontal { domain = "CurrentPaneDomain" } },
  { key = "h",          mods = "LEADER", action = act.ActivatePaneDirection("Left") },
  { key = "j",          mods = "LEADER", action = act.ActivatePaneDirection("Down") },
  { key = "k",          mods = "LEADER", action = act.ActivatePaneDirection("Up") },
  { key = "l",          mods = "LEADER", action = act.ActivatePaneDirection("Right") },
  { key = "phys:Space", mods = "LEADER", action = act.RotatePanes "Clockwise" },
  { key = "z",          mods = "LEADER", action = act.TogglePaneZoomState },
  { key = "x",          mods = "LEADER", action = act.CloseCurrentPane { confirm = true } },
  {
    key = '!',
    mods = 'LEADER | SHIFT',
    action = wezterm.action_callback(function(win, pane)
      local tab, window = pane:move_to_new_tab()
    end),
  },
  -- We can make separate keybindings for resizing panes
  -- But Wezterm offers custom "mode" in the name of "KeyTable"
  { key = "r", mods = "LEADER", action = act.ActivateKeyTable { name = "resize_pane", one_shot = false } },
}

-- I can use the tab navigator (LDR t), but I also want to quickly navigate tabs with index
for i = 1, 9 do
  table.insert(config.keys, {
    key = tostring(i),
    mods = "LEADER",
    action = act.ActivateTab(i - 1)
  })
end

config.key_tables = {
  resize_pane = {
    { key = "<",      action = act.AdjustPaneSize { "Left", 1 } },
    { key = "-",      action = act.AdjustPaneSize { "Down", 1 } },
    { key = "+",      action = act.AdjustPaneSize { "Up", 1 } },
    { key = ">",      action = act.AdjustPaneSize { "Right", 1 } },
    { key = "Escape", action = "PopKeyTable" },
    { key = "Enter",  action = "PopKeyTable" },
  },
  move_tab = {
    { key = "h",      action = act.MoveTabRelative(-1) },
    { key = "j",      action = act.MoveTabRelative(-1) },
    { key = "k",      action = act.MoveTabRelative(1) },
    { key = "l",      action = act.MoveTabRelative(1) },
    { key = "Escape", action = "PopKeyTable" },
    { key = "Enter",  action = "PopKeyTable" },
  }
}

-- Tab bar
-- I don't like the look of "fancy" tab bar
config.use_fancy_tab_bar = false
config.status_update_interval = 1000
config.tab_bar_at_bottom = true
wezterm.on("update-status", function(window, pane)
  -- Workspace name
  local stat = window:active_workspace()
  local stat_color = custom_colors.red
  -- It's a little silly to have workspace name all the time
  -- Utilize this to display LDR or current key table name
  if window:active_key_table() then
    stat = window:active_key_table()
    stat_color = custom_colors.cyan
  end
  if window:leader_is_active() then
    stat = "LDR"
    stat_color = custom_colors.magenta
  end

  local basename = function(s)
    -- Nothing a little regex can't fix
    return string.gsub(s, "(.*[/\\])(.*)", "%2")
  end

  -- Current working directory
  local cwd = pane:get_current_working_dir()
  if cwd then
    cwd = basename(cwd.file_path) --> URL object introduced in 20240127-113634-bbcac864 (type(cwd) == "userdata")
    -- cwd = basename(cwd) --> 20230712-072601-f4abf8fd or earlier version
  else
    cwd = ""
  end

  -- Current command
  local cmd = pane:get_foreground_process_name()
  -- CWD and CMD could be nil (e.g. viewing log using Ctrl-Alt-l)
  cmd = cmd and basename(cmd) or ""

  -- Left status (left of the tab line)
  window:set_left_status(wezterm.format({
    { Foreground = { Color = stat_color } },
    { Text = "  " },
    { Text = wezterm.nerdfonts.oct_table .. "  " .. stat },
    { Text = " |" },
  }))

  -- Right status
  window:set_right_status(wezterm.format({
    -- Wezterm has a built-in nerd fonts
    -- https://wezfurlong.org/wezterm/config/lua/wezterm/nerdfonts.html
    { Text = wezterm.nerdfonts.md_folder .. "  " .. cwd },
    { Text = " | " },
    { Foreground = { Color = custom_colors.yellow } },
    { Text = wezterm.nerdfonts.fa_code .. "  " .. cmd },
    { Text = "  " },
  }))
end)

--[[ Appearance setting for when I need to take pretty screenshots
config.enable_tab_bar = false
config.window_padding = {
  left = '0.5cell',
  right = '0.5cell',
  top = '0.5cell',
  bottom = '0cell',

}
--]]

return config
