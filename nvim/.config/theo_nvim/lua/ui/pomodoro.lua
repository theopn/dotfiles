--- Theo's simple Pomodoro Timer
--- Supports a single instance of pause-able countdown timer per Neovim instance
--- and integration as a tabline module (or Statusline or Winbar if you modify updateTabline())
--- Inspired by cbrgm's countdown.nvim

local M = {}

---------- Enums ----------

local States = {
  NONE = 0,
  RUNNING = 1,
  PAUSED = 2,
  ENDED = 3,
}

local Icons = {
  [States.NONE] = "󰚭",
  [States.RUNNING] = "󰔟",
  [States.PAUSED] = "󱦠",
  [States.ENDED] = "󱦟",
}


---------- Fields ----------

M.timer = nil
M.remainingSec = 0
M.lastFinishedTimestamp = nil
M.state = States.NONE

---------- Helpers ----------

---@param seconds number of seconds to format.
---@return string of the format "HH:MM:SS".
local function formatSeconds(seconds)
  return string.format("%02d:%02d:%02d",
    math.floor(seconds / 3600),
    math.floor((seconds % 3600) / 60),
    seconds % 60
  )
end


---@return string|osdate the current timestamp in the format "HH:MM:SS".
local function getCurrTime()
  return os.date("%H:%M:%S")
end


--- Updates the tabline every 900ms with the choice of API while avoiding |textlock| (E5560)
local function updateTabline()
  vim.defer_fn(function()
    --vim.cmd("redrawtabline")
    vim.api.nvim__redraw({ tabline = true })
  end, 900)
end


---------- Methods ----------

--- Updates the M.remaining_seconds and redraws Tabline
M.callback = function()
  M.remainingSec = M.remainingSec - 1

  if M.remainingSec <= 0 then
    M.lastFinishedTimestamp = getCurrTime()
    M.timer:stop()
    M.timer:close()
    M.timer = nil
    M.state = States.ENDED
    print(string.format("[Pomodoro] Time is up! (@ %s)", M.lastFinishedTimestamp))
  end

  updateTabline()
end


--- Starts a timer assuming none is running yet
--- WARN: it is advised to use `M.restartTimer()` instead of direct calls
---
---@param seconds number? of seconds to start the timer, should already be processed to a positive integer
M.startTimer = function(seconds)
  M.remainingSec = seconds
  M.state = States.RUNNING

  M.timer = vim.uv.new_timer()
  M.timer:start(1000, 1000, M.callback)
end


--- Stops and resets the current timer object.
--- If timer is not running, do not take any action.
--- Keeps the M.remaining_seconds so that it can be resumed
M.stopTimer = function()
  if M.timer then
    M.timer:stop()
    M.timer:close()
    M.timer = nil
    M.state = States.PAUSED
  end
end


--- Starts a new timer based on `M.remainingSec` that previous timer left before calling `M.stopTimer`
M.resumeTimer = function()
  if M.remainingSec > 0 then
    M.restartTimer(M.remainingSec)
  else
    print("[Pomodoro] No timer is currently running")
  end
end


--- Pauses the current timer, deletes the timer object, and start a new timer
---@param seconds number? of seconds to start the timer, should already be processed to a positive integer
M.restartTimer = function(seconds)
  M.stopTimer()
  M.startTimer(seconds)
end


---@return string Nerd font glyph indicating the status and the timestamp information to be used with tabline
M.getFormattedTime = function()
  local stamp = "No Timer"

  --if M.remainingSec > 0 then
  if M.state == States.RUNNING or M.state == States.PAUSED then
    stamp = formatSeconds(M.remainingSec)
    --elseif M.lastFinishedTimestamp then
  elseif M.state == States.ENDED then
    stamp = "Ended @ " .. M.lastFinishedTimestamp
  end

  return string.format("%s:%s", Icons[M.state], stamp)
end


M.setup = function()
  vim.api.nvim_create_user_command("PomodoroStartNew", function(args)
    local min = tonumber(args.args)
    if min == nil or min < 0 then
      print("[Pomodoro] Time must be a positive number")
    else
      M.restartTimer(tonumber(args.args) * 60)
    end
  end, { nargs = 1 })

  vim.api.nvim_create_user_command("PomodoroPause", function()
    M.stopTimer()
  end, { nargs = 0 })

  vim.api.nvim_create_user_command("PomodoroResume", function()
    M.resumeTimer()
  end, { nargs = 0 })

  vim.api.nvim_create_user_command("PomodoroGetTime", function()
    print(M.getFormattedTime())
  end, { nargs = 0 })
end

-------------------

return M
