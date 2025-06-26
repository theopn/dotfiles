--- Theo's simple Pomodoro Timer
--- Supports a single instance of pause-able countdown timer
--- and integration as a tabline modue (or Statusline if you modify M.callback)
--- Inspired by cbrgm's countdown.nvim

local M = {}

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

M.timer = nil
M.remainingSec = 0
M.lastFinishedTimestamp = nil
M.state = States.NONE

---@param seconds number of seconds to format.
---@return string of the format "HH:MM:SS".
local function formatSeconds(seconds)
  return string.format("%02d:%02d:%02d",
    math.floor(seconds / 3600),
    math.floor((seconds % 3600) / 60),
    seconds % 60
  )
end

---@return string|osdate of the current timestamp in the format "HH:MM:SS".
local function getCurrTime()
  return os.date("%H:%M:%S")
end

--- Updates the M.remaining_seconds and redraws Tabline
M.callback = function()
  M.remainingSec = M.remainingSec - 1

  -- To avoid E5560
  vim.defer_fn(function()
    vim.cmd("redrawtabline")
  end, 1000)

  if M.remainingSec <= 0 then
    M.lastFinishedTimestamp = getCurrTime()
    print(string.format("[Pomodoro] Time is up! (%s)", M.lastFinishedTimestamp))
    M.timer:stop()
    M.timer:close()
    M.timer = nil
    M.state = States.ENDED
  end
end

--- Stops and resets the current timer object
--- Keeps the M.remaining_seconds so that it can be resumed
M.stopTimer = function()
  if M.timer then
    M.timer:stop()
    M.timer:close()
    M.timer = nil
    M.state = States.PAUSED
  end
end

--- Starts a timer assuming none is running yet
---@param minutes number? of minutes to start the timer
M.startTimer = function(minutes)
  if minutes <= 0 then
    print("[Pomodoro] Error: Minutes should be a positive number")
  end

  M.remainingSec = minutes * 60
  M.state = States.RUNNING

  M.timer = vim.uv.new_timer()
  M.timer:start(1000, 1000, M.callback)
end

--- Safely pauses the current timer and deletes the timer object
--- before calling M.startTimer()
---@param minutes number? of minutes to start the timer
M.startTimerWrapper = function(minutes)
  M.stopTimer()
  M.startTimer(minutes)
end

---@return string
M.timerGetTime = function()
  local stamp = ""
  if M.remainingSec > 0 then
    stamp = formatSeconds(M.remainingSec)
  elseif M.lastFinishedTimestamp then
    stamp = M.lastFinishedTimestamp
  end

  return string.format("%s:%s", Icons[M.state], stamp)
end

M.setup = function()
  vim.api.nvim_create_user_command("PomodoroPause", function()
    M.stopTimer()
  end, { nargs = 0 })

  vim.api.nvim_create_user_command("PomodoroResume", function()
    M.resumeTimer()
  end, { nargs = 0 })

  vim.api.nvim_create_user_command("PomodoroStart", function(args)
    local minutes = args["args"]
    M.startTimerWrapper(tonumber(minutes))
  end, { nargs = "?" })

  vim.api.nvim_create_user_command("PomodoroGetTime", function()
    print(M.timerGetTime())
  end, { nargs = 0 })
end


return M
