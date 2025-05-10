--- winbar.lua
--- $figlet -f weird theovim
---   /    /                   /
--- (___ (___  ___  ___         _ _
--- |    |   )|___)|   ) \  )| | | )
--- |__  |  / |__  |__/   \/ | |  /


Winbar = {}

local function isOnlyWin()
  return #vim.api.nvim_tabpage_list_wins(0) == 1
end


local function getFileicon()
  if not Winbar.has_devicons then return "" end

  local bufname = vim.fn.bufname()
  local ext = vim.fn.fnamemodify(bufname, ":e")
  return Winbar.devicons.get_icon(bufname, ext, { default = true })
end

local function getLspServers()
  local clients = vim.lsp.get_clients({
    bufnr = vim.api.nvim_get_current_buf()
  })
  if rawequal(next(clients), nil) then return "" end

  local format = vim.g.have_nerd_font and "󰧑" or "LSP:"
  for _, client in pairs(clients) do
    format = string.format(" %s [%s]", format, client.name)
  end
  return format
end


local function getDiagnostics()
  local str = ""

  local levels = {
    e = vim.diagnostic.severity.ERROR,
    w = vim.diagnostic.severity.WARN,
    i = vim.diagnostic.severity.INFO,
    h = vim.diagnostic.severity.HINT,
  }

  local counts = {}
  for level, id in pairs(levels) do
    counts[level] = #vim.diagnostic.get(0, { severity = id })
  end

  for level, count in pairs(counts) do
    if count > 0 then
      str = string.format("%s %s: %i", str, level:upper(), count)
    end
  end

  return str
end


local function getGitSigns()
  if not vim.b.gitsigns_head then
    return ""
  end

  local icon = vim.g.have_nerd_font and "" or "GIT:"
  local head = vim.b.gitsigns_head or "-"
  local status = vim.b.gitsigns_status or ""

  return string.format(" %s [%s] %s", icon, head, status)
end


Winbar.build = function(isActive)
  local highlight = function(hl)
    return isActive and hl or "%#WinbarNC#"
  end

  local winbar = table.concat({
    "%#Normal#",
    " ",

    highlight("%#MiniStatuslineFilename#"),
    getFileicon(),
    "%<", --> Truncation starts here so the icon will be visible at all time
    " ",

    -- File info
    "%t",
    "%m",
    "%r ",

    highlight("%#MiniStatuslineDevinfo#"),
    -- Git
    getGitSigns(),

    -- Spacer
    "%=",

    -- LSP info
    highlight("%#MiniStatuslineFilename#"),
    getLspServers(),
    getDiagnostics(),

    "%#Normal#",
    " "
  })

  return winbar
end


function Winbar.setup()
  Winbar.has_devicons, Winbar.devicons = pcall(require, "nvim-web-devicons")
  if not vim.g.have_nerd_font then
    Winbar.has_devicons = false
  end

  vim.go.winbar =
  "%{%(nvim_get_current_win()==#g:actual_curwin) ? luaeval('Winbar.build(true)') : luaeval('Winbar.build(false)')%}"

  vim.api.nvim_create_autocmd({ "LspAttach", "LspDetach", "DiagnosticChanged" },
    {
      group = vim.api.nvim_create_augroup("StatuslineUpdate", { clear = true }),
      pattern = "*",
      callback = vim.schedule_wrap(function()
        vim.cmd("redrawstatus")
      end),
      desc = "Update statusline/winbar on LSP and diagnostics update"
    })
end

return Winbar
