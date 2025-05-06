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
  for _, client in ipairs(clients) do
    format = string.format("%s [%s]", format, client.name)
  end
  return format
end

Winbar.build = function(isActive)
  local highlight = isActive and "%#MiniStatuslineFileinfo#" or "%#WinbarNC#"

  local winbar = table.concat({
    "%#Normal#",
    highlight,

    getFileicon(),
    "%<", --> Truncation starts here so the icon will be visible at all time
    " ",

    -- File info
    "%t",
    "%m",
    "%r ",

    -- LSP Servers
    getLspServers(),

    "%=", --> spacer

    " %P ",
    "%#Normal#"
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
      desc = "Update statusline/winbar"
    })
end

return Winbar
