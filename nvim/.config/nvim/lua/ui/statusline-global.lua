--- statusline.lua
--- $ figlet -f stacey theovim
--- ________________________________ _________________
--- 7      77  7  77     77     77  V  77  77        7
--- !__  __!|  !  ||  ___!|  7  ||  |  ||  ||  _  _  |
---   7  7  |     ||  __|_|  |  ||  !  ||  ||  7  7  |
---   |  |  |  7  ||     7|  !  ||     ||  ||  |  |  |
---   !__!  !__!__!!_____!!_____!!_____!!__!!__!__!__!
---
--- Global statusline to be used with laststatus=3 and winbar, simpler with no dynamic components
--- Somewhat inspired by Mini.Statusline, uses its highlight groups

Statusline = {}


Statusline.getMode = function()
  local CTRL_S = vim.api.nvim_replace_termcodes("<C-S>", true, true, true)
  local CTRL_V = vim.api.nvim_replace_termcodes("<C-V>", true, true, true)
  local modesTable = {
    ["n"] = { name = "N", hl = "%#MiniStatuslineModeNormal#" },
    ["v"] = { name = "V", hl = "%#MiniStatuslineModeVisual#" },
    ["V"] = { name = "V-LINE", hl = "%#MiniStatuslineModeVisual#" },
    [CTRL_V] = { name = "V-BLOCK", hl = "%#MiniStatuslineModeVisual#" },
    ["s"] = { name = "SEL", hl = "%#MiniStatuslineModeVisual#" },
    ["S"] = { name = "SEL-LINE", hl = "%#MiniStatuslineModeVisual#" },
    [CTRL_S] = { name = "SEL-BLOCK", hl = "%#MiniStatuslineModeVisual#" },
    ["i"] = { name = "I", hl = "%#MiniStatuslineModeInsert#" },
    ["R"] = { name = "R", hl = "%#MiniStatuslineModeReplace#" },
    ["c"] = { name = "CMD", hl = "%#MiniStatuslineModeCommand#" },
    ["r"] = { name = "PROMPT", hl = "%#MiniStatuslineModeOther#" },
    ["!"] = { name = "SH", hl = "%#MiniStatuslineModeOther#" },
    ["t"] = { name = "TERM", hl = "%#MiniStatuslineModeOther#" },
  }

  -- set a default option via metatable
  local modes = setmetatable(modesTable, {
    __index = function()
      return { name = "UNKNOWN", hl = "%#MiniStatuslineModeOther#" }
    end
  })

  return modes[vim.fn.mode()]
end


Statusline.build = function()
  local modeInfo = Statusline.getMode()
  local cwd = string.format("%s %s ",
    (vim.g.have_nerd_font and " " or "CWD: "), vim.fn.fnamemodify(vim.fn.getcwd(), ":t"))
  local filepath = (vim.g.have_nerd_font and " " or "") .. "%f"
  local filemodifier = "%m%r "

  local ff = vim.bo.fileformat
  local enc = (vim.bo.fileencoding == "") and (vim.go.encoding) or (vim.bo.fileencoding)
  local fileInfo = string.format(" %%Y | %s | %s ", ff:upper(), enc:upper()) --> FT + format + encoding
  local location = " %l/%L:%2v/%-2{virtcol('$') - 1} "
  local percentage = (vim.g.have_nerd_font and "  " or " ") .. "%P "

  return table.concat({
    modeInfo.hl,
    " ",
    modeInfo.name,
    " ",

    "%#MiniStatuslineDevinfo#",
    " ",
    cwd,
    "%<", --> truncation point

    "%#MiniStatuslineFilename#",
    " ",
    filepath,
    filemodifier,


    "%#MiniStatuslineInactive#",
    "%=", --> spacer


    "%#MiniStatuslineFilename#",
    fileInfo,

    "%#MiniStatuslineFileinfo#",
    location,

    modeInfo.hl,
    percentage,
  })
end


Statusline.setup = function()
  vim.go.statusline = "%!v:lua.Statusline.build()"
end

return Statusline
