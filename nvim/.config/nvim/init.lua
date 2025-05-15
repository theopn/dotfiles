--- init.lua
---
---      \/       \/
---      /\_______/\
---     /   o   o   \
---    (  ==  ^  ==  )
---     )  [Ollie]  (
---    (             )
---    ( (  )   (  ) )
---   (__(__)___(__)__)
---  ___
---   | |_  _  _     o __
---   | | |(/_(_)\_/ | |||
---  Oliver : https://www.asciiart.eu/animals/cats + Jonathan added a few layers of belly because Oliver is a chunky boi
---
--- Global variable initialization and module calls

-- Global variables
vim.g.have_nerd_font = true
vim.g.have_transparent_bg = true
if vim.g.neovide then
  require("neovide")
end

-- Core modules
require("theovim.opt")
require("theovim.keymaps")
require("theovim.commands")
require("theovim.autocmds")
require("theovim.lsp")
--require("theovim.completion")

require("theovim.lazy")

-- Custom UI modules
require("ui.tabline").setup()
require("ui.statusline-global").setup()
require("ui.winbar").setup()
