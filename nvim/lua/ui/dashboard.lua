--- *dashboard.lua* Startup Dashboard
---
--- $ figlet -f mini theovim
--- _|_|_  _  _   o._ _
---  |_| |(/_(_)\/|| | |
---
--- Provide a framework to open a dashboard on the Neovim startup when there is no buffer opened (only the empty buf)
---
--- Suggested dependencies:
--- - Nerd font
--- - MiniStarter* highlights from your choice of colorscheme
---

local Dashboard = {}

-- ASCII arts of my chunky cat Oliver
-- Mostly from: https://www.asciiart.eu/animals/cats
-- Make sure the length of each string is consistent! Hard coding much...
local olivers = {
  {
    [[          \/   \/              ]],
    [[          |\__/,|     _        ]],
    [[        _.|o o  |_   ) )       ]],
    [[       -(((---(((--------      ]]
  },
  -- This one is by Jonathan
  {
    [[       \/ \/                   ]],
    [[       /\_/\ _______           ]],
    [[      = o_o =  _ _  \     _    ]],
    [[      (__^__)   __(  \.__) )   ]],
    [[   (@)<_____>__(_____)____/    ]],
    [[     ♡ ~~ ♡ OLIVER ♡ ~~ ♡      ]],
  },
  {
    [[        \/   \/                ]],
    [[        |\__/,|        _       ]],
    [[        |_ _  |.-----.) )      ]],
    [[        ( T   ))        )      ]],
    [[       (((^_(((/___(((_/       ]]
  },
  {
    [[          \/       \/          ]],
    [[          /\_______/\          ]],
    [[         /   o   o   \         ]],
    [[        (  ==  ^  ==  )        ]],
    [[         )           (         ]],
    [[        (             )        ]],
    [[        ( (  )   (  ) )        ]],
    [[       (__(__)___(__)__)       ]],
  },
  {
    [[                          _    ]],
    [[         |\      _-``---,) )   ]],
    [[   ZZZzz /,`.-'`'    -.   /    ]],
    [[        |,4-  ) )-,_. ,\ (     ]],
    [[       '---''(_/--'  `-'\_)    ]]
  },
}
math.randomseed(os.time())
local header = olivers[math.random(#olivers)]

local logo = {
  [[ ___                    ]],
  [[  | |_  _  _     o __   ]],
  [[  | | |(/_(_)\_/ | |||  ]],
  "",
  os.date("[ ━━%m-%d━━ ❖ ━━%H:%M━━ ]"),
}

-- Calculating max width and make an empty length of the max width
local max_width = #header[1]
local empty_line = string.rep(" ", max_width)
-- Add empty line paddings for ASCII
table.insert(header, 1, empty_line)
header[#header + 1] = empty_line
logo[#logo + 1] = empty_line
-- max height = empty line + #header + #logo + empty line + 1 safety net
local max_height = 1 + #header + #logo + 1 + 1


--- render()
--- If the buffer does not have a name, replace the buffer with the formated Dashboard contents
--- Inspired by : https://github.com/chadcat7/kodo/blob/main/lua/ui/dash/init.lua
---               https://github.com/NvChad/ui/blob/dev/lua/nvchad_ui/nvdash/init.lua
---
local render = function()
  local win_width = vim.api.nvim_win_get_width(0)
  local win_height = vim.api.nvim_win_get_height(0)

  -----------------------------------
  -- Condition check --

  -- Expands the current file name and see if it's empty
  if vim.api.nvim_buf_get_name(0) ~= "" then return end --> or use vim.fn.expand("%")

  -- Check if window is too small to launch
  if win_height < max_height then
    vim.notify_once("Dashboard: window size is too small :(") --> use notify_once to stop notification spamming
    return
  end

  -- The default empty buffer will go away when the new Dashboard buffer replaces it
  --vim.api.nvim_buf_set_option(0, "bufhidden", "wipe")
  vim.opt_local.bufhidden = "wipe"

  -- Create a new buffer and replace the old one
  local buf = vim.api.nvim_create_buf(false, true) --> listed false, scratchbuffer true
  vim.api.nvim_win_set_buf(0, buf)
  vim.opt_local.filetype       = "TheovimDashboard"
  vim.opt_local.buflisted      = false
  vim.opt_local.list           = false
  vim.opt_local.wrap           = true
  vim.opt_local.relativenumber = false
  vim.opt_local.number         = false
  vim.opt_local.cursorline     = false
  vim.opt_local.cursorcolumn   = false
  vim.opt_local.colorcolumn    = "0"

  vim.opt_local.modifiable     = true

  -----------------------------------
  -- Init the DB contents--

  -- Table for the contents
  local dashboard              = {}
  -- add padding to the header
  local add_padding            = function(str)
    local pad = (win_width - vim.fn.strwidth(str)) / 2
    return string.rep(" ", math.floor(pad)) .. str
  end

  -- Inserting contents to the DB table
  for _, val in ipairs(header) do
    table.insert(dashboard, add_padding(val))
  end
  for _, val in ipairs(logo) do
    table.insert(dashboard, add_padding(val))
  end
  table.insert(dashboard, empty_line)

  --------------------
  -- Setting the dashboard --
  local result = {}
  for i = 1, win_height do
    result[i] = ""
  end

  local hdr_start_idx_save = math.floor((win_height / 2) - (#dashboard / 2) - 1)
  local hdr_start_idx = math.floor((win_height / 2) - (#dashboard / 2) - 1)

  -- adding the dashboard
  for _, val in ipairs(dashboard) do
    result[hdr_start_idx_save] = val
    hdr_start_idx_save = hdr_start_idx_save + 1
  end

  -- setting the dasboard
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, result)

  -- set the cursor: 15 is is my best guess on where the first char of the button would be. If too narrow, use 0
  local cursor_column_idx = (win_width > max_width) and (math.floor(win_width / 2) - 15) or (0)
  vim.api.nvim_win_set_cursor(0, { hdr_start_idx + #header + #logo, cursor_column_idx })

  ---------------------------
  -- Setting highlights --

  for i = hdr_start_idx, hdr_start_idx + #header - 2 do                       --> Ignore last two empty lines
    vim.api.nvim_buf_add_highlight(buf, -1, "MiniStarterFooter", i, 0, -1)    --> -1 for no namespace
  end
  for i = hdr_start_idx + #header - 2, hdr_start_idx + #header + #logo - 2 do --> Again, -2 because of empty lines
    vim.api.nvim_buf_add_highlight(buf, -1, "MiniStarterHeader", i, 0, -1)
  end

  -- The end --
  vim.opt_local.modifiable = false
end

--- opener()
--- Wrap Dashboard.render() with schedule().
--- schedule_wrap({cb}) "Defers callback `cb` until the Nvim API is safe to call," and schedule() calls wrapped func
--- Let's say the user is resizing the terminal window. Without deferring, render() will jump ahead and start rendering,
--- which casues breakage in calculations and rendering. This allows render() to wait until Neovim says it's safe
---
Dashboard.opener = function()
  vim.schedule(render)
end

--- setup()
--- Call opener in the startup and make autocmd for resizing
---
Dashboard.setup = function()
  Dashboard.opener()

  vim.api.nvim_create_autocmd("VimResized", {
    callback = function()
      if vim.bo.filetype == "TheovimDashboard" then
        vim.opt_local.modifiable = true
        vim.api.nvim_buf_set_lines(0, 0, -1, false, { "" })
        Dashboard.opener()
      end
    end,
  })
end

return Dashboard
