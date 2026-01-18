-- Theovim options
-- Excludes default Neovim options:
-- https://neovim.io/doc/user/vim_diff.html#nvim-defaults
local o           = vim.o

-- Tab
o.softtabstop     = 0    --> How many chracters the /cursor moves/ with <TAB> and <BS> -- 0 to disable
o.expandtab       = true --> Use space instead of tab
o.shiftwidth      = 2    --> Number of spaces to use for auto-indentation, <<, >>, etc.
o.shiftround      = true --> Make the indentation to a multiple of shiftwidth when using < or >

-- Location in the buffer
o.number          = true
o.relativenumber  = true
o.cursorline      = true
o.cursorlineopt   = "number" --> line, screenline, both (i.e., "number,line")
o.cursorcolumn    = true

-- Search and replace
o.ignorecase      = true    --> Ignore case in search
o.smartcase       = true    --> /smartcase -> apply ignorecase | /sMartcase -> do not apply ignorecase
o.inccommand      = "split" --> show the substitution in a split window

-- Split
o.splitright      = true --> Vertical split created right
o.splitbelow      = true --> Horizontal split created below

-- UI
o.signcolumn      = "yes"     --> Render signcolumn always to prevent text shifting
o.scrolloff       = 7         --> Keep minimum x number of screen lines above and below the cursor
o.termguicolors   = true      --> Enables 24-bit RGB color in the TUI
o.showtabline     = 2         --> 0: never, 1: >= 2 tabs, 2: always
o.laststatus      = 3         --> 0: never, 1: >= 2 windows, 2: always, 3: always and have one global statusline
o.winborder       = "rounded" --> border appearance used by some plugins (e.g., nvim-cmp)

-- Char rendering
o.list            = true --> Render special char in listchars
vim.opt.listchars = { tab = "⇥ ", trail = "␣", nbsp = "⍽", leadmultispace = "┊ ", }
o.showbreak       = "↪" --> Render beginning of wrapped lines
o.breakindent     = true --> Wrapped line will have the same indentation level as the beginning of the line

-- Spell
o.spell           = false    --> autocmd will enable spellcheck in Tex or markdown
vim.opt.spelllang = { "en", }
o.spellsuggest    = "best,8" --> Suggest 8 words for spell suggestion
o.spelloptions    = "camel"  --> Consider CamelCase when checking spelling

-- Fold
o.foldenable      = false    --> Open all folds until I close them using zc/zC or update using zx
o.foldmethod      = "marker" --> filetypes with treesitters enabled, it will be set to expr
--o.foldlevel       = 2        --> Ignore n - 1 level fold

-- Update time
o.updatetime      = 250
o.timeoutlen      = 300

-- Window size
o.winminwidth     = 3

-- Completion
o.wildmode        =
"noselect:full"                                   --> behavior of <TAB> in cmd completion. Do not pre-select, match full completion
o.completeopt     = "noselect,menu,menuone,popup" --> no pre-insert or pre-select, show menu even w/ one item, use popup

-- Others
o.mouse           = "a"
o.confirm         = true --> Confirm before exiting with unsaved bufffer(s)
