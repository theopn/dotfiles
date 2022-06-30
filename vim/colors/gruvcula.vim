" -----------------------------------------------------------------------------
"  ▄▀  █▄▄▄▄  ▄       ▄   ▄█▄      ▄   █    ██ 
"▄▀    █  ▄▀   █       █  █▀ ▀▄     █  █    █ █
"█ ▀▄  █▀▀▌ █   █ █     █ █   ▀  █   █ █    █▄▄█
"█   █ █  █ █   █  █    █ █▄  ▄▀ █   █ ███▄ █  █
" ███    █  █▄ ▄█   █  █  ▀███▀  █▄ ▄█     ▀   █
"       ▀    ▀▀▀     █▐           ▀▀▀         █
"                    ▐                       ▀
" Mix between Gruvbox.vim and Dracula.vim
" OG Author: Dracula by dsifford, dsifford @ GitHub
"          : Gruvcula by merhetz @ GitHub
" By theopn
" -----------------------------------------------------------------------------

" Supporting code -------------------------------------------------------------
" Initialisation: {{{

if version > 580
  hi clear
  if exists("syntax_on")
    syntax reset
  endif
endif

let g:colors_name='gruvcula'

if !(has('termguicolors') && &termguicolors) && !has('gui_running') && &t_Co != 256
  finish
endif

" }}}
" Global Settings: {{{

if !exists('g:gruvcula_bold')
  let g:gruvcula_bold=1
endif
if !exists('g:gruvcula_italic')
  if has('gui_running') || $TERM_ITALICS == 'true'
    let g:gruvcula_italic=1
  else
    let g:gruvcula_italic=0
  endif
endif
if !exists('g:gruvcula_undercurl')
  let g:gruvcula_undercurl=1
endif
if !exists('g:gruvcula_underline')
  let g:gruvcula_underline=1
endif
if !exists('g:gruvcula_inverse')
  let g:gruvcula_inverse=1
endif

if !exists('g:gruvcula_guisp_fallback') || index(['fg', 'bg'], g:gruvcula_guisp_fallback) == -1
  let g:gruvcula_guisp_fallback='NONE'
endif

if !exists('g:gruvcula_improved_strings')
  let g:gruvcula_improved_strings=0
endif

if !exists('g:gruvcula_improved_warnings')
  let g:gruvcula_improved_warnings=0
endif

if !exists('g:gruvcula_termcolors')
  let g:gruvcula_termcolors=256
endif

if !exists('g:gruvcula_invert_indent_guides')
  let g:gruvcula_invert_indent_guides=0
endif

if exists('g:gruvcula_contrast')
  echo 'g:gruvcula_contrast is deprecated; use g:gruvcula_contrast_light and g:gruvcula_contrast_dark instead'
endif

if !exists('g:gruvcula_contrast_dark')
  let g:gruvcula_contrast_dark='medium'
endif

if !exists('g:gruvcula_contrast_light')
  let g:gruvcula_contrast_light='medium'
endif

let s:is_dark=(&background == 'dark')

" }}}
" Palette: {{{

" setup palette dictionary
let s:gc = {}

" fill it with absolute colors
let s:gc.dark0_hard  = ['#282A35', 234]     " 29-32-33
let s:gc.dark0       = ['#282A35', 235]     " 40-40-40
let s:gc.dark0_soft  = ['#282A35', 236]     " 50-48-47
let s:gc.dark1       = ['#242630', 237]     " 60-56-54
let s:gc.dark2       = ['#424450', 239]     " 80-73-69
let s:gc.dark3       = ['#53576E', 241]     " 102-92-84
let s:gc.dark4       = ['#696E8C', 243]     " 124-111-100
let s:gc.dark4_256   = ['#696E8C', 243]     " 124-111-100

let s:gc.gray_245    = ['#675A51', 245]     " 146-131-116
let s:gc.gray_244    = ['#675A51', 244]     " 146-131-116

let s:gc.light0_hard = ['#FFFFFF', 230]     " 249-245-215
let s:gc.light0      = ['#F8F8F2', 229]     " 253-244-193
let s:gc.light0_soft = ['#F8F8F2', 228]     " 242-229-188
let s:gc.light1      = ['#A4FFFF', 223]     " 235-219-178
let s:gc.light2      = ['#8BE9FD', 250]     " 213-196-161
let s:gc.light3      = ['#72E3FD', 248]     " 189-174-147
let s:gc.light4      = ['#6272A4', 246]     " 168-153-132
let s:gc.light4_256  = ['#6272A4', 246]     " 168-153-132

let s:gc.bright_red     = ['#ff6e6e', 167]     " 251-73-52
let s:gc.bright_green   = ['#69ff94', 142]     " 184-187-38
let s:gc.bright_yellow  = ['#ffffa5', 214]     " 250-189-47
let s:gc.bright_blue    = ['#d6acff', 109]     " 131-165-152
let s:gc.bright_purple  = ['#ff92df', 175]     " 211-134-155
let s:gc.bright_aqua    = ['#8ec07c', 108]     " 142-192-124
let s:gc.bright_orange  = ['#fe8019', 208]     " 254-128-25

let s:gc.neutral_red    = ['#ff5555', 124]     " 204-36-29
let s:gc.neutral_green  = ['#50fa7b', 106]     " 152-151-26
let s:gc.neutral_yellow = ['#f1fa8c', 172]     " 215-153-33
let s:gc.neutral_blue   = ['#bd93f9', 66]      " 69-133-136
let s:gc.neutral_purple = ['#b16286', 132]     " 177-98-134
let s:gc.neutral_aqua   = ['#689d6a', 72]      " 104-157-106
let s:gc.neutral_orange = ['#d65d0e', 166]     " 214-93-14

let s:gc.faded_red      = ['#9d0006', 88]      " 157-0-6
let s:gc.faded_green    = ['#79740e', 100]     " 121-116-14
let s:gc.faded_yellow   = ['#b57614', 136]     " 181-118-20
let s:gc.faded_blue     = ['#076678', 24]      " 7-102-120
let s:gc.faded_purple   = ['#8f3f71', 96]      " 143-63-113
let s:gc.faded_aqua     = ['#427b58', 66]      " 66-123-88
let s:gc.faded_orange   = ['#af3a03', 130]     " 175-58-3

" }}}
" Setup Emphasis: {{{

let s:bold = 'bold,'
if g:gruvcula_bold == 0
  let s:bold = ''
endif

let s:italic = 'italic,'
if g:gruvcula_italic == 0
  let s:italic = ''
endif

let s:underline = 'underline,'
if g:gruvcula_underline == 0
  let s:underline = ''
endif

let s:undercurl = 'undercurl,'
if g:gruvcula_undercurl == 0
  let s:undercurl = ''
endif

let s:inverse = 'inverse,'
if g:gruvcula_inverse == 0
  let s:inverse = ''
endif

" }}}
" Setup Colors: {{{

let s:vim_bg = ['bg', 'bg']
let s:vim_fg = ['fg', 'fg']
let s:none = ['NONE', 'NONE']

" determine relative colors
if s:is_dark
  let s:bg0  = s:gc.dark0
  if g:gruvcula_contrast_dark == 'soft'
    let s:bg0  = s:gc.dark0_soft
  elseif g:gruvcula_contrast_dark == 'hard'
    let s:bg0  = s:gc.dark0_hard
  endif

  let s:bg1  = s:gc.dark1
  let s:bg2  = s:gc.dark2
  let s:bg3  = s:gc.dark3
  let s:bg4  = s:gc.dark4

  let s:gray = s:gc.gray_245

  let s:fg0 = s:gc.light0
  let s:fg1 = s:gc.light1
  let s:fg2 = s:gc.light2
  let s:fg3 = s:gc.light3
  let s:fg4 = s:gc.light4

  let s:fg4_256 = s:gc.light4_256

  let s:red    = s:gc.bright_red
  let s:green  = s:gc.bright_green
  let s:yellow = s:gc.bright_yellow
  let s:blue   = s:gc.bright_blue
  let s:purple = s:gc.bright_purple
  let s:aqua   = s:gc.bright_aqua
  let s:orange = s:gc.bright_orange
else
  let s:bg0  = s:gc.light0
  if g:gruvcula_contrast_light == 'soft'
    let s:bg0  = s:gc.light0_soft
  elseif g:gruvcula_contrast_light == 'hard'
    let s:bg0  = s:gc.light0_hard
  endif

  let s:bg1  = s:gc.light1
  let s:bg2  = s:gc.light2
  let s:bg3  = s:gc.light3
  let s:bg4  = s:gc.light4

  let s:gray = s:gc.gray_244

  let s:fg0 = s:gc.dark0
  let s:fg1 = s:gc.dark1
  let s:fg2 = s:gc.dark2
  let s:fg3 = s:gc.dark3
  let s:fg4 = s:gc.dark4

  let s:fg4_256 = s:gc.dark4_256

  let s:red    = s:gc.faded_red
  let s:green  = s:gc.faded_green
  let s:yellow = s:gc.faded_yellow
  let s:blue   = s:gc.faded_blue
  let s:purple = s:gc.faded_purple
  let s:aqua   = s:gc.faded_aqua
  let s:orange = s:gc.faded_orange
endif

" reset to 16 colors fallback
if g:gruvcula_termcolors == 16
  let s:bg0[1]    = 0
  let s:fg4[1]    = 7
  let s:gray[1]   = 8
  let s:red[1]    = 9
  let s:green[1]  = 10
  let s:yellow[1] = 11
  let s:blue[1]   = 12
  let s:purple[1] = 13
  let s:aqua[1]   = 14
  let s:fg1[1]    = 15
endif

" save current relative colors back to palette dictionary
let s:gc.bg0 = s:bg0
let s:gc.bg1 = s:bg1
let s:gc.bg2 = s:bg2
let s:gc.bg3 = s:bg3
let s:gc.bg4 = s:bg4

let s:gc.gray = s:gray

let s:gc.fg0 = s:fg0
let s:gc.fg1 = s:fg1
let s:gc.fg2 = s:fg2
let s:gc.fg3 = s:fg3
let s:gc.fg4 = s:fg4

let s:gc.fg4_256 = s:fg4_256

let s:gc.red    = s:red
let s:gc.green  = s:green
let s:gc.yellow = s:yellow
let s:gc.blue   = s:blue
let s:gc.purple = s:purple
let s:gc.aqua   = s:aqua
let s:gc.orange = s:orange

" Overload Setting: {{{

let s:hls_cursor = s:orange
if exists('g:gruvcula_hls_cursor')
  let s:hls_cursor = get(s:gc, g:gruvcula_hls_cursor)
endif

let s:number_column = s:none
if exists('g:gruvcula_number_column')
  let s:number_column = get(s:gc, g:gruvcula_number_column)
endif

let s:sign_column = s:bg1

if exists('g:gitgutter_override_sign_column_highlight') &&
      \ g:gitgutter_override_sign_column_highlight == 1
  let s:sign_column = s:number_column
else
  let g:gitgutter_override_sign_column_highlight = 0

  if exists('g:gruvcula_sign_column')
    let s:sign_column = get(s:gc, g:gruvcula_sign_column)
  endif
endif

let s:color_column = s:bg1
if exists('g:gruvcula_color_column')
  let s:color_column = get(s:gc, g:gruvcula_color_column)
endif

let s:vert_split = s:bg0
if exists('g:gruvcula_vert_split')
  let s:vert_split = get(s:gc, g:gruvcula_vert_split)
endif

let s:invert_signs = ''
if exists('g:gruvcula_invert_signs')
  if g:gruvcula_invert_signs == 1
    let s:invert_signs = s:inverse
  endif
endif

let s:invert_selection = s:inverse
if exists('g:gruvcula_invert_selection')
  if g:gruvcula_invert_selection == 0
    let s:invert_selection = ''
  endif
endif

let s:invert_tabline = ''
if exists('g:gruvcula_invert_tabline')
  if g:gruvcula_invert_tabline == 1
    let s:invert_tabline = s:inverse
  endif
endif

let s:italicize_comments = s:italic
if exists('g:gruvcula_italicize_comments')
  if g:gruvcula_italicize_comments == 0
    let s:italicize_comments = ''
  endif
endif

let s:italicize_strings = ''
if exists('g:gruvcula_italicize_strings')
  if g:gruvcula_italicize_strings == 1
    let s:italicize_strings = s:italic
  endif
endif

" }}}
" Highlighting Function: {{{

function! s:HL(group, fg, ...)
  " Arguments: group, guifg, guibg, gui, guisp

  " foreground
  let fg = a:fg

  " background
  if a:0 >= 1
    let bg = a:1
  else
    let bg = s:none
  endif

  " emphasis
  if a:0 >= 2 && strlen(a:2)
    let emstr = a:2
  else
    let emstr = 'NONE,'
  endif

  " special fallback
  if a:0 >= 3
    if g:gruvcula_guisp_fallback != 'NONE'
      let fg = a:3
    endif

    " bg fallback mode should invert higlighting
    if g:gruvcula_guisp_fallback == 'bg'
      let emstr .= 'inverse,'
    endif
  endif

  let histring = [ 'hi', a:group,
        \ 'guifg=' . fg[0], 'ctermfg=' . fg[1],
        \ 'guibg=' . bg[0], 'ctermbg=' . bg[1],
        \ 'gui=' . emstr[:-2], 'cterm=' . emstr[:-2]
        \ ]

  " special
  if a:0 >= 3
    call add(histring, 'guisp=' . a:3[0])
  endif

  execute join(histring, ' ')
endfunction

" }}}
" Gruvcula Hi Groups: {{{

" memoize common hi groups
call s:HL('GruvculaFg0', s:fg0)
call s:HL('GruvculaFg1', s:fg1)
call s:HL('GruvculaFg2', s:fg2)
call s:HL('GruvculaFg3', s:fg3)
call s:HL('GruvculaFg4', s:fg4)
call s:HL('GruvculaGray', s:gray)
call s:HL('GruvculaBg0', s:bg0)
call s:HL('GruvculaBg1', s:bg1)
call s:HL('GruvculaBg2', s:bg2)
call s:HL('GruvculaBg3', s:bg3)
call s:HL('GruvculaBg4', s:bg4)

call s:HL('GruvculaRed', s:red)
call s:HL('GruvculaRedBold', s:red, s:none, s:bold)
call s:HL('GruvculaGreen', s:green)
call s:HL('GruvculaGreenBold', s:green, s:none, s:bold)
call s:HL('GruvculaYellow', s:yellow)
call s:HL('GruvculaYellowBold', s:yellow, s:none, s:bold)
call s:HL('GruvculaBlue', s:blue)
call s:HL('GruvculaBlueBold', s:blue, s:none, s:bold)
call s:HL('GruvculaPurple', s:purple)
call s:HL('GruvculaPurpleBold', s:purple, s:none, s:bold)
call s:HL('GruvculaAqua', s:aqua)
call s:HL('GruvculaAquaBold', s:aqua, s:none, s:bold)
call s:HL('GruvculaOrange', s:orange)
call s:HL('GruvculaOrangeBold', s:orange, s:none, s:bold)

call s:HL('GruvculaRedSign', s:red, s:sign_column, s:invert_signs)
call s:HL('GruvculaGreenSign', s:green, s:sign_column, s:invert_signs)
call s:HL('GruvculaYellowSign', s:yellow, s:sign_column, s:invert_signs)
call s:HL('GruvculaBlueSign', s:blue, s:sign_column, s:invert_signs)
call s:HL('GruvculaPurpleSign', s:purple, s:sign_column, s:invert_signs)
call s:HL('GruvculaAquaSign', s:aqua, s:sign_column, s:invert_signs)
call s:HL('GruvculaOrangeSign', s:orange, s:sign_column, s:invert_signs)

" }}}

" Vanilla colorscheme ---------------------------------------------------------
" General UI: {{{

" Normal text
call s:HL('Normal', s:fg1, s:bg0)

" Correct background (see issue #7):
" --- Problem with changing between dark and light on 256 color terminal
" --- https://github.com/morhetz/gruvcula/issues/7
if s:is_dark
  set background=dark
else
  set background=light
endif

if version >= 700
  " Screen line that the cursor is
  call s:HL('CursorLine',   s:none, s:bg1)
  " Screen column that the cursor is
  hi! link CursorColumn CursorLine

  " Tab pages line filler
  call s:HL('TabLineFill', s:bg4, s:bg1, s:invert_tabline)
  " Active tab page label
  call s:HL('TabLineSel', s:green, s:bg1, s:invert_tabline)
  " Not active tab page label
  hi! link TabLine TabLineFill

  " Match paired bracket under the cursor
  call s:HL('MatchParen', s:none, s:bg3, s:bold)
endif

if version >= 703
  " Highlighted screen columns
  call s:HL('ColorColumn',  s:none, s:color_column)

  " Concealed element: \lambda → λ
  call s:HL('Conceal', s:blue, s:none)

  " Line number of CursorLine
  call s:HL('CursorLineNr', s:yellow, s:bg1)
endif

hi! link NonText GruvculaBg2
hi! link SpecialKey GruvculaBg2

call s:HL('Visual',    s:none,  s:bg3, s:invert_selection)
hi! link VisualNOS Visual

call s:HL('Search',    s:yellow, s:bg0, s:inverse)
call s:HL('IncSearch', s:hls_cursor, s:bg0, s:inverse)

call s:HL('Underlined', s:blue, s:none, s:underline)

call s:HL('StatusLine',   s:bg2, s:fg1, s:inverse)
call s:HL('StatusLineNC', s:bg1, s:fg4, s:inverse)

" The column separating vertically split windows
call s:HL('VertSplit', s:bg3, s:vert_split)

" Current match in wildmenu completion
call s:HL('WildMenu', s:blue, s:bg2, s:bold)

" Directory names, special names in listing
hi! link Directory GruvculaGreenBold

" Titles for output from :set all, :autocmd, etc.
hi! link Title GruvculaGreenBold

" Error messages on the command line
call s:HL('ErrorMsg',   s:bg0, s:red, s:bold)
" More prompt: -- More --
hi! link MoreMsg GruvculaYellowBold
" Current mode message: -- INSERT --
hi! link ModeMsg GruvculaYellowBold
" 'Press enter' prompt and yes/no questions
hi! link Question GruvculaOrangeBold
" Warning messages
hi! link WarningMsg GruvculaRedBold

" }}}
" Gutter: {{{

" Line number for :number and :# commands
call s:HL('LineNr', s:bg4, s:number_column)

" Column where signs are displayed
call s:HL('SignColumn', s:none, s:sign_column)

" Line used for closed folds
call s:HL('Folded', s:gray, s:bg1) ", s:italic)
" Column where folds are displayed
call s:HL('FoldColumn', s:gray, s:bg1)

" }}}
" Cursor: {{{

" Character under cursor
call s:HL('Cursor', s:none, s:none, s:inverse)
" Visual mode cursor, selection
hi! link vCursor Cursor
" Input moder cursor
hi! link iCursor Cursor
" Language mapping cursor
hi! link lCursor Cursor

" }}}
" Syntax Highlighting: {{{

if g:gruvcula_improved_strings == 0
  hi! link Special GruvculaOrange
else
  call s:HL('Special', s:orange, s:bg1, s:italicize_strings)
endif

call s:HL('Comment', s:gray, s:none, s:italicize_comments)
call s:HL('Todo', s:vim_fg, s:vim_bg, s:bold . s:italic)
call s:HL('Error', s:red, s:vim_bg, s:bold . s:inverse)

" Generic statement
hi! link Statement GruvculaRed
" if, then, else, endif, swicth, etc.
hi! link Conditional GruvculaRed
" for, do, while, etc.
hi! link Repeat GruvculaRed
" case, default, etc.
hi! link Label GruvculaRed
" try, catch, throw
hi! link Exception GruvculaRed
" sizeof, "+", "*", etc.
hi! link Operator Normal
" Any other keyword
hi! link Keyword GruvculaRed

" Variable name
hi! link Identifier GruvculaBlue
" Function name
hi! link Function GruvculaGreenBold

" Generic preprocessor
hi! link PreProc GruvculaAqua
" Preprocessor #include
hi! link Include GruvculaAqua
" Preprocessor #define
hi! link Define GruvculaAqua
" Same as Define
hi! link Macro GruvculaAqua
" Preprocessor #if, #else, #endif, etc.
hi! link PreCondit GruvculaAqua

" Generic constant
hi! link Constant GruvculaPurple
" Character constant: 'c', '/n'
hi! link Character GruvculaPurple
" String constant: "this is a string"
if g:gruvcula_improved_strings == 0
  call s:HL('String',  s:green, s:none, s:italicize_strings)
else
  call s:HL('String',  s:fg1, s:bg1, s:italicize_strings)
endif
" Boolean constant: TRUE, false
hi! link Boolean GruvculaPurple
" Number constant: 234, 0xff
hi! link Number GruvculaPurple
" Floating point constant: 2.3e10
hi! link Float GruvculaPurple

" Generic type
hi! link Type GruvculaYellow
" static, register, volatile, etc
hi! link StorageClass GruvculaOrange
" struct, union, enum, etc.
hi! link Structure GruvculaAqua
" typedef
hi! link Typedef GruvculaYellow

" }}}
" Completion Menu: {{{

if version >= 700
  " Popup menu: normal item
  call s:HL('Pmenu', s:fg1, s:bg2)
  " Popup menu: selected item
  call s:HL('PmenuSel', s:bg2, s:blue, s:bold)
  " Popup menu: scrollbar
  call s:HL('PmenuSbar', s:none, s:bg2)
  " Popup menu: scrollbar thumb
  call s:HL('PmenuThumb', s:none, s:bg4)
endif

" }}}
" Diffs: {{{

call s:HL('DiffDelete', s:red, s:bg0, s:inverse)
call s:HL('DiffAdd',    s:green, s:bg0, s:inverse)
"call s:HL('DiffChange', s:bg0, s:blue)
"call s:HL('DiffText',   s:bg0, s:yellow)

" Alternative setting
call s:HL('DiffChange', s:aqua, s:bg0, s:inverse)
call s:HL('DiffText',   s:yellow, s:bg0, s:inverse)

" }}}
" Spelling: {{{

if has("spell")
  " Not capitalised word, or compile warnings
  if g:gruvcula_improved_warnings == 0
    call s:HL('SpellCap',   s:none, s:none, s:undercurl, s:red)
  else
    call s:HL('SpellCap',   s:green, s:none, s:bold . s:italic)
  endif
  " Not recognized word
  call s:HL('SpellBad',   s:none, s:none, s:undercurl, s:blue)
  " Wrong spelling for selected region
  call s:HL('SpellLocal', s:none, s:none, s:undercurl, s:aqua)
  " Rare word
  call s:HL('SpellRare',  s:none, s:none, s:undercurl, s:purple)
endif

" }}}

" Plugin specific -------------------------------------------------------------
" EasyMotion: {{{

hi! link EasyMotionTarget Search
hi! link EasyMotionShade Comment

" }}}
" Sneak: {{{

hi! link Sneak Search
hi! link SneakLabel Search

" }}}
" Indent Guides: {{{

if !exists('g:indent_guides_auto_colors')
  let g:indent_guides_auto_colors = 0
endif

if g:indent_guides_auto_colors == 0
  if g:gruvcula_invert_indent_guides == 0
    call s:HL('IndentGuidesOdd', s:vim_bg, s:bg2)
    call s:HL('IndentGuidesEven', s:vim_bg, s:bg1)
  else
    call s:HL('IndentGuidesOdd', s:vim_bg, s:bg2, s:inverse)
    call s:HL('IndentGuidesEven', s:vim_bg, s:bg3, s:inverse)
  endif
endif

" }}}
" IndentLine: {{{

if !exists('g:indentLine_color_term')
  let g:indentLine_color_term = s:bg2[1]
endif
if !exists('g:indentLine_color_gui')
  let g:indentLine_color_gui = s:bg2[0]
endif

" }}}
" Rainbow Parentheses: {{{

if !exists('g:rbpt_colorpairs')
  let g:rbpt_colorpairs =
    \ [
      \ ['blue', '#458588'], ['magenta', '#b16286'],
      \ ['red',  '#cc241d'], ['166',     '#d65d0e']
    \ ]
endif

let g:rainbow_guifgs = [ '#d65d0e', '#cc241d', '#b16286', '#458588' ]
let g:rainbow_ctermfgs = [ '166', 'red', 'magenta', 'blue' ]

if !exists('g:rainbow_conf')
   let g:rainbow_conf = {}
endif
if !has_key(g:rainbow_conf, 'guifgs')
   let g:rainbow_conf['guifgs'] = g:rainbow_guifgs
endif
if !has_key(g:rainbow_conf, 'ctermfgs')
   let g:rainbow_conf['ctermfgs'] = g:rainbow_ctermfgs
endif

let g:niji_dark_colours = g:rbpt_colorpairs
let g:niji_light_colours = g:rbpt_colorpairs

"}}}
" GitGutter: {{{

hi! link GitGutterAdd GruvculaGreenSign
hi! link GitGutterChange GruvculaAquaSign
hi! link GitGutterDelete GruvculaRedSign
hi! link GitGutterChangeDelete GruvculaAquaSign

" }}}
" GitCommit: "{{{

hi! link gitcommitSelectedFile GruvculaGreen
hi! link gitcommitDiscardedFile GruvculaRed

" }}}
" Signify: {{{

hi! link SignifySignAdd GruvculaGreenSign
hi! link SignifySignChange GruvculaAquaSign
hi! link SignifySignDelete GruvculaRedSign

" }}}
" Syntastic: {{{

call s:HL('SyntasticError', s:none, s:none, s:undercurl, s:red)
call s:HL('SyntasticWarning', s:none, s:none, s:undercurl, s:yellow)

hi! link SyntasticErrorSign GruvculaRedSign
hi! link SyntasticWarningSign GruvculaYellowSign

" }}}
" Signature: {{{
hi! link SignatureMarkText   GruvculaBlueSign
hi! link SignatureMarkerText GruvculaPurpleSign

" }}}
" ShowMarks: {{{

hi! link ShowMarksHLl GruvculaBlueSign
hi! link ShowMarksHLu GruvculaBlueSign
hi! link ShowMarksHLo GruvculaBlueSign
hi! link ShowMarksHLm GruvculaBlueSign

" }}}
" CtrlP: {{{

hi! link CtrlPMatch GruvculaYellow
hi! link CtrlPNoEntries GruvculaRed
hi! link CtrlPPrtBase GruvculaBg2
hi! link CtrlPPrtCursor GruvculaBlue
hi! link CtrlPLinePre GruvculaBg2

call s:HL('CtrlPMode1', s:blue, s:bg2, s:bold)
call s:HL('CtrlPMode2', s:bg0, s:blue, s:bold)
call s:HL('CtrlPStats', s:fg4, s:bg2, s:bold)

" }}}
" Startify: {{{

hi! link StartifyBracket GruvculaFg3
hi! link StartifyFile GruvculaFg1
hi! link StartifyNumber GruvculaBlue
hi! link StartifyPath GruvculaGray
hi! link StartifySlash GruvculaGray
hi! link StartifySection GruvculaYellow
hi! link StartifySpecial GruvculaBg2
hi! link StartifyHeader GruvculaOrange
hi! link StartifyFooter GruvculaBg2

" }}}
" Vimshell: {{{

let g:vimshell_escape_colors = [
  \ s:bg4[0], s:red[0], s:green[0], s:yellow[0],
  \ s:blue[0], s:purple[0], s:aqua[0], s:fg4[0],
  \ s:bg0[0], s:red[0], s:green[0], s:orange[0],
  \ s:blue[0], s:purple[0], s:aqua[0], s:fg0[0]
  \ ]

" }}}
" BufTabLine: {{{

call s:HL('BufTabLineCurrent', s:bg0, s:fg4)
call s:HL('BufTabLineActive', s:fg4, s:bg2)
call s:HL('BufTabLineHidden', s:bg4, s:bg1)
call s:HL('BufTabLineFill', s:bg0, s:bg0)

" }}}
" Asynchronous Lint Engine: {{{

call s:HL('ALEError', s:none, s:none, s:undercurl, s:red)
call s:HL('ALEWarning', s:none, s:none, s:undercurl, s:yellow)
call s:HL('ALEInfo', s:none, s:none, s:undercurl, s:blue)

hi! link ALEErrorSign GruvculaRedSign
hi! link ALEWarningSign GruvculaYellowSign
hi! link ALEInfoSign GruvculaBlueSign

" }}}
" Dirvish: {{{

hi! link DirvishPathTail GruvculaAqua
hi! link DirvishArg GruvculaYellow

" }}}
" Netrw: {{{

hi! link netrwDir GruvculaAqua
hi! link netrwClassify GruvculaAqua
hi! link netrwLink GruvculaGray
hi! link netrwSymLink GruvculaFg1
hi! link netrwExe GruvculaYellow
hi! link netrwComment GruvculaGray
hi! link netrwList GruvculaBlue
hi! link netrwHelpCmd GruvculaAqua
hi! link netrwCmdSep GruvculaFg3
hi! link netrwVersion GruvculaGreen

" }}}
" NERDTree: {{{

hi! link NERDTreeDir GruvculaAqua
hi! link NERDTreeDirSlash GruvculaAqua

hi! link NERDTreeOpenable GruvculaOrange
hi! link NERDTreeClosable GruvculaOrange

hi! link NERDTreeFile GruvculaFg1
hi! link NERDTreeExecFile GruvculaYellow

hi! link NERDTreeUp GruvculaGray
hi! link NERDTreeCWD GruvculaGreen
hi! link NERDTreeHelp GruvculaFg1

hi! link NERDTreeToggleOn GruvculaGreen
hi! link NERDTreeToggleOff GruvculaRed

" }}}
" Vim Multiple Cursors: {{{

call s:HL('multiple_cursors_cursor', s:none, s:none, s:inverse)
call s:HL('multiple_cursors_visual', s:none, s:bg2)

" }}}

" Filetype specific -----------------------------------------------------------
" Diff: {{{

hi! link diffAdded GruvculaGreen
hi! link diffRemoved GruvculaRed
hi! link diffChanged GruvculaAqua

hi! link diffFile GruvculaOrange
hi! link diffNewFile GruvculaYellow

hi! link diffLine GruvculaBlue

" }}}
" Html: {{{

hi! link htmlTag GruvculaBlue
hi! link htmlEndTag GruvculaBlue

hi! link htmlTagName GruvculaAquaBold
hi! link htmlArg GruvculaAqua

hi! link htmlScriptTag GruvculaPurple
hi! link htmlTagN GruvculaFg1
hi! link htmlSpecialTagName GruvculaAquaBold

call s:HL('htmlLink', s:fg4, s:none, s:underline)

hi! link htmlSpecialChar GruvculaOrange

call s:HL('htmlBold', s:vim_fg, s:vim_bg, s:bold)
call s:HL('htmlBoldUnderline', s:vim_fg, s:vim_bg, s:bold . s:underline)
call s:HL('htmlBoldItalic', s:vim_fg, s:vim_bg, s:bold . s:italic)
call s:HL('htmlBoldUnderlineItalic', s:vim_fg, s:vim_bg, s:bold . s:underline . s:italic)

call s:HL('htmlUnderline', s:vim_fg, s:vim_bg, s:underline)
call s:HL('htmlUnderlineItalic', s:vim_fg, s:vim_bg, s:underline . s:italic)
call s:HL('htmlItalic', s:vim_fg, s:vim_bg) ", s:italic)

" }}}
" Xml: {{{

hi! link xmlTag GruvculaBlue
hi! link xmlEndTag GruvculaBlue
hi! link xmlTagName GruvculaBlue
hi! link xmlEqual GruvculaBlue
hi! link docbkKeyword GruvculaAquaBold

hi! link xmlDocTypeDecl GruvculaGray
hi! link xmlDocTypeKeyword GruvculaPurple
hi! link xmlCdataStart GruvculaGray
hi! link xmlCdataCdata GruvculaPurple
hi! link dtdFunction GruvculaGray
hi! link dtdTagName GruvculaPurple

hi! link xmlAttrib GruvculaAqua
hi! link xmlProcessingDelim GruvculaGray
hi! link dtdParamEntityPunct GruvculaGray
hi! link dtdParamEntityDPunct GruvculaGray
hi! link xmlAttribPunct GruvculaGray

hi! link xmlEntity GruvculaOrange
hi! link xmlEntityPunct GruvculaOrange
" }}}
" Vim: {{{

call s:HL('vimCommentTitle', s:fg4_256, s:none, s:bold . s:italicize_comments)

hi! link vimNotation GruvculaOrange
hi! link vimBracket GruvculaOrange
hi! link vimMapModKey GruvculaOrange
hi! link vimFuncSID GruvculaFg3
hi! link vimSetSep GruvculaFg3
hi! link vimSep GruvculaFg3
hi! link vimContinue GruvculaFg3

" }}}
" Clojure: {{{

hi! link clojureKeyword GruvculaBlue
hi! link clojureCond GruvculaOrange
hi! link clojureSpecial GruvculaOrange
hi! link clojureDefine GruvculaOrange

hi! link clojureFunc GruvculaYellow
hi! link clojureRepeat GruvculaYellow
hi! link clojureCharacter GruvculaAqua
hi! link clojureStringEscape GruvculaAqua
hi! link clojureException GruvculaRed

hi! link clojureRegexp GruvculaAqua
hi! link clojureRegexpEscape GruvculaAqua
call s:HL('clojureRegexpCharClass', s:fg3, s:none, s:bold)
hi! link clojureRegexpMod clojureRegexpCharClass
hi! link clojureRegexpQuantifier clojureRegexpCharClass

hi! link clojureParen GruvculaFg3
hi! link clojureAnonArg GruvculaYellow
hi! link clojureVariable GruvculaBlue
hi! link clojureMacro GruvculaOrange

hi! link clojureMeta GruvculaYellow
hi! link clojureDeref GruvculaYellow
hi! link clojureQuote GruvculaYellow
hi! link clojureUnquote GruvculaYellow

" }}}
" C: {{{

hi! link cOperator GruvculaPurple
hi! link cStructure GruvculaOrange

" }}}
" Python: {{{

hi! link pythonBuiltin GruvculaOrange
hi! link pythonBuiltinObj GruvculaOrange
hi! link pythonBuiltinFunc GruvculaOrange
hi! link pythonFunction GruvculaAqua
hi! link pythonDecorator GruvculaRed
hi! link pythonInclude GruvculaBlue
hi! link pythonImport GruvculaBlue
hi! link pythonRun GruvculaBlue
hi! link pythonCoding GruvculaBlue
hi! link pythonOperator GruvculaRed
hi! link pythonException GruvculaRed
hi! link pythonExceptions GruvculaPurple
hi! link pythonBoolean GruvculaPurple
hi! link pythonDot GruvculaFg3
hi! link pythonConditional GruvculaRed
hi! link pythonRepeat GruvculaRed
hi! link pythonDottedName GruvculaGreenBold

" }}}
" CSS: {{{

hi! link cssBraces GruvculaBlue
hi! link cssFunctionName GruvculaYellow
hi! link cssIdentifier GruvculaOrange
hi! link cssClassName GruvculaGreen
hi! link cssColor GruvculaBlue
hi! link cssSelectorOp GruvculaBlue
hi! link cssSelectorOp2 GruvculaBlue
hi! link cssImportant GruvculaGreen
hi! link cssVendor GruvculaFg1

hi! link cssTextProp GruvculaAqua
hi! link cssAnimationProp GruvculaAqua
hi! link cssUIProp GruvculaYellow
hi! link cssTransformProp GruvculaAqua
hi! link cssTransitionProp GruvculaAqua
hi! link cssPrintProp GruvculaAqua
hi! link cssPositioningProp GruvculaYellow
hi! link cssBoxProp GruvculaAqua
hi! link cssFontDescriptorProp GruvculaAqua
hi! link cssFlexibleBoxProp GruvculaAqua
hi! link cssBorderOutlineProp GruvculaAqua
hi! link cssBackgroundProp GruvculaAqua
hi! link cssMarginProp GruvculaAqua
hi! link cssListProp GruvculaAqua
hi! link cssTableProp GruvculaAqua
hi! link cssFontProp GruvculaAqua
hi! link cssPaddingProp GruvculaAqua
hi! link cssDimensionProp GruvculaAqua
hi! link cssRenderProp GruvculaAqua
hi! link cssColorProp GruvculaAqua
hi! link cssGeneratedContentProp GruvculaAqua

" }}}
" JavaScript: {{{

hi! link javaScriptBraces GruvculaFg1
hi! link javaScriptFunction GruvculaAqua
hi! link javaScriptIdentifier GruvculaRed
hi! link javaScriptMember GruvculaBlue
hi! link javaScriptNumber GruvculaPurple
hi! link javaScriptNull GruvculaPurple
hi! link javaScriptParens GruvculaFg3

" }}}
" YAJS: {{{

hi! link javascriptImport GruvculaAqua
hi! link javascriptExport GruvculaAqua
hi! link javascriptClassKeyword GruvculaAqua
hi! link javascriptClassExtends GruvculaAqua
hi! link javascriptDefault GruvculaAqua

hi! link javascriptClassName GruvculaYellow
hi! link javascriptClassSuperName GruvculaYellow
hi! link javascriptGlobal GruvculaYellow

hi! link javascriptEndColons GruvculaFg1
hi! link javascriptFuncArg GruvculaFg1
hi! link javascriptGlobalMethod GruvculaFg1
hi! link javascriptNodeGlobal GruvculaFg1
hi! link javascriptBOMWindowProp GruvculaFg1
hi! link javascriptArrayMethod GruvculaFg1
hi! link javascriptArrayStaticMethod GruvculaFg1
hi! link javascriptCacheMethod GruvculaFg1
hi! link javascriptDateMethod GruvculaFg1
hi! link javascriptMathStaticMethod GruvculaFg1

" hi! link javascriptProp GruvculaFg1
hi! link javascriptURLUtilsProp GruvculaFg1
hi! link javascriptBOMNavigatorProp GruvculaFg1
hi! link javascriptDOMDocMethod GruvculaFg1
hi! link javascriptDOMDocProp GruvculaFg1
hi! link javascriptBOMLocationMethod GruvculaFg1
hi! link javascriptBOMWindowMethod GruvculaFg1
hi! link javascriptStringMethod GruvculaFg1

hi! link javascriptVariable GruvculaOrange
" hi! link javascriptVariable GruvculaRed
" hi! link javascriptIdentifier GruvculaOrange
" hi! link javascriptClassSuper GruvculaOrange
hi! link javascriptIdentifier GruvculaOrange
hi! link javascriptClassSuper GruvculaOrange

" hi! link javascriptFuncKeyword GruvculaOrange
" hi! link javascriptAsyncFunc GruvculaOrange
hi! link javascriptFuncKeyword GruvculaAqua
hi! link javascriptAsyncFunc GruvculaAqua
hi! link javascriptClassStatic GruvculaOrange

hi! link javascriptOperator GruvculaRed
hi! link javascriptForOperator GruvculaRed
hi! link javascriptYield GruvculaRed
hi! link javascriptExceptions GruvculaRed
hi! link javascriptMessage GruvculaRed

hi! link javascriptTemplateSB GruvculaAqua
hi! link javascriptTemplateSubstitution GruvculaFg1

" hi! link javascriptLabel GruvculaBlue
" hi! link javascriptObjectLabel GruvculaBlue
" hi! link javascriptPropertyName GruvculaBlue
hi! link javascriptLabel GruvculaFg1
hi! link javascriptObjectLabel GruvculaFg1
hi! link javascriptPropertyName GruvculaFg1

hi! link javascriptLogicSymbols GruvculaFg1
hi! link javascriptArrowFunc GruvculaYellow

hi! link javascriptDocParamName GruvculaFg4
hi! link javascriptDocTags GruvculaFg4
hi! link javascriptDocNotation GruvculaFg4
hi! link javascriptDocParamType GruvculaFg4
hi! link javascriptDocNamedParamType GruvculaFg4

hi! link javascriptBrackets GruvculaFg1
hi! link javascriptDOMElemAttrs GruvculaFg1
hi! link javascriptDOMEventMethod GruvculaFg1
hi! link javascriptDOMNodeMethod GruvculaFg1
hi! link javascriptDOMStorageMethod GruvculaFg1
hi! link javascriptHeadersMethod GruvculaFg1

hi! link javascriptAsyncFuncKeyword GruvculaRed
hi! link javascriptAwaitFuncKeyword GruvculaRed

" }}}
" PanglossJS: {{{

hi! link jsClassKeyword GruvculaAqua
hi! link jsExtendsKeyword GruvculaAqua
hi! link jsExportDefault GruvculaAqua
hi! link jsTemplateBraces GruvculaAqua
hi! link jsGlobalNodeObjects GruvculaFg1
hi! link jsGlobalObjects GruvculaFg1
hi! link jsFunction GruvculaAqua
hi! link jsFuncParens GruvculaFg3
hi! link jsParens GruvculaFg3
hi! link jsNull GruvculaPurple
hi! link jsUndefined GruvculaPurple
hi! link jsClassDefinition GruvculaYellow

" }}}
" TypeScript: {{{

hi! link typeScriptReserved GruvculaAqua
hi! link typeScriptLabel GruvculaAqua
hi! link typeScriptFuncKeyword GruvculaAqua
hi! link typeScriptIdentifier GruvculaOrange
hi! link typeScriptBraces GruvculaFg1
hi! link typeScriptEndColons GruvculaFg1
hi! link typeScriptDOMObjects GruvculaFg1
hi! link typeScriptAjaxMethods GruvculaFg1
hi! link typeScriptLogicSymbols GruvculaFg1
hi! link typeScriptDocSeeTag Comment
hi! link typeScriptDocParam Comment
hi! link typeScriptDocTags vimCommentTitle
hi! link typeScriptGlobalObjects GruvculaFg1
hi! link typeScriptParens GruvculaFg3
hi! link typeScriptOpSymbols GruvculaFg3
hi! link typeScriptHtmlElemProperties GruvculaFg1
hi! link typeScriptNull GruvculaPurple
hi! link typeScriptInterpolationDelimiter GruvculaAqua

" }}}
" PureScript: {{{

hi! link purescriptModuleKeyword GruvculaAqua
hi! link purescriptModuleName GruvculaFg1
hi! link purescriptWhere GruvculaAqua
hi! link purescriptDelimiter GruvculaFg4
hi! link purescriptType GruvculaFg1
hi! link purescriptImportKeyword GruvculaAqua
hi! link purescriptHidingKeyword GruvculaAqua
hi! link purescriptAsKeyword GruvculaAqua
hi! link purescriptStructure GruvculaAqua
hi! link purescriptOperator GruvculaBlue

hi! link purescriptTypeVar GruvculaFg1
hi! link purescriptConstructor GruvculaFg1
hi! link purescriptFunction GruvculaFg1
hi! link purescriptConditional GruvculaOrange
hi! link purescriptBacktick GruvculaOrange

" }}}
" CoffeeScript: {{{

hi! link coffeeExtendedOp GruvculaFg3
hi! link coffeeSpecialOp GruvculaFg3
hi! link coffeeCurly GruvculaOrange
hi! link coffeeParen GruvculaFg3
hi! link coffeeBracket GruvculaOrange

" }}}
" Ruby: {{{

hi! link rubyStringDelimiter GruvculaGreen
hi! link rubyInterpolationDelimiter GruvculaAqua

" }}}
" ObjectiveC: {{{

hi! link objcTypeModifier GruvculaRed
hi! link objcDirective GruvculaBlue

" }}}
" Go: {{{

hi! link goDirective GruvculaAqua
hi! link goConstants GruvculaPurple
hi! link goDeclaration GruvculaRed
hi! link goDeclType GruvculaBlue
hi! link goBuiltins GruvculaOrange

" }}}
" Lua: {{{

hi! link luaIn GruvculaRed
hi! link luaFunction GruvculaAqua
hi! link luaTable GruvculaOrange

" }}}
" MoonScript: {{{

hi! link moonSpecialOp GruvculaFg3
hi! link moonExtendedOp GruvculaFg3
hi! link moonFunction GruvculaFg3
hi! link moonObject GruvculaYellow

" }}}
" Java: {{{

hi! link javaAnnotation GruvculaBlue
hi! link javaDocTags GruvculaAqua
hi! link javaCommentTitle vimCommentTitle
hi! link javaParen GruvculaFg3
hi! link javaParen1 GruvculaFg3
hi! link javaParen2 GruvculaFg3
hi! link javaParen3 GruvculaFg3
hi! link javaParen4 GruvculaFg3
hi! link javaParen5 GruvculaFg3
hi! link javaOperator GruvculaOrange

hi! link javaVarArg GruvculaGreen

" }}}
" Elixir: {{{

hi! link elixirDocString Comment

hi! link elixirStringDelimiter GruvculaGreen
hi! link elixirInterpolationDelimiter GruvculaAqua

hi! link elixirModuleDeclaration GruvculaYellow

" }}}
" Scala: {{{

" NB: scala vim syntax file is kinda horrible
hi! link scalaNameDefinition GruvculaFg1
hi! link scalaCaseFollowing GruvculaFg1
hi! link scalaCapitalWord GruvculaFg1
hi! link scalaTypeExtension GruvculaFg1

hi! link scalaKeyword GruvculaRed
hi! link scalaKeywordModifier GruvculaRed

hi! link scalaSpecial GruvculaAqua
hi! link scalaOperator GruvculaFg1

hi! link scalaTypeDeclaration GruvculaYellow
hi! link scalaTypeTypePostDeclaration GruvculaYellow

hi! link scalaInstanceDeclaration GruvculaFg1
hi! link scalaInterpolation GruvculaAqua

" }}}
" Markdown: {{{

call s:HL('markdownItalic', s:fg3, s:none) ", s:italic)

hi! link markdownH1 GruvculaGreenBold
hi! link markdownH2 GruvculaGreenBold
hi! link markdownH3 GruvculaYellowBold
hi! link markdownH4 GruvculaYellowBold
hi! link markdownH5 GruvculaYellow
hi! link markdownH6 GruvculaYellow

hi! link markdownCode GruvculaAqua
hi! link markdownCodeBlock GruvculaAqua
hi! link markdownCodeDelimiter GruvculaAqua

hi! link markdownBlockquote GruvculaGray
hi! link markdownListMarker GruvculaGray
hi! link markdownOrderedListMarker GruvculaGray
hi! link markdownRule GruvculaGray
hi! link markdownHeadingRule GruvculaGray

hi! link markdownUrlDelimiter GruvculaFg3
hi! link markdownLinkDelimiter GruvculaFg3
hi! link markdownLinkTextDelimiter GruvculaFg3

hi! link markdownHeadingDelimiter GruvculaOrange
hi! link markdownUrl GruvculaPurple
hi! link markdownUrlTitleDelimiter GruvculaGreen

call s:HL('markdownLinkText', s:gray, s:none, s:underline)
hi! link markdownIdDeclaration markdownLinkText

" }}}
" Haskell: {{{

" hi! link haskellType GruvculaYellow
" hi! link haskellOperators GruvculaOrange
" hi! link haskellConditional GruvculaAqua
" hi! link haskellLet GruvculaOrange
"
hi! link haskellType GruvculaFg1
hi! link haskellIdentifier GruvculaFg1
hi! link haskellSeparator GruvculaFg1
hi! link haskellDelimiter GruvculaFg4
hi! link haskellOperators GruvculaBlue
"
hi! link haskellBacktick GruvculaOrange
hi! link haskellStatement GruvculaOrange
hi! link haskellConditional GruvculaOrange

hi! link haskellLet GruvculaAqua
hi! link haskellDefault GruvculaAqua
hi! link haskellWhere GruvculaAqua
hi! link haskellBottom GruvculaAqua
hi! link haskellBlockKeywords GruvculaAqua
hi! link haskellImportKeywords GruvculaAqua
hi! link haskellDeclKeyword GruvculaAqua
hi! link haskellDeriving GruvculaAqua
hi! link haskellAssocType GruvculaAqua

hi! link haskellNumber GruvculaPurple
hi! link haskellPragma GruvculaPurple

hi! link haskellString GruvculaGreen
hi! link haskellChar GruvculaGreen

" }}}
" Json: {{{

hi! link jsonKeyword GruvculaGreen
hi! link jsonQuote GruvculaGreen
hi! link jsonBraces GruvculaFg1
hi! link jsonString GruvculaFg1

" }}}


" Functions -------------------------------------------------------------------
" Search Highlighting Cursor {{{

function! GruvculaHlsShowCursor()
  call s:HL('Cursor', s:bg0, s:hls_cursor)
endfunction

function! GruvculaHlsHideCursor()
  call s:HL('Cursor', s:none, s:none, s:inverse)
endfunction

" }}}

" vim: set sw=2 ts=2 sts=2 et tw=80 ft=vim fdm=marker:
