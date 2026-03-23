"===============================================================================
"
"                                 \/       \/
"                                 /\_______/\
"                                /   o   o   \
"                               (  ==  ^  ==  )
"                                )           (
"                               (             )
"                               ( (  )   (  ) )
"                              (__(__)___(__)__)
"                                 O l i v e r
"
"===============================================================================
"
" Theo's .vimrc based on:
" =====================================================================
" =========================== KICKSTART.VIM ===========================
" =====================================================================
" ========                                    .-----.          ========
" ========         .----------------------.   | === |          ========
" ========         |.-""""""""""""""""""-.|   |-----|          ========
" ========         || GITHUB.COM/THEOPN/ ||   | === |          ========
" ========         ||   KICKSTART.VIM    ||   |-----|          ========
" ========         ||                    ||   | === |          ========
" ========                                       ---|          ========
" ========                                        ::|          ========
" =====================================================================
"

" {{{ Options
let mapleader=' '
let maplocalleader = ' '

filetype on
syntax on

" Text editing
set backspace=indent,eol,start
set completeopt=menuone,noselect
set encoding=utf-8
set nojoinspaces  " prevent automatic space insertion when using J

" Behavior
set autoread  " reflect changes made outside of Vim
set autochdir  " change CWD to the parent dir of the buf
set belloff=all
set confirm  " ask to save
set mouse=a
set nostartofline  " keep the cursor in the same column when navigating
set scrolloff=10
set matchpairs+=<:>,=:;  " pairs to register for %

" Buffer
set hidden switchbuf=uselast

" Command
set history=10000 wildmenu
set wildmode=noselect:full  " do not auto-select and complete the full match

" Performance
set updatetime=250 timeoutlen=300

" Indentation
" copies indentation of prev line, read help for smarttab
set autoindent smarttab
set tabstop=2 shiftwidth=0 expandtab  " shiftwidth follows tabstop
set shiftround  " indent to a multiple of shiftwidth when using <. >

" Appearance
set background=dark
set breakindent  " give wrapped lines visual indentation
let &showbreak="> "  " indicate wrapped lines
set display=lastline  " display @@@ in the truncated last line
set ruler
set number relativenumber
set laststatus=2 showtabline=2 signcolumn=yes showcmd
set list listchars=tab:»\ ,trail:·,nbsp:␣,leadmultispace:\|\  " lms req. > 0.9
set fillchars=stlnc:-
set cursorline cursorcolumn colorcolumn=80

" Search
set incsearch hlsearch
set ignorecase smartcase
set shortmess-=S  " enable search count in the ruler (off by default)

" Split
set splitright splitbelow

" Spell
set spell spelllang=en_us spellsuggest=best,8 spelloptions=camel

" Fold
set foldmethod=marker foldlevel=0
" }}}


" {{{ Keymaps
" Default overrides
nnoremap <expr> <silent> k v:count == 0 ? 'gk' : 'k'
nnoremap <expr> <silent> j v:count == 0 ? 'gj' : 'j'
nnoremap n nzz
nnoremap N Nzz
nnoremap <C-d> <C-d>zz
nnoremap <C-u> <C-u>zz
nnoremap <C-w>+ <C-w>+:call feedkeys("\<lt>C-w>")<CR>
nnoremap <C-w>- <C-w>-:call feedkeys("\<lt>C-w>")<CR>

" ESC's
inoremap <silent> jk <ESC>
nnoremap <Esc> :nohlsearch<CR>
tnoremap <Esc><Esc> <C-\><C-n>

" Auto pairs
inoremap ( ()<LEFT>
inoremap [ []<LEFT>
inoremap {<CR> {<CR>}<ESC>ko<TAB>

" Copy and paste
nnoremap <leader>a gg<S-v>G
xnoremap <leader>y "+y
nnoremap <leader>p :reg<CR>
      \:echo '[Theovim] e.g., `:normal "+p` to paste from the register +'<CR>
      \:normal "
xnoremap <leader>p "_dP

" Emacs navigation
inoremap <C-a> <C-o>^
inoremap <C-b> <LEFT>
inoremap <C-f> <RIGHT>

" Terminal
nnoremap <leader>t :botright term<CR>

" Spell check
inoremap <C-s> <C-g>u<ESC>[s1z=`]a<C-g>u

" Emulating Vimwiki's link feature
nnoremap <expr> <C-n><C-n> ':e ' . expand('%:h') . '/<C-r><C-f>.md'

" Buffer
nnoremap <leader>b :ls<CR>:b<SPACE>
nnoremap [b :bprevious<CR>
nnoremap ]b :bnext<CR>
nnoremap <leader>k :ls<CR>
      \: echo '[Theovim] Choose a buf to delete
      \(blank: choose curr buf, RET: confirm, ESC: cancel)'<CR>
      \:bdelete<SPACE>

" Window
fun! SmartWinMove(key)
  let t:currwin = winnr()
  exec 'wincmd ' . a:key
  if t:currwin == winnr()
    if a:key == 'h' || a:key == 'l'
      wincmd v
    else
      wincmd s
    endif
    exec 'wincmd ' . a:key
  endif
endfun
nnoremap <C-h> :call SmartWinMove('h')<CR>
nnoremap <C-j> :call SmartWinMove('j')<CR>
nnoremap <C-k> :call SmartWinMove('k')<CR>
nnoremap <C-l> :call SmartWinMove('l')<CR>
" }}}


" {{{ Commands
fun! TrimWhitespace()
  let l:save = winsaveview()
  keeppatterns %s/\s\+$//ec
  call winrestview(l:save)
endfun
command TrimWhiespace call TrimWhitespace()

command CD :lcd %:h
command RO :setlocal readonly nomodifiable
" }}}


" {{{ netrw
let g:netrw_banner = 0 " No guide on the top, turn on if error occurs
let g:netrw_liststyle = 1 " 0 simple, 1 detailed, 2 thick, 3 tree
let g:netrw_browse_split = 3
let g:netrw_winsize = 15
augroup netrw_autocmds
  autocmd!
  " Close netrw if it is the last buffer
  autocmd WinEnter * if tabpagenr('$') == 1 && winnr('$') == 1
  \ && getbufvar(winbufnr(winnr()), "&filetype") == 'netrw' | quit | endif
  " autocmd VimEnter * :Lexplore! | wincmd p  " Netrw open on startup
augroup END

" Function to toggle netrw buffer using global var and buf wipeout
let g:NetrwIsOpen = 0 " Set this to 1 if netrw is open on the startup
fun! ToggleNetrw()
  if g:NetrwIsOpen
    for i in range(1, bufnr('$'))
      if getbufvar(i, "&filetype") == 'netrw'
        silent exe 'bwipeout ' . i
      endif
    endfor
    let g:NetrwIsOpen = 0
  else
    let g:NetrwIsOpen = 1
    silent Lex!
  endif
endfun

nnoremap <leader>n :call ToggleNetrw()<CR>
" }}}


" {{{ Colorscheme
set termguicolors
colorscheme sorbet
" }}}


" {{{ Statusline
let g:currentmode={
  \ 'n' : 'N', 'no' : 'N OPERATOR PENDING',
  \ 'v' : 'V', 'V' : 'V LINE', "\<C-V>" : 'V BLOCK',
  \ 's' : 'SELECT', 'S' : 'S LINE', "\<C-S>" : 'S BLOCK',
  \ 'i' : 'I', 'R' : 'R', 'Rv' : 'V REPLACE',
  \ 'c' : 'CMD', 'cv' : 'VIM EX', 'ce' : 'EX',
  \ 'r' : 'PROMPT', 'rm' : 'MORE', 'r?' : 'CONFIRM',
  \ '!' : 'SH', 't' : 'TERM', 'nt' : 'N TERM' }
set laststatus=2                                      " Always show statusline
set statusline=                                       " Reset w/ nothing
set statusline+=\ [%{toupper(g:currentmode[mode()])}] " Current mode
set statusline+=\ \|                                  " Space + Bar
set statusline+=\ %{fnamemodify(getcwd(),':t')}       " Current working dir
set statusline+=\ %f                                  " Current buffer name
set statusline+=\ %m                                  " [+] for modified
set statusline+=%r                                    " [RO] for read only
set statusline+=\|                                    " Bar
set statusline+=%=                                    " Spacer
set statusline+=%<                                    " Truncation point
set statusline+=Messing\ w\ vimrc\ again?             " Yes I am
set statusline+=\ %{'>^•-•^<'}                        " Why in {}? Why not
set statusline+=%=                                    " Spacer
set statusline+=\ \|                                  " Space + Bar
set statusline+=\ %{(&expandtab==1?'SPC':'TAB')}      " Tab or space
set statusline+=\ \|                                  " Space + Bar
set statusline+=\ FT:\ %Y                             " Filetype
set statusline+=\ \|                                  " Space + Bar
set statusline+=\ %{toupper(&ff)}                     " File format
set statusline+=:                                     " Colon
set statusline+=%{(&fenc!=''?&fenc:&enc)}             " Fileencoding or encoding
set statusline+=\ \|                                  " Space + Bar
set statusline+=\ @                                   " At
set statusline+=\ %l                                  " Current line num
set statusline+=:                                     " Colon
set statusline+=%c                                    " Current column num
set statusline+=\ %P                                  " Percent file displayed
set statusline+=\ \|%{''}                             " Space, bar, empty char
" }}}


" {{{ "Bufferline": https://theopark.me/blog/2023-03-17-vimscript-bufferline/
set showtabline=2

fun! SpawnBufferLine()
  let s = ' ₍^. .^₎⟆ '

  " Gets the list of buffers. Use bufexists() to include hidden buffers
  let bufferNums = filter(range(1, bufnr('$')), 'buflisted(v:val)')
  for i in bufferNums
    let s .= '%#TabLineFill#%T|'  " Resets highlight and adds a left separator

    " Highlights if it's the current buffer
    let s .= (i == bufnr()) ? ('%#TabLineSel#') : ('%#TabLine#')
    let s .= ' ' . i . ' '  " Appends the buffer number

    " Gives a [NEW] flag to an unnamed buffer
    if bufname(i) == ''
      let s .= '[NEW]'
    endif

    if getbufvar(i, "&modifiable")
      let s .= fnamemodify(bufname(i), ':t')  " Appends the file name
      " let s .= pathshorten(bufname(i))  " Use this if you want a trimmed path

      " If the buffer is modified, add [+] flag
      if getbufvar(i, "&modified")
        let s .= ' [+]'
      endif

    else
      let s .= fnamemodify(bufname(i), ':t') . ' [RO]'  " Add read only flag
    endif

    let s .= ' %#TabLineFill#%T'  " Resets highlight and adds a spacer
  endfor
  let s .= '|'  " Adds a right separator to the last buffer

  let s .= '%='  " Adds a spacer

  " Making a tab list on the right side
  for i in range(1, tabpagenr('$'))  " Loop through the number of tabs
    " Highlight with yellow if it's the current tab
    let s .= (i == tabpagenr()) ? ('%#TabLineSel#') : ('%#TabLine#')
    let s .= '%' . i . 'T '  " set the tab page number (for mouse clicks)
    let s .= i . ''          " set page number string
  endfor
  let s .= '%#TabLineFill#%T'  " Reset highlight

  " Close button on the right if there are multiple tabs
  if tabpagenr('$') > 1
    let s .= '%999X X'
  endif
  return s
endfun

set tabline=%!SpawnBufferLine()  " Assign the tabline
" }}}


" vim: ts=2 sts=2 sw=2 et
