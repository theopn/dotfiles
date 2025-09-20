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
set wildmode=full  " complete the full match

" Performance
set updatetime=250 timeoutlen=300

" Indentation
" copies indentation of prev line, read help for smarttab
set autoindent smarttab
set tabstop=2 shiftwidth=2 expandtab  " C-centric indentation settings
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

 " Terminal
 nnoremap <leader>t :FloatermToggle<CR>

" Spell check
inoremap <C-s> <C-g>u<ESC>[s1z=`]a<C-g>u

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
" }}}


" {{{ Custom Bufferline & Bufferpanel
" Files located in .vim/plugin/
" Choose between (Bufferpanel + Tabline) || (Tabpanel + Bufferpanel)
"let g:theoline_buflist = 1
let g:theopanel_buflist = 1
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
  autocmd VimEnter * :Lexplore! | wincmd p  " Netrw open on startup
augroup END

" Function to toggle netrw buffer using global var and buf wipeout
let g:NetrwIsOpen = 1 " Since I open netrw in the startup
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


" {{{ vim-plug
let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin()
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-commentary'
Plug 'airblade/vim-gitgutter'
Plug 'liuchengxu/vim-which-key'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-lsp.vim'
Plug 'ghifarit53/tokyonight-vim'
Plug 'nordtheme/vim'
Plug 'vim-airline/vim-airline'

Plug 'vimwiki/vimwiki'
Plug 'voldikss/vim-floaterm'
call plug#end()
" }}}


" {{{ colorscheme
set termguicolors
let g:tokyonight_style = 'night'  " available: night, storm
let g:tokyonight_enable_italic = 0
colorscheme tokyonight
" colorscheme nord


" }}}


" {{{ vim-which-key
call which_key#register('<Space>', "g:which_key_map")
nnoremap <silent> <leader>      :<c-u>WhichKey '<Space>'<CR>
nnoremap <silent> <localleader> :<c-u>WhichKey  '<Space>'<CR>

let g:which_key_map =  {}
let g:which_key_map.s = { 'name' : '[S]earch' }
let g:which_key_map.h = { 'name' : 'Git [H]unk' }

" Adding non-plugin keybindings defined above
let g:which_key_map.a = 'Select all'
let g:which_key_map.b = 'Switch buffer'
let g:which_key_map.k = 'Kill buffer'
let g:which_key_map.n = 'Toggle netrw'
let g:which_key_map.p = 'Choose from a register'
" }}}


" {{{ fzf.vim
nmap <leader>sh :Helptags<CR>
let g:which_key_map.s.h = '[S]earch [H]elp'
nmap <leader>sk :Maps<CR>
let g:which_key_map.s.k = '[S]earch [K]eymaps'
nmap <leader>sf :Files<CR>
let g:which_key_map.s.f = '[S]earch [F]iles'
nmap <leader>sg :Rg<CR>
let g:which_key_map.s.g = '[S]earch by [G]rep'
nmap <leader>s. :History<CR>
let g:which_key_map.s['.'] = '[S]earch Recent Files ("." for repeat)'
nmap <leader><leader> :Buffers<CR>
let g:which_key_map[' '] = '[ ] Find existing buffers'
nmap <leader>/ :BLines<CR>
let g:which_key_map['/'] = '[/] Fuzzily search in current buffer'
" }}}


" {{{ vim-lsp
let g:lsp_use_native_client = 1  " Requires Vim 8.2+
let g:lsp_semantic_enabled = 0  " Can cause a lot of lag

function! s:on_lsp_buffer_enabled() abort
  setlocal omnifunc=lsp#complete
  if exists('+tagfunc') | setlocal tagfunc=lsp#tagfunc | endif

  nnoremap <buffer> [d <plug>(lsp-previous-diagnostic)
  nnoremap <buffer> ]d <plug>(lsp-next-diagnostic)
  nnoremap <buffer> K <plug>(lsp-hover)
  nnoremap <buffer> grn <plug>(lsp-rename)
  nnoremap <buffer> gra <plug>(lsp-code-action-float)
  nnoremap <buffer> grr <plug>(lsp-references)
  nnoremap <buffer> gri <plug>(lsp-implementation)
  nnoremap <buffer> gO <plug>(lsp-document-symbol-search)
  nnoremap <buffer> <C-s> <plug>(lsp-signature-help)
  nnoremap <buffer> grd <plug>(lsp-definition)
  nnoremap <buffer> grD <plug>(lsp-declaration)
  nnoremap <buffer> grt <plug>(lsp-peek-type-definition)
  nnoremap <buffer> gW <plug>(lsp-workspace-symbol-search)

  let g:lsp_format_sync_timeout = 1000
  nnoremap <buffer> <leader>f <plug>(lsp-document-format)
  let g:which_key_map.f = '[F]ormat buffer'
endfunction

augroup lsp_install
  au!
  autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END
" }}}


" {{{ asyncomplete.vim
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <CR>    pumvisible() ? asyncomplete#close_popup() : "\<CR>""

let g:asyncomplete_auto_completeopt = 0
set completeopt=menuone,noinsert,noselect,preview
" }}}


" {{{ Vimwiki
let g:vimwiki_list = [{ 'path': '~/Nextcloud/theo-vimwiki/',
       \ 'syntax':'markdown', 'ext': '.md' }]
let g:vimwiki_global_ext = 1
" }}}


" vim: ts=2 sts=2 sw=2 et
