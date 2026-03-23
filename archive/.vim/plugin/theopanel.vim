" vi:syntax=vim
" -----------------------------------------------------------------------------
" ___         _
"  ||_  _  _ |_)_.._  _ |
"  || |(/_(_)| (_|| |(/_|
"
" Theo's custom simple Tabpanel and "Bufferpanel"
"
" Requires:
" - Vim > 9.1.1391 for 'tabpanel' support
" - Nerd Font for rendering the window icon
"
" By default, minimal tabpanel with the list of tabs and number of windows
" is loaded.
" Define g:theopanel_buflist to switch to the Bufferpanel
" (list of buffer numbers, name, and modified/modifiable flags).
" -----------------------------------------------------------------------------


if exists('g:theopanel_loaded')
    finish
endif
let g:theopanel_loaded = 1

" {{{ Tabline options
set showtabpanel=2
set fillchars+=tpl_vert:\|
set tabpanelopt=vert,align:left
" }}}


" {{{ Assignment
if exists('g:theopanel_buflist')
  set tabpanelopt+=columns:20
  set tabpanel=%!Bufferpanel()

  " Force redraw on buffer changes
  autocmd BufAdd,BufCreate,BufDelete,BufWipeout * redrawtabpanel

  " Toggle bufferpanel
  nnoremap <expr><silent> <leader>b &showtabpanel==2 ?
                          \ ':set showtabpanel=0<CR>' : ':set showtabpanel=2<CR>'
else
  set tabpanelopt+=columns:9
  set tabpanel=%!Tabpanel()
endif
" }}}


" {{{ Tabpanel

function! Tabpanel() abort
  " case where it is loaded before UI elements are finalized (e.g., .vim/plugin)
  let curr = exists('g:actual_curtabpage') ? g:actual_curtabpage : 1

  let s = printf("%2d", curr)
  let numWin = len(tabpagebuflist(curr))
  if numWin > 1
    let s .= '|  ' . numWin
  endif

  return s
endfunction
" }}}


" {{{ Bufferpanel
function! Bufferpanel() abort
  let s = '%#TabPanelFill#'

  " tabpanel is evaluated per tab; workaround to create the list only once
  if g:actual_curtabpage == 1

    " Get the list of buffers. Use bufexists() to include hidden buffers
    let bufferNums = filter(range(1, bufnr('$')), 'buflisted(v:val)')

    for i in bufferNums
      " Highlight if it's the current buffer
      let s .= (i == bufnr()) ? ('%#TabPanelSel#') : ('%#TabPanel#')  

      let s .= ' ' . i . ' '  " Append the buffer number

      " Give a [NEW] flag to an unnamed buffer
      if bufname(i) == ''
        let s .= '[NEW]'
      endif

      " Append bufname
      let bufname = fnamemodify(bufname(i), ':t')

      " Truncate bufname
      " -1 if vertical separators are on
      " -3 for the buffer number
      " -3 for the potential modified flag
      " -2 for the ..
      let lenLimit = 11
      if len(bufname) > lenLimit
        " expr-[:] is range-inclusive (i.e., [0:10] returns 11 char)
        let bufname = bufname[0:lenLimit - 1] . '..'
      endif

      let s .= bufname

      " Add modified & read only flag
      if getbufvar(i, "&modified")
        let s .= '[+]'
      endif
      if !getbufvar(i, "&modifiable")
        let s .= '[-]'
      endif
      if getbufvar(i, "&readonly")
        let s .= '[RO]'   
      endif

      let s .= "\n"
    endfor

    let s .= "%#TabPanelFill#"
  endif

  return s
endfunction

" }}}
