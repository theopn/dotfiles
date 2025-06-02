"


if exists('g:theoline_loaded')
    finish
endif
let g:theoline_loaded = 1

" {{{ Assignment
if exists('g:theoline_buflist')
  set tabline=%!SpawnBufferline()
else
  set tabline=%!SpawnTabline()
endif
" }}}

" {{{ Tabline
fun! SpawnTabline()
  let s = ' Tabs :) '

  for i in range(1, tabpagenr('$'))  " Loop through the number of tabs
    " Highlight the current tab
    let s .= (i == tabpagenr()) ? ('%#TabLineSel#') : ('%#TabLine#')
    let s .= '%' . i . 'T '  " set the tab page number (for mouse clicks)
    let s .= i               " set page number string

    " Add a number of window if applicable
    let numWin = len(tabpagebuflist(i))
    if numWin > 1
      let s .= '[ ' . numWin . ']'
    endif

    let s .= ' '
  endfor
  let s .= '%#TabLineFill#%T'  " Reset highlight

  " Close button on the right if there are multiple tabs
  if tabpagenr('$') > 1
    let s .= '%=%#TabLineSel#%999X[X]'
  endif
  return s
endfun
" }}}

" {{{ Bufferline
fun! SpawnBufferline()
  let s = ' :) '

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
" }}}

