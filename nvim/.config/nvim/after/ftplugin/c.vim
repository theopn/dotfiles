" Vim filetype plugin file
" Language:	C
" Maintainer:	Theo P.
" Last Change:	2023-10-15

setlocal expandtab tabstop=2 softtabstop=2 shiftwidth=2
setlocal colorcolumn=80 textwidth=79
setlocal matchpairs+==:;

" This command is to compile and run a single C file using gcc.
" For more complex project (that is, any project with more than one .c file),
" you should consider making a Makefile and use `:make` command
command RunC !gcc %:p -Wall -Werror -std=c17 -o %:p:r && %:p:r
