" Vim filetype plugin file
" Language:	C++
" Maintainer:	Theo P.
" Last Change:	2023-10-15

setlocal expandtab tabstop=2 softtabstop=2 shiftwidth=2
setlocal colorcolumn=80 textwidth=79
setlocal matchpairs+==:;

" This command is to compile and run a single C++ file using g++.
" For more complex project (that is, any project with more than one .cc file),
" you should consider making a Makefile and use `:make` command
command RunCpp !g++ %:p -Wall -Werror -std=c++11 -o %:p:r && %:p:r
