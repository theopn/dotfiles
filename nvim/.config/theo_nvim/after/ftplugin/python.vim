" Vim filetype plugin file
" Language:	Python
" Maintainer:	Theo P.
" Last Change:	2023-10-14

" As suggested by PEP8.
" Already done in the built-in ftplugin, repeated for verbosity
setlocal expandtab tabstop=4 softtabstop=4 shiftwidth=4

" Bar at the 80th column
setlocal colorcolumn=80
" Break line at the 79th char
setlocal textwidth=79

" Command to run the current file
command RunPython !python3 %:p
