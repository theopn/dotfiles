return
{
  'voldikss/vim-floaterm',
  config = function()
    vim.g.floaterm_width = 0.8
    vim.g.floaterm_height = 0.8
    vim.g.floaterm_title = 'Terminal ($1/$2)'
    vim.g.floaterm_autoclose = 1
    vim.g.floaterm_position = 'center'
    vim.g.floaterm_borderchars = '─│─│╭╮╯╰'
    vim.g.floaterm_winblend = 0
  end,
}
