--- plugins/treesitter.lua
--- $ figlet -f threepoint theovim
--- _|_|_  _  _   . _ _
---  | | |(/_(_)\/|| | |
---
--- Configure Neovim built-in Treesitter engine

-- https://echasnovski.com/blog/2026-03-13-a-guide-to-vim-pack.html#many-vim-pack-add
vim.api.nvim_create_autocmd("PackChanged", { callback = function(ev)
  local name, kind = ev.data.spec.name, ev.data.kind
  if name == "nvim-treesitter" and kind == "update" then
    if not ev.data.active then vim.cmd.packadd("nvim-treesitter") end
    vim.cmd("TSUpdate")
  end
end })

vim.pack.add({
  "https://github.com/nvim-treesitter/nvim-treesitter",
})


require("nvim-treesitter").setup()

-- Find the name of parsers with:
--  := require("nvim-treesitter").get_available()
-- (LaTeX clashes with Vimtex)
local languages = { "bash",
  "c",
  "cpp",
  "fish",
  "html",
  "java",
  "javascript",
  "lua",
  "markdown",
  "markdown_inline",
  "python",
  "sql",
  "vimscript",
  "vimdoc",
}

local filetypes = {}
for _, lang in ipairs(languages) do
  for _, ft in ipairs(vim.treesitter.language.get_filetypes(lang)) do
    table.insert(filetypes, ft)
  end
end

vim.api.nvim_create_autocmd("FileType", {
  pattern = filetypes,
  callback = function()
    vim.treesitter.start()
    vim.wo[0][0].foldexpr = "v:lua.vim.treesitter.foldexpr()"
    vim.wo[0][0].foldmethod = "expr"
    -- Indentation is Experimental
    -- vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
  end,
})
