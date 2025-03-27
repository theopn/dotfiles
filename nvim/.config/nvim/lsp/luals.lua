---@type vim.lsp.Config
return {
  cmd = { "lua-language-server" },
  filetypes = { "lua" },
  root_markers = { ".luarc.json", ".luarc.jsonc", ".luacheckrc", ".stylua.toml", "stylua.toml", "selene.toml", "selene.yml", ".git" },
  --on_init = require("util").lua_ls_on_init,
  settings = {
    Lua = {
      telemetry = { enable = false },
      workspace = {
        library = vim.api.nvim_get_runtime_file("", true)
      }
    },
  },
}
