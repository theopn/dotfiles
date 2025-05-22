-- Default 50: https://luals.github.io/wiki/settings/#hover
local maxNumFieds = 100

---@type vim.lsp.Config
return {
  cmd = { "lua-language-server" },
  filetypes = { "lua" },
  root_markers = { ".luarc.json",
    ".luarc.jsonc",
    ".luacheckrc",
    ".stylua.toml",
    "stylua.toml",
    "selene.toml",
    "selene.yml",
    ".git"
  },
  settings = {
    Lua = {
      hover = {
        enumsLimit = maxNumFieds,
        previewFields = maxNumFieds,
      },
      telemetry = {
        enable = false
      },
      workspace = {
        library = vim.api.nvim_get_runtime_file("", true)
      }
    },
  },
}
