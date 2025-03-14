local M = { "neovim/nvim-lspconfig", }

M.dependencies = {
  { "williamboman/mason.nvim", config = true, }, --> LSP server manager
  "williamboman/mason-lspconfig.nvim",           --> Bridge between lspconfig and mason
  { "j-hui/fidget.nvim",       opts = {}, },     --> LSP status indicator
  {
    "folke/lazydev.nvim",                        --> Neovim dev environment
    ft = "lua",
    opts = {
      library = {
        { path = "luvit-meta/library", words = { "vim%.uv" }, },
      },
    },
  },
  { 'Bilal2453/luvit-meta', lazy = true }, --> vim.uv support
}

M.config = function()
  -- Sets the LSP UI look
  vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(
    vim.lsp.handlers.hover, {
      border = "rounded",
      title = "Hover"
    }
  )
  vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(
    vim.lsp.handlers.signature_help, {
      border = "rounded",
      title = "Signature Help"
    }
  )

  -- Makes autocmd for LSP functionalities
  local theovim_lsp_config_group = vim.api.nvim_create_augroup("TheovimLspConfig", { clear = true, })

  vim.api.nvim_create_autocmd("LspAttach", {
    group = theovim_lsp_config_group,
    callback = function(event)
      local map = function(keys, func, desc)
        vim.keymap.set("n", keys, func, { buffer = event.buf, desc = "LSP: " .. desc })
      end

      -- Creates a keybinding to format code
      map("<leader>F", vim.lsp.buf.format, "[F]ormat buffer")

      -- Sets the default values for LSP functions with Telescope counterparts
      local status, builtin = pcall(require, "telescope.builtin")
      local telescope_opt = { jump_type = "tab" } --> spawns selection in a new tab
      -- Sets the navigation keymaps
      map("gd", status and function() builtin.lsp_definitions(telescope_opt) end or vim.lsp.buf.definition,
        "[G]oto [D]efinition")
      map("gr", builtin.lsp_references or vim.lsp.buf.references, "[G]oto [R]eferences")
      map("gI", builtin.lsp_implementations or vim.lsp.buf.implementation, "[G]oto [I]mplementation")
      map("<leader>D",
        status and function() builtin.lsp_type_definitions(telescope_opt) end or vim.lsp.buf.type_definition,
        "Type [D]efinition")
      -- Sets the symbol keymaps
      map("<leader>ds", builtin.lsp_document_symbols or vim.lsp.buf.document_symbol, "[D]ocument [S]ymbols")
      map("<leader>ws", builtin.lsp_dynamic_workspace_symbols or vim.lsp.buf.workspace_symbol, "[W]orkspace [S]ymbols")

      -- Sets the functiosn with no Telescope counterparts
      map("<leader>rn", vim.lsp.buf.rename, "[R]e[n]ame")
      map("<leader>ca", vim.lsp.buf.code_action, "[C]ode [A]ction")
      map("gD", vim.lsp.buf.declaration, "[G]o [D]eclaration")

      -- Creates an autocmd to highlight the symbol under the cursor
      local client = vim.lsp.get_client_by_id(event.data.client_id)
      if client and client.supports_method(vim.lsp.protocol.Methods.textDocument_documentHighlight) then
        local theovim_lsp_hl_group = vim.api.nvim_create_augroup("TheovimLspHl", { clear = false, })
        vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
          buffer = event.buf,
          group = theovim_lsp_hl_group,
          callback = vim.lsp.buf.document_highlight,
        })

        vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
          buffer = event.buf,
          group = theovim_lsp_hl_group,
          callback = vim.lsp.buf.clear_references,
        })

        local theovim_lsp_detach_group = vim.api.nvim_create_augroup("TheovimLspHlDetach", { clear = true, })
        vim.api.nvim_create_autocmd("LspDetach", {
          group = theovim_lsp_detach_group,
          callback = function(event2)
            vim.lsp.buf.clear_references()
            vim.api.nvim_clear_autocmds({ group = "TheovimLspHl", buffer = event2.buf })
          end
        })
      end

      -- Creates a keybinding to toggle inlay hints, as hints can displace some of the code
      if client and client.supports_method(vim.lsp.protocol.Methods.textDocument_inlayHint) then
        map("<leader>th", function()
          vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = event.buf }))
        end, "[T]oggle Inlay [H]ints")
      end
    end,
  })

  -- Gets nvim-cmp capabilities
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities = vim.tbl_deep_extend("force", capabilities, require("cmp_nvim_lsp").default_capabilities())

  -- Calls Mason
  require("mason").setup()

  -- Defines a list of servers and server-specific config
  local servers = {
    bashls = {},
    clangd = {},
    pylsp = {},
    texlab = {},
    -- html = { filetypes = { "html", "twig", "hbs"} },

    lua_ls = {
      settings = {
        Lua = {
          telemetry = { enable = false },
        },
      },
    },
  }

  -- Calls mason-lspconfig to
  -- 1. ensure servers are installed
  -- 2. Set up handlers for each server using the `server` table
  require("mason-lspconfig").setup({
    ensure_installed = vim.tbl_keys(servers or {}),
    -- Separate setup_handlers() function could be used for the same purpose.
    -- Read |mason-lspconfig.setup_handlers()| for more information
    handlers = {
      function(server_name)
        -- Uses default server config (`{}`) or server-specific config from `server` table
        local server = servers[server_name] or {}
        capabilities = capabilities
        require("lspconfig")[server_name].setup(server)
      end
    }
  })
end

return M
