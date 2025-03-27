return {
  "monkoose/neocodeium",
  enabled = true,
  event = "VeryLazy",
  config = function()
    local neocodeium = require("neocodeium")
    neocodeium.setup({
      -- If `false`, then would not start codeium server (disabled state)
      -- You can manually enable it at runtime with `:NeoCodeium enable`
      enabled = false,
      -- When set to `true`, autosuggestions are disabled.
      -- Use `require'neodecodeium'.cycle_or_complete()` to show suggestions manually
      manual = true,
    })

    vim.keymap.set("n", "<leader>c", function()
      require("neocodeium.commands").toggle(true) --> true to half the server completely
    end, { desc = "Toggle Neo[C]odeium" })


    -- create an autocommand which closes cmp when ai completions are displayed
    vim.api.nvim_create_autocmd("User", {
      pattern = "NeoCodeiumCompletionDisplayed",
      callback = function() require("cmp").abort() end
    })

    vim.keymap.set("i", "<C-e>", neocodeium.cycle_or_complete)
    vim.keymap.set("i", "<C-r>", function()
      require("neocodeium").cycle_or_complete(-1)
    end)

    vim.keymap.set("i", "<C-f>", neocodeium.accept)
  end,
}
