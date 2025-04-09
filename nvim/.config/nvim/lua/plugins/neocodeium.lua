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

    vim.keymap.set("i", "<C-x><C-r>", neocodeium.cycle_or_complete)
    -- Map <C-n>, <C-p>, <C-e>, etc. to do a certain thing iff neocodeium is visible to simulate native ins-completion
    local function map_neocodeium(key, action)
      vim.keymap.set("i", key, function()
        if neocodeium.visible() then
          return action()
        else
          return key
        end
      end, { expr = true })
    end

    map_neocodeium("<C-n>", neocodeium.cycle)
    map_neocodeium("<C-p>", function() neocodeium.cycle(-1) end)
    map_neocodeium("<C-y>", neocodeium.accept)
    map_neocodeium("<C-e>", neocodeium.clear)
  end,
}

-- return {
--   "zbirenbaum/copilot.lua",
--   enabled = true,
--   cmd = "Copilot",
--   event = "InsertEnter",
--   config = function()
--     local copilot = require("copilot")
--     copilot.setup({ })
--
--     local suggestion = require("copilot.suggestion")
--     vim.keymap.set("i", "<C-x><C-r>", suggestion.next)
--     local function map_copilot(key, action)
--       vim.keymap.set("i", key, function()
--         if suggestion.is_visible() then
--           return action()
--         else
--           return key
--         end
--       end, { expr = true })
--     end
--
--     map_copilot("<C-n>", suggestion.next)
--     map_copilot("<C-p>", suggestion.prev)
--     map_copilot("<C-y>", suggestion.accept)
--     map_copilot("<C-e>", suggestion.dismiss)
--   end,
-- }
