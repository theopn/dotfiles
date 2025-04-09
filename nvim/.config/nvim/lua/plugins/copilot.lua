return {
  "zbirenbaum/copilot.lua",
  enabled = true,
  cmd = "Copilot",
  event = "InsertEnter",
  config = function()
    local copilot = require("copilot")
    copilot.setup({ })

    local suggestion = require("copilot.suggestion")
    vim.keymap.set("i", "<C-x><C-r>", suggestion.next)
    local function map_copilot(key, action)
      vim.keymap.set("i", key, function()
        if suggestion.is_visible() then
          return action()
        else
          return key
        end
      end, { expr = true })
    end

    map_copilot("<C-n>", suggestion.next)
    map_copilot("<C-p>", suggestion.prev)
    map_copilot("<C-y>", suggestion.accept)
    map_copilot("<C-e>", suggestion.dismiss)
  end,
}
