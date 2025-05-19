return {
  "stevearc/oil.nvim",
  dependencies = {
    { "nvim-tree/nvim-web-devicons" },
  },
  config = function()
    local oil = require("oil")

    -- https://github.com/stevearc/oil.nvim/blob/master/doc/recipes.md#show-cwd-in-the-winbar
    -- Declare a global function to retrieve the current directory
    function _G.get_oil_winbar()
      local bufnr = vim.api.nvim_win_get_buf(vim.g.statusline_winid)
      local dir = oil.get_current_dir(bufnr)
      if dir then
        dir = vim.fn.fnamemodify(dir, ":~")
      else
        -- If there is no current directory (e.g. over ssh), just show the buffer name
        dir = vim.api.nvim_buf_get_name(0)
      end

      return table.concat({
        "%#Normal#",

        "%#MiniStatuslineModeOther#",
        "[OIL.NVIM] ",

        dir,
      })
    end

    oil.setup({
      view_options = {
        show_hidden = true,
      },
      win_options = {
        winbar = "%!v:lua.get_oil_winbar()",
      },
    })

    vim.keymap.set("n", "<leader>b", function()
      oil.open_float(nil, { preview = {} })
    end, { desc = "Toggle File [B]rowser (Oil.nvim)" })
  end,
}
