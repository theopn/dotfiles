return {
	"catgoose/nvim-colorizer.lua",
	event = "VeryLazy", -- Load on very lazy event
	opts = {
		filetypes = { "*" }, -- Filetype options.  Accepts table like `user_default_options`
		buftypes = {}, -- Buftype options.  Accepts table like `user_default_options`
		-- Boolean | List of usercommands to enable.  See User commands section.
		user_commands = true, -- Enable all or some usercommands
		lazy_load = true, -- Lazily schedule buffer highlighting setup function
		user_default_options = {
			names = true, -- "Name" codes like Blue or red.  Added from `vim.api.nvim_get_color_map()`
			names_opts = { -- options for mutating/filtering names.
				lowercase = true, -- name:lower(), highlight `blue` and `red`
				camelcase = true, -- name, highlight `Blue` and `Red`
				uppercase = false, -- name:upper(), highlight `BLUE` and `RED`
				strip_digits = false, -- ignore names with digits,
				-- highlight `blue` and `red`, but not `blue3` and `red4`
			},
			-- Expects a table of color name to #RRGGBB value pairs.  # is optional
			-- Example: { cool = "#107dac", ["notcool"] = "ee9240" }
			-- Set to false to disable, for example when setting filetype options
			names_custom = false, -- Custom names to be highlighted: table|function|false
			RGB = true, -- #RGB hex codes
			RGBA = true, -- #RGBA hex codes
			RRGGBB = true, -- #RRGGBB hex codes
			RRGGBBAA = false, -- #RRGGBBAA hex codes
			AARRGGBB = false, -- 0xAARRGGBB hex codes
			rgb_fn = false, -- CSS rgb() and rgba() functions
			hsl_fn = false, -- CSS hsl() and hsla() functions
			css = false, -- Enable all CSS *features*:
			-- names, RGB, RGBA, RRGGBB, RRGGBBAA, AARRGGBB, rgb_fn, hsl_fn
			css_fn = false, -- Enable all CSS *functions*: rgb_fn, hsl_fn
			-- Tailwind colors.  boolean|'normal'|'lsp'|'both'.  True sets to 'normal'
			tailwind = false, -- Enable tailwind colors
			tailwind_opts = { -- Options for highlighting tailwind names
				update_names = false, -- When using tailwind = 'both', update tailwind names from LSP results.  See tailwind section
			},
			-- parsers can contain values used in `user_default_options`
			sass = { enable = false, parsers = { "css" } }, -- Enable sass colors
			-- Highlighting mode.  'background'|'foreground'|'virtualtext'
			mode = "background",       -- Set the display mode
			-- Virtualtext character to use
			virtualtext = "■",
			-- Display virtualtext inline with color.  boolean|'before'|'after'.  True sets to 'after'
			virtualtext_inline = false,
			-- Virtualtext highlight mode: 'background'|'foreground'
			virtualtext_mode = "foreground",
			-- update color values even if buffer is not focused
			-- example use: cmp_menu, cmp_docs
			always_update = false,
			-- hooks to invert control of colorizer
			hooks = {
				-- called before line parsing.  Accepts boolean or function that returns boolean
				-- see hooks section below
				disable_line_highlight = false,
			},
		},
	},
	{
		"roobert/tailwindcss-colorizer-cmp.nvim",
		-- optionally, override the default options:
		config = function()
			require("tailwindcss-colorizer-cmp").setup({
				color_square_width = 2,
			})
		end
	},
	{
		"luckasRanarison/tailwind-tools.nvim",
		name = "tailwind-tools",
		build = ":UpdateRemotePlugins",
		dependencies = {
			"nvim-treesitter/nvim-treesitter",
		},
		opts = {
			document_color = {
				enabled = true, -- can be toggled by commands
				kind = "inline", -- "inline" | "foreground" | "background"
				inline_symbol = "󰝤 ", -- only used in inline mode
				debounce = 200, -- in milliseconds, only applied in insert mode

				cmp = {
					highlight = "foreground", -- color preview style, "foreground" | "background"
				},
			},
		} -- your configuration
	}
}
