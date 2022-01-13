
-- FONT
vim.o.guifont = "Iosevka Nerd Font:h11"


-- COMMON SETTINGS

vim.o.hlsearch = false
vim.o.mouse = 'a'
vim.o.breakindent = true
vim.o.ignorecase = true
vim.o.termguicolors = true
vim.o.completeopt = 'menuone,noselect'
vim.o.smartcase = true
vim.o.updatetime = 250
vim.g.onedark_terminal_italics = 2
vim.wo.signcolumn = 'yes'
vim.wo.number = true
vim.opt.undofile = true

require('settings.mappings')
require('settings.pps')
require('settings.theme')
