vim.api.nvim_command('command! TSRehighlight :write | edit | TSBufEnable highlight')

local opts = {noremap = true, silent = true}

-- vim.api.nvim_set_keymap('n', '<Leader>n', ':noh<CR>', opts)
vim.api.nvim_set_keymap('t', '<Esc>', '<C-\\><C-n>', opts)

vim.cmd('au FileType cpp ia <buffer> itn int')
