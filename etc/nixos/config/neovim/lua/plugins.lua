vim.cmd('packadd packer.nvim')
-- vim.cmd('packadd nvim-treesitter')
-- vim.cmd('packadd nvim-lspconfig')

return require('packer').startup(
  function()
    use '9mm/vim-closer'
    use 'machakann/vim-highlightedyank'
    use 'machakann/vim-sandwich'
    use 'lifepillar/vim-gruvbox8'
    use 'tpope/vim-commentary'
    use 'honza/vim-snippets'
    use 'SirVer/ultisnips'
    use 'romainl/vim-cool'
    use 'junegunn/fzf'
    use 'junegunn/fzf.vim'
    use 'mizlan/termbufm'
    -- use 'nvim-treesitter/nvim-treesitter'
    -- use 'neovim/nvim-lspconfig'
    -- use 'nvim-lua/completion-nvim'
    -- use 'nvim-lua/diagnostic-nvim'
    use 'tweekmonster/startuptime.vim'
    use 'wbthomason/packer.nvim'
  end
)
