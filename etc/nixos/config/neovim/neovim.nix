{ config, pkgs, ... }:

{
  programs.neovim = {
      enable = true;
      vimAlias = true;
      extraConfig = builtins.readFile ./init.vim;
      plugins = with pkgs.vimPlugins; [
        vim-closer
        vim-sandwich
        vim-gruvbox8
        onedark-vim
        vim-commentary
        vim-cool

        lualine-nvim

        dashboard-nvim
        galaxyline-nvim
        nvim-tree-lua
        nvim-web-devicons
        vim-lsp
        telescope-nvim
        nvim-compe

        vim-orgmode
    
        deoplete-nvim
        deoplete-dictionary
        deoplete-lsp
        fzf-vim
        vim-startuptime
        ultisnips 
      ];
    };
  xdg.configFile = {
    "nvim/lua/binds.lua".source = ./lua/binds.lua;
    "nvim/lua/init.lua".source = ./lua/init.lua;
    "nvim/lua/opts.lua".source = ./lua/opts.lua;
    "nvim/lua/setup.lua".source = ./lua/setup.lua;
    "nvim/lua/stl.lua".source = ./lua/stl.lua;
    
    "nvim/lua/colors/init.lua".source = ./lua/colors/init.lua;
    "nvim/lua/colors/highlights.lua".source = ./lua/colors/highlights.lua;
    "nvim/lua/colors/themes/onedark.lua".source = ./lua/colors/themes/onedark.lua;
   };
  }
