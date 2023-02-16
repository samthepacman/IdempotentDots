{
     shellutil.nur      = import ../../config/nur/nur-progs.nix;
     shellutil.starship = import ../../config/starship/starship.nix;
     shellutil.tmux     = import ../../config/tmux/tmux.nix;

     shell.zsh     = import ../../config/zsh/zsh.nix;
     shell.nushell = import ../../config/nushell/nushell.nix;
     shell.fish    = import ../../config/fish/fish.nix;

     ui.sway   = import ../../config/sway/sway.nix;
     ui.waybar = import ../../config/waybar/waybar.nix;
     ui.picom  = import ../../config/picom/picom.nix;
     ui.dmenu  = import ../../config/dmenu/dmeun.nix;

     misc.apps = import ./programs.nix;
     misc.xdg  = import ../../config/xdg/xdg.nix;

     web.qutebrowser = import ../../config/qutebrowser/qutebrowser.nix;
     web.mozilla     = import ../../config/mozilla/mozilla.nix;
     web.chromium    = import ../../config/chromium/chromium.nix;

     prog.doom    = import ../../config/doom/doom.nix;
     prog.ncmpcpp = import ../../config/ncmpcpp/ncmpcpp.nix;
     prog.neovim  = import ../../config/neovim/neovim.nix;
     prog.neomutt = import ../../config/neomutt/neomutt.nix;
     prog.surf    = import ../../config/surf/surf.nix;
     prog.git     = import ../../config/git/git.nix;

     notif.dunst = import ../../config/dunst/dunst.nix;
     notif.mako  = import ../../config/mako/mako.nix;

     term.kitty     = import ../../config/kitty/kitty.nix;
     term.alacritty = import ../../config/alacritty/alacritty.nix;
     term.foot      = import ../../config/foot/foot.nix;

}
