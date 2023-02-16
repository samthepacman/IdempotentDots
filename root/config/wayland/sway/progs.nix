# ~/.config/nixpkgs/user/app.nix
{ config, pkgs, lib, ... }:
let
    mode-sway = pkgs.writeShellScriptBin "swaymode"
    (import ../../machine/bin/swaymode.nix { inherit pkgs; });
in
{

fonts.fontconfig.enable = true;
home.packages = with pkgs; [

##### DESKTOP ##### -----------------
feh
polybar
sxhkd
dmenu

##### SYSTEM ##### ------------------
ranger
unzip
trash-cli
pfetch
imv
cmst
mpv
any-nix-shell

##### USER APPS ##### ---------------
discord
spotify-unwrapped
eolie
chromium
easytag
hakuneko
marktext
cherrytree
thunderbird

gimp
blender
inkscape
lmms
obs-studio
tenacity
vmpk

##### NETWORK ##### -----------------
connman-notify
connman-ncurses
wpa_supplicant_gui
blueman

##### MEDIA ##### -------------------
# gimp
mpd
mpc_cli
pulsemixer
# blender
# inkscape

##### LIB ##### ---------------------
libnotify
libsForQt5.qtstyleplugins

##### THEMING ##### --------------------
nordic
plata-theme
paper-icon-theme
terminus_font

##### RUST ##### --------------------
broot
exa
tokei
procs
ripgrep
fd
dust
hyperfine
delta
cargo
bat
skim
tealdeer

##### DEVELOP ##### -----------------

gcc       # C / C++
python3   # PYTHON
guile_3_0
ruby_3_0
perldevel
lua5_4
ghc
mysql80

helvetica-neue-lt-std
source-sans


##### APPLIST END ##### -------------

    ];
}
