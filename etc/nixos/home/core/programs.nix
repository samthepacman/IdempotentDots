# ~/.config/nixpkgs/user/app.nix
{ config, pkgs, lib, ... }:
{

home.packages = with pkgs; [

##### SYSTEM ##### ------------------
ranger
unzip
trash-cli
wpa_supplicant_gui
blueman
wofi
dmenu-wayland
nnn
connman_dmenu

##### SWAY ##### ---------------------
swaybg
wf-recorder
swaylock
xdg-desktop-portal-wlr
wallutils
playerctl
eww-wayland

#### DESKTOP ##### ------------------
discord
spotify-unwrapped
chromium
easytag
cherrytree

##### UTILITIES ##### ---------------
pfetch
imv
cmst
mpv
any-nix-shell

##### NETWORK ##### -----------------
connman-notify
connman-ncurses


##### MEDIA ##### -------------------
mpd
mpc_cli
pulsemixer

##### LIB ##### ---------------------
libnotify
libsForQt5.qtstyleplugins

##### MISC ##### --------------------
arc-theme
nordic
arc-icon-theme
easytag
hakuneko

##### RUST ##### --------------------
broot
exa
tokei
procs
ripgrep
fd
cargo
bat
tealdeer

##### DEVELOP ##### -----------------
gcc
python3
rustc
neovim
neovide

##### MEDIA EDITING ##### -----------
gimp
inkscape

##### APPLIST END ##### -------------

    ];
}
