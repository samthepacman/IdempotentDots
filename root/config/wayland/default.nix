{ config, pkgs, lib, ...}:

{
  imports = [
    ./dirs/xdg.nix
    ./foot/foot.nix
    ./mako/mako.nix
    ./sway/sway.nix
    ./waybar/waybar.nix
    ];
}
