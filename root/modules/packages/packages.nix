{ config, lib, pkgs, ... }:
let
    nix_bash_wrapper = pkgs.writeScriptBin "znx"
    (import ../../bin/znx.nix { inherit pkgs; });
    
    sway_mode_wrapper = pkgs.writeScriptBin "swayswitch"
    (import ../../bin/swaymode.nix { inherit pkgs; });
in
{

  ## PACKAGES
  environment.systemPackages = with pkgs; [
    unzip
    git
    wget
    alacritty
    grim
    lxappearance-gtk2
    papirus-icon-theme
    gtk-layer-shell
    cached-nix-shell
    greetd.tuigreet
    nix_bash_wrapper
    sway_mode_wrapper

    lynis
    ossec
    chkrootkit
    aide
  ];
}
