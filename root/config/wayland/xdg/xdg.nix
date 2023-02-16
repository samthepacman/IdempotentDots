{ pkgs, config, lib, ... }:

{
  xdg.configFile = {
    "wofi/config".source= ../wofi/config;
    "wofi/style.css".source= ../wofi/style.css;
  };
}
