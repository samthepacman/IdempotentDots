{ config, pkgs, inputs, lib, ... }:
with lib;
{
   imports = [
    ./core
   ];

   programs.home-manager.enable = true;
         # programs.home-manager.path = "$HOME/.config/nixpkgs/home-manager";
}

