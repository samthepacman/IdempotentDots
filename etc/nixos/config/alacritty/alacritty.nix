{ config, pkgs, lib, ... }:
{

     programs.alacritty = {
       enable = true;
      settings = lib.attrsets.recursiveUpdate (import ./settings.nix) {
       };
     };
}
