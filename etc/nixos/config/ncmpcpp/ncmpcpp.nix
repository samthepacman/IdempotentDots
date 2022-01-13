{ config, pkgs, ... }:
{
programs.ncmpcpp = {
      enable = true;
      settings = import ./settings.nix;
    };
}
