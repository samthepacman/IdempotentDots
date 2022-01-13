{ config, pkgs, lib, ... }:

let
  nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {inherit pkgs;};
in

{
  home.packages = with pkgs; [
  ];
}
