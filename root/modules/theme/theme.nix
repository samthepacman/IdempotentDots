{ pkgs }:

let
  wallpaperPath = ../assets/wallpapers;
in
rec {
  borderWidth = "0";
  primaryColor = "green";
  lightModeEnabled = false;

  colors =
   import ./colors/bujitsu.nix { inherit pkgs primaryColor; };
}
