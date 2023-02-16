{ config, lib, pkgs, inputs, ... }:

let 
  theme = import ../../theme/theme.nix { inherit pkgs; };
in
{

# =================================

   fonts.fonts = with pkgs; [
     (nerdfonts.override { fonts = [ "Iosevka" "Cousine" "Hack" "JetBrainsMono" ]; })
   ];

# =================================

   console = 
     let
      normal = with theme.colors; [ c0 c1 c2 c3 c4 c5 c6 c7 ];
      bright = with theme.colors; [ c8 c9 c10 c11 c12 c13 c14 c15 ];
    in
    {
     colors = normal ++ bright;
    };
 
# =================================

  services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";
  services.xserver.libinput.enable = true;

  services.xserver = {
      enable = true;
      displayManager = {
         gdm.enable = true;
         defaultSession = "none+xmonad";
  };
  windowManager = {
         xmonad.enable = true;
         xmonad.enableContribAndExtras = true;
         xmonad.extraPackages = hpkgs: [
           hpkgs.xmonad
           hpkgs.xmonad-contrib
           hpkgs.xmonad-extras
         ];
      };
   };
}
