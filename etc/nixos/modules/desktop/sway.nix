{ config, lib, pkgs, inputs, ... }:

let 
  theme = import ../theme/theme.nix { inherit pkgs; };
in
{

  ## DESKTOP SETUP

   fonts.fonts = with pkgs; [
     (nerdfonts.override { fonts = [ "Iosevka""JetBrainsMono" ]; })
   ];

   services.greetd = {
     vt = 2;
     enable = true;
     settings = {
       default_session = {
         command = "${lib.makeBinPath [pkgs.greetd.tuigreet] }/tuigreet --time --cmd sway";
         user = "greeter";
       };
     };
   };

   console = 
     let
      normal = with theme.colors; [ c0 c1 c2 c3 c4 c5 c6 c7 ];
      bright = with theme.colors; [ c8 c9 c10 c11 c12 c13 c14 c15 ];
    in
    {
     colors = normal ++ bright;
    };
  
   programs = {
   dconf.enable = true;
   sway = {
     enable = true;
     wrapperFeatures.gtk = true; # so that gtk works properly
     extraPackages = with pkgs; [
       swayidle
       wl-clipboard
      ];
    };
   xwayland.enable = false;
 };
}
