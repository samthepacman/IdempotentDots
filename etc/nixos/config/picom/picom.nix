{ config, pkgs, ... }:

{
 services.picom = {
   enable = true;
   # package = pkgs.callPackage ./picom-tryone.nix { };
   # package = pkgs.picom.overrideAttrs (
   #       o: {
   #         src = pkgs.fetchFromGitHub {
   #           repo = "compton";
   #           owner = "blackcapcoder";
   #           rev = "a8445684fe18946604848efb73ace9457b29bf80";
   #           sha256 = "R+YUGBrLst6CpUgG9VCwaZ+LiBSDWTp0TLt1Ou4xmpQ=";
   #         };
   #       }
   #    );
   experimentalBackends = true;
   fade = true;
   fadeDelta = 2;
   shadow = true;
   shadowOffsets = [ (-8) (-8) ];
   shadowOpacity = "0.75";
   shadowExclude = [ "class_g = 'dzen*'" "class_g = 'dzen slave'" ];
   noDockShadow = false;
   noDNDShadow = false;
   # inactiveDim = "0.15";
   # activeOpacity = "1.0";
   # inactiveOpacity = "1.0";
   opacityRule = [ "100:name *?= 'xmobar'" "80:name *?= 'shellPrompt'" ];
   menuOpacity = "1.0";
   backend = "glx";
   vSync = true;
   extraOptions = ''
      shadow-radius = 8;
      clear-shadow = true;
      corner-radius = 20; 
      frame-opacity = 1.0;
    # blur-background = false;
    # blur-method = "dual_kawase";
    # blur-background-frame = true;
    # blur-strength = 6;
    # alpha-step = 0.06;
     detect-client-opacity = true;
     detect-rounded-corners = true;
     # paint-on-overlay = true;
     # detect-transient = true;
     mark-wmwin-focused = false;
     mark-ovredir-focused = false;
     # shadow-red = 0.22;
     # shadow-green = 0.22;
     # shadow-blue = 0.28;
    '';
  };
}

