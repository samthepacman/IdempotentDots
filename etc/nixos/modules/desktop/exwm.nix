{ config, lib, pkgs, inputs, ... }:

{

  ## DESKTOP SETUP

  # services.xserver.enable = true;
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";
  services.xserver.libinput.enable = true;
  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  services.emacs.enable = true;
  # services.emacs.package = pkgs.emacsPgtkGcc;
 

   fonts.fonts = with pkgs; [
     (nerdfonts.override { fonts = [ "Iosevka" "Cousine" "Hack" "JetBrainsMono" ]; })
   ];


   services.xserver = {
        enable = true;
          displayManager = {
	   gdm.enable = true;
       #   lightdm.enable = true;
       #   lightdm.greeters.mini = {
       #       enable = true;
       #       user = "shiva";
       #       extraConfig = ''
       #           [greeter]
       #           show-password-label = false
       #           password-alignment = left
       #           password-label-text = Enter Pin :
       #           invalid-password-text = Invalid Pin
       #           [greeter-theme]
       #           window-color = "#232529"
       #           background-image = "/etc/lightdm/images/einstein.png";
       #           border-color = "#232530"
       #           border-width = 2px
       #           layout-space = 15
       #           password-color = "#f8f8f2"
       #           password-background-color = "#212329"
       #           password-border-color = "#bd93f9"
       #           password-border-width = 2px
       #       '';
       #    };
       };
   windowManager.exwm.enable = true;
  };
  

 # windowmanager = {
 #      xmonad.enable = true;
 #      xmonad.enablecontribandextras = true;
 #      xmonad.extrapackages = hpkgs: [
 #        hpkgs.xmonad
 #        hpkgs.xmonad-contrib
 #        hpkgs.xmonad-extras
 #         ];
 #      };
 #   };
}
