{ config, lib, pkgs, inputs, ... }:

{

  ## DESKTOP SETUP

   services.xserver = {
        enable = true;
          displayManager = {
	   gdm.enable = true;
       };
  };
}
