{ config, lib, pkgs, ... }:

{

## NETWORKING

  networking = {
   networkmanager.enable = true;
   #networkmanager.wifi.backend = "iwd";
   #wireless.iwd.settings.Settings.AutoConnect = true;
   hostName = "bijutsu";
   useDHCP = false;
   interfaces = { 
      enp0s31f6.useDHCP = true;
      wlp2s0.useDHCP = true;
   };
   firewall.enable = true;
  };
}

