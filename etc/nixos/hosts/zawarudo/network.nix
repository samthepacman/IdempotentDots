{ config, lib, pkgs, ... }:

{

## NETWORKING

  networking = { 
   hostName = "zawarudo";
   useDHCP = false;
   interfaces = { 
      eno1.useDHCP = true;
      wlp2s0.useDHCP = true;
   };
   
   wireless = { 
     enable = true;
     userControlled.enable = true;
     interfaces = [ "wlp2s0" ];
   };

   firewall.enable = true;
};

   services.connman.enable = true;


}

