{ config, lib, pkgs, ... }:

{

    services = {
      resolved = { 
         enable = true;
         dnssec = "true";
         fallbackDns =  [ "8.8.8.8" ];
       };
      tor.enable = true;
      tlp.enable = true;
      thermald.enable = true;
      earlyoom.enable = true;
      gpm.enable = true;
  };
}
