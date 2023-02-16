{ config, lib, pkgs, ... }:

{

## USER CONFIG
  users.users.johndoe = {
    isNormalUser = true;
    extraGroups = [ " " ];
     # select groups like "wheel" "networkmanager" "video" "users" "audio" "sudo" , "netdev"
  };
}
