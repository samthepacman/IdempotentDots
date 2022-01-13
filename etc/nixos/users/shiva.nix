{ config, lib, pkgs, ... }:

{

## USER CONFIG
  programs.adb.enable = true;
  users.users.shiva = {
    isNormalUser = true;
    extraGroups = [ "wheel" "netdev" "users" "audio" "sudo" ];
     # select groups like "wheel" "networkmanager" "video" "users" "audio" "sudo" , "netdev"
  };
}
