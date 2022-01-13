{ config, lib, pkgs, ... }:

{

## USER CONFIG
  programs.adb.enable = true;
  users.users.sam = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "video" "users" "audio" "sudo" ];
     # select groups like "wheel" "networkmanager" "video" "users" "audio" "sudo" , "netdev"
  };
}
