{ config, lib, pkgs, ... }:
{
## USER CONFIG
  programs.adb.enable = true;

  programs.fuse.userAllowOther = true;
  users.users.root.initialHashedPassword = "<hash-here>";
  users.users.sam.initialHashedPassword = "<hash-here>";
  users.mutableUsers = false;
  users.users.sam = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "video" "users" "audio" "sudo" "adbusers" ];
     # select groups like "wheel" "networkmanager" "video" "users" "audio" "sudo" , "netdev"
 };
#  imports = [ ../home/home.nix ];
}
