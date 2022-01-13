{ config, lib, pkgs, ... }:

{
imports = [
   ./users/sam.nix
   ./hosts/art/art.nix
 ];

# PERSISTENT FILES
  environment.etc."machine-id".source
    = "/nix/persist/etc/machine-id";
}
