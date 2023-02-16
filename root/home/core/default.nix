{ config, pkgs, hostName, lib, ...}:
{
  imports = [ ./options.nix
              ./environment.nix
            ];
}
