{ self, config, lib, pkgs, ... }:
{
  nix = {
    systemFeatures = [ "benchmark" "big-parallel" "kvm" ];
    autoOptimiseStore = true;
    gc = { 
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
    maxJobs = 8;
    useSandbox = true;
    optimise.automatic = true;
    allowedUsers = [ "@wheel" ];
    trustedUsers = [ "root" "@wheel" ];
    extraOptions = ''
      min-free = ${toString (100 * 1024 * 1024)}
      max-free = ${toString (1024 * 1024 * 1024)}
      keep-outputs = true
      keep-derivations = true
      fallback = true
    '';
   };

   documentation.nixos.enable = true;
}
