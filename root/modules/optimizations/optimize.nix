{ self, config, lib, pkgs, ... }:
{
  nix.settings = {
    sandbox = true;
    system-features = [ "benchmark" "big-parallel" "kvm" ];
    auto-optimise-store = true;
    max-jobs = 8;
    allowed-users = [ "@wheel" ];
    trusted-users = [ "root" "@wheel" ];
};

  nix = {
    gc = { 
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
    optimise.automatic = true;
    extraOptions = ''
#      min-free = ${toString (100 * 1024 * 1024)}
#      max-free = ${toString (1024 * 1024 * 1024)}
      keep-outputs = true
      keep-derivations = true
      fallback = true
    '';
   };

   documentation.nixos.enable = true;

#   virtualisation = {
#    podman.enable = true;
#    waydroid.enable = true;
    # xen = {
    #   enable = true;
    #   package = pkgs.xen;
    # };
}
