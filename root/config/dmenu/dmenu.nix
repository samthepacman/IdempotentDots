{ pkgs, lib, config, ... }:

{
nixpkgs.config = {

packageOverrides = pkgs: rec{
      dmenu = pkgs.dmenu.override {
          patches = [ ./height.diff ];
       };
    };
  };
}
