{ pkgs, lib, config, ... }:

{
nixpkgs.config = {

packageOverrides = pkgs: rec{
      surf = pkgs.surf.override {
          patches = [ 
                      ./history.diff
                      ./homepage.diff
                      ./vim.diff];
       };
    };
  };
}
