{ config, pkgs, ... }:

{
programs.starship = {
  enable = true;
  enableZshIntegration = false;
    settings = {
   #  add_newline = false; # Disable the vertical gap
   #  prompt_order = [ "directory" "character" ];
     scan_timeout = 10;
   #  character.symbol = "∳ ";
    };
  };
}
