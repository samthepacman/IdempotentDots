{ config, pkgs, lib, ... }:

{
  programs.zoxide = { 
    enable = true;
    enableFishIntegration = true;
  };

  programs.starship = {
    enable = true;
    enableFishIntegration = true;
    settings = {
    format = lib.concatStrings [ 
       "$directory"
       "$package"
       "$character"
       ];
    add_newline = true;
    character = {
       success_symbol = "[λ](green)";
       error_symbol = "[λ](red)";
       };
     };
  };

  programs.fish = { 
    enable = true;
    shellInit = ''
      set fish_greeting
      set -U fish_color_normal normal
      set -U fish_color_command F8F8F2
      set -U fish_color_quote F1FA8C
      set -U fish_color_redirection 8BE9FD
      set -U fish_color_end 50FA7B
      set -U fish_color_error FFB86C
      set -U fish_color_param FF79C6
      set -U fish_color_comment 6272A4
      set -U fish_color_match --background=brblue
      set -U fish_color_selection white --bold --background=brblack
      set -U fish_color_search_match bryellow --background=brblack
      set -U fish_color_history_current --bold
      set -U fish_color_operator 00a6b2
      set -U fish_color_escape 00a6b2
      set -U fish_color_cwd green
      set -U fish_color_cwd_root red
      set -U fish_color_valid_path --underline
      set -U fish_color_autosuggestion BD93F9
      set -U fish_color_user brgreen
      set -U fish_color_host normal
      set -U fish_color_cancel -r
      set -U fish_pager_color_completion normal
      set -U fish_pager_color_description B3A06D yellow
      set -U fish_pager_color_prefix normal --bold --underline
      set -U fish_pager_color_progress brwhite --background=cyan
      '';
    shellAliases = import ./alias.nix;
  };
}
