{ config, pkgs, lib, ... }:

{
  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.starship = {
    enable = true;
    enableZshIntegration = true;
    settings = {
      add_newline = true;
     };
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableSyntaxHighlighting = true;
    enableCompletion = true;
    shellAliases = import ./alias.nix;
    autocd = true;
    dotDir = ".config/zsh";
    initExtra = ''
        any-nix-shell zsh --info-right | source /dev/stdin
    '';
   # initExtra = "sh ~/.config/nixpkgs/machine/bin/colorscript -e crunchbang-mini";
  };
}
