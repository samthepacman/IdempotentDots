{ config, pkgs, ... }:

{
  programs.git = {
    package = pkgs.gitAndTools.gitFull;
    enable = true;
    userName = "name";
    userEmail = "mail";
    aliases = {
      co = "checkout";
      ci = "commit";
      s = "status";
      st = "status";
      cl = "clone";
    };
    extraConfig = {
      core.editor = "nvim";
      protocol.keybase.allow = "always";
     # credential.helper = "store --file ~/.config/git/git-credentials";
      pull.rebase = "false";
    };
  };
}
