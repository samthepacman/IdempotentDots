{ config, pkgs, ...}:

{
programs.nushell = {
    enable = true;
    settings = {
      skip_welcome_message = true;
      edit_mode = "vi";
      # prompt = "starship_prompt";
      # prompt = "echo '$PRMPT'"; # Enable the Starship prompt
      startup = [  # "mkdir ~/.cache/starship"
                   # "starship init nu | save ~/.cache/starship/init.nu"
                   # "source ~/.cache/starship/init.nu"
                   # "let-env VISUAL = 'emacs'"
                   # "alias $VISUAL = echo $nu.env.VISUAL"
                   # "alias $EDITOR = echo $nu.env.EDITOR"
       ];
    };
  };
}
