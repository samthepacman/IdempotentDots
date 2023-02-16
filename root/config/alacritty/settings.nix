let
  clr = import ../themes/one.nix;
in
  {
    env = {
     "TERM" = "xterm-256color";
  };

  background_opacity = 1.0;

#### PADDING

  window = {
    padding.x = 12;
    padding.y = 12;
    decorations = "Full";
  };

#### FONTS ----------------

  font = {
    size = 11.5;
    use_thin_strokes = true;

    normal = {
      family = "Iosevka Nerd Font";
      style = "Regular";
    };

    bold = {
      family = "Iosevka Nerd Font";
      style = "Regular";
    };

    italic = {
      family = "Iosevka Nerd Font";
      style = "Regular";
    };
  };

#### STYLING --------------

  cursor.style = "Block";

  shell = {
    program = "zsh";
  };

#### COLOR SCHEME --------------

  colors = {
    # Default colors
    primary = {
      background = clr.background;
      foreground = clr.foreground;
    };

  normal = {
    black =   clr.black;
    red =     clr.red;
    green =   clr.green;
    yellow =  clr.yellow;
    blue =    clr.blue;
    magenta = clr.magenta;
    cyan =    clr.cyan;
    white =   clr.white;
};
  bright = {
    black =   clr.black-br;
    red =     clr.red-br;
    green =   clr.green-br;
    yellow =  clr.yellow-br;
    blue =    clr.blue-br;
    magenta = clr.magenta-br;
    cyan =    clr.cyan-br;
    white =   clr.white-br;
};
  };
}
