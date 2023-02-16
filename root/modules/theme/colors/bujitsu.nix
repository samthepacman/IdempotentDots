{ pkgs, primaryColor }:

rec {
  fg = "ebd3e0";
  bg = "161616";
  c0 = "161616";
  c1 = "ff6c6b";
  c2 = "98be65";
  c3 = "ecbe7b";
  c4 = "51afef";
  c5 = "c678dd";
  c6 = "46d9ff";
  c7 = "bbc2cf";
  c8 = "2e3440";
  c9 = "ff6c6b";
  c10 = "98be65";
  c11 = "ecbe7b";
  c12 = "51afef";
  c13 = "c678dd";
  c14 = "46d9ff";
  c15 = "bbc2cf";

  primary =
    if primaryColor == "red"
    then c1
    else if primaryColor == "green"
    then c2
    else if primaryColor == "yellow"
    then c3
    else if primaryColor == "magenta"
    then c5
    else c4;

  secondary =
    if primaryColor == "red"
    then c2
    else if primaryColor == "green"
    then c1
    else if primaryColor == "yellow"
    then c1
    else if primaryColor == "magenta"
    then c3
    else c5;

  primaryBright =
    if primaryColor == "red"
    then c9
    else if primaryColor == "green"
    then c10
    else if primaryColor == "yellow"
    then c11
    else if primaryColor == "magenta"
    then c13
    else c12;

  secondaryBright =
    if primaryColor == "red"
    then c10
    else if primaryColor == "green"
    then c9
    else if primaryColor == "yellow"
    then c11
    else if primaryColor == "magenta"
    then c13
    else c12;

  muted = "204571";
  transparent = "00000000";
  textColor = bg;
  activeBorderColor = primary;
  inactiveBorderColor = c8;
  highlightColor = primary;
  vimColorscheme = "horizon";

}
