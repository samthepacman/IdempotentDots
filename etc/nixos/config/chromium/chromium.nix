{ pkgs, lib, config, ... }:

{
    programs.chromium = {
        enable = true;
        extensions = [
          { id = "ihennfdbghdiflogeancnalflhgmanop"; } # gruvbox theme
          { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # ublock origin
          { id = "eimadpbcbfnmbkopoojfekhnkhdbieeh"; } # dark reader
          { id = "dbepggeogbaibhgnhhndojpepiihcmeb"; } # vimium
	  ];
      };
  }
