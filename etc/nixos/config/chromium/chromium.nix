{ pkgs, lib, config, ... }:

{
    programs.chromium = {
        enable = true;
        extensions = [
          # { id = "gcbommkclmclpchllfjekcdonpmejbdp"; } # https everywhere
          # { id = "jlmpjdjjbgclbocgajdjefcidcncaied"; } # daily.dev
          { id = "nnigpbiaggiephcndokoaongeefpbdcj"; } # quickcode
          # { id = "fockhhgebmfjljjmjhbdgibcmofjbpca"; } # onion browser button
          { id = "hkgfoiooedgoejojocmhlaklaeopbecg"; } # picture-in-picture
          { id = "hpkjepofpoigjlihphohlecofocheiaf"; } # onedark
          # { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # ublock origin
          { id = "eimadpbcbfnmbkopoojfekhnkhdbieeh"; } # dark reader
          { id = "dbepggeogbaibhgnhhndojpepiihcmeb"; } # vimium
	  ];
      };
  }
