{ pkgs, config, ... }:

{
programs.qutebrowser = {
      enable = true;
      extraConfig = ''
        c.statusbar.padding = {'top': 5, 'bottom': 5, 'left': 3, 'right': 3}
        c.tabs.padding = {'top': 10, 'bottom': 8, 'left': 5, 'right': 3}
        config.bind(',ap', 'config-cycle content.user_stylesheets ~/.config/nixpkgs/themes/solarized-everything-css/css/apprentice/apprentice-all-sites.css ""')
        config.bind(',dr', 'config-cycle content.user_stylesheets ~/.config/nixpkgs/themes/solarized-everything-css/css/darculized/darculized-all-sites.css ""')
        config.bind(',gr', 'config-cycle content.user_stylesheets ~/.config/nixpkgs/themes/solarized-everything-css/css/gruvbox/gruvbox-all-sites.css ""')
        config.bind(',sd', 'config-cycle content.user_stylesheets ~/.config/nixpkgs/themes/solarized-everything-css/css/solarized-dark/solarized-dark-all-sites.css ""')
        config.bind(',ysd', 'config-cycle content.user_stylesheets ~/.config/nixpkgs/core/sway/recipes/qutebrowser/horizon.css ""')
        config.bind(',sl', 'config-cycle content.user_stylesheets ~/.config/nixpkgs/themes/solarized-everything-css/css/solarized-light/solarized-light-all-sites.css ""')

      '';
      searchEngines = {
        "DEFAULT" =  "https://duckduckgo.com/?q={}";
        "w" =  "https://en.wikipedia.org/w/index.php?search={}";
        "gs" =  "https://scholar.google.com/scholar?q={}";
        "aw" =  "https://wiki.archlinux.org/?search={}";
        "o" =  "https://search.nixos.org/options?channel=unstable&from=0&size=50&sort=relevance&query={}";
        "p" =  "https://search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&query={}";
        "d" =  "https://en.wiktionary.org/wiki/{}";
        "v" =  "https://eo.wiktionary.org/wiki/{}";
        "melpa" =  "https://melpa.org/#/?q={}";
        "s" =  "http://stackoverflow.com/search?q={}";
        "ss" = "https://www.semanticscholar.org/search?q={}";
        "c" =  "https://clio.columbia.edu/quicksearch?q={}";
        "gh" =  "https://github.com/search?q={}&type=Repositories";
        "h" =  "https://hackage.haskell.org/packages/search?terms={}";
        "libgen" =  "https://libgen.is/search.php?req={}";
        "viki" =  "https://eo.wikipedia.org/w/index.php?search={}";
        "ia" =  "https://archive.org/details/texts?and%5B%5D={}&sin=";
        };
      settings = {
        colors = {
          tabs = {
            even.bg = "#000000";
            odd.bg = "#000000";
            selected.even.bg = "#1e1e1e";
            selected.odd.bg = "#1e1e1e";
          };
        };
        fonts = {
          completion.category = "13pt Iosevka";
          default_family = "Iosevka, Terminus, Monospace, monospace, Fixed";
          prompts = "14pt Iosevka";
        };
        hints.chars = "arstdhneio";
        # url.default_page = "${scripts}/homepage/homepage.html";
      };
    };
  }
