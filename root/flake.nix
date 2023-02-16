{
  description = "NixOS Incandescent";

  inputs ={
    nixpkgs.url = "nixpkgs/nixos-unstable";
    home-manager.url = "github:rycee/home-manager/master";
    impermanence.url = "github:nix-community/impermanence";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
};

  outputs = { self, nixpkgs, home-manager, ... }@inputs:
  let
   system = "x86_64-linux";   
   pkgs = import nixpkgs {
       inherit system;
       config = { allowUnfree = true; };
   };


 lib = nixpkgs.lib;

  in {     
  nixosConfigurations = {
      bijutsu = lib.nixosSystem {
        inherit system;

   modules = [ 
       ./config.nix
       home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.sam = { 
	     imports = [ 
                   ./home
	                 "${inputs.impermanence}/home-manager.nix"
		             ];
		home.stateVersion = "23.05";
	        home.persistence."/nix/persist/home/sam" = {
                  directories = [  
                        ".gnupg"
                        ".cache"
                        
                        ".config/chromium"
			".config/dconf"
			".config/discord"
                        ".config/gtk-3.0"
                        ".config/gtk-4.0"
			".config/nvim"
			".config/eww"
			".config/spotify"
			".config/hypr"
                        ".local"
                       
			"Downloads"
			"Documents"
			"Pictures"
			"Videos"
			"Music"
                   ];
                  allowOther = true;
                  files = [
                         ".swayrc"
			 ".gtkrc-2.0"
                ];	    
              }; 
            };
	  }
        ];
      };
    };
  };
}
