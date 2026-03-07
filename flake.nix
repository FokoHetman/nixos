{
  description = "fokflakefokflakefokflakefokflakefokflakefokflakefokflakefokflakefokflakefokflakefokflake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nix-index-database.url = "github:nix-community/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/nur";
    nur.inputs.nixpkgs.follows = "nixpkgs";

    nvf.url = "github:NotAShelf/nvf/v0.8";

    nathan.url = "github:poollovernathan/nixos";
    nathan.inputs.nixpkgs.follows = "nixpkgs";
    
    juna = {
      url = "git+https://codeberg.org/juna-lab/juna";
      inputs.nixpkgs.follows = "nixpkgs"; # Critical for store hygiene
    };

    ags.url = "github:Aylur/ags/v1";
    
    hyprland = {
      type = "git";
      url = "https://github.com/hyprwm/Hyprland";
      submodules = true;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hyprland-plugins = {
      url = "github:hyprwm/hyprland-plugins";
      inputs.hyprland.follows = "hyprland";
    };
    
    sops-nix.url = "github:Mic92/sops-nix";
    
    kmonad.url = "git+https://github.com/kmonad/kmonad?submodules=1&dir=nix";
    xmonad-contrib.url = "github:xmonad/xmonad-contrib";
    quickshell = {
      url = "git+https://git.outfoxxed.me/outfoxxed/quickshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    
    nix-on-droid = {
      url = "github:nix-community/nix-on-droid";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    mobile-nixos = {
      url = "github:nixos/mobile-nixos";
      flake = false;
    };
    
    blackmarket.type = "git";
    blackmarket.url = "https://git.hetman.at/blackmarket";
    blackmarket.inputs.nixpkgs.follows = "nixpkgs";
    
    #flake-parts.url = "github:hercules-ci/flake-parts";
    templater.url = ./templater;
  };

  outputs = { self, nixpkgs, home-manager, nix-on-droid, /*flake-parts,*/templater, ... }@inputs:
    templater.lib.makeConfig {inherit inputs self;} 
    {
      imports = [./templates.nix] ++ templater.lib.import-tree ./modules;
      architectures = templater.architectures.default;
      nixpkgsConfig = {config.allowUnfree = true;};
      templates = import ./templates.nix;
    };
}
