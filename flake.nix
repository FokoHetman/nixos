{
  description = "fokflakefokflakefokflakefokflakefokflakefokflakefokflakefokflakefokflakefokflakefokflake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    fokquote.url = "github:fokohetman/fok-quote";
    nixvim.url = "github:fokohetman/nixvim-foko";
    chess.url = "github:fokohetman/cli_chess";
    fokutils.url = "github:fokohetman/fok-utils";
    fokshell.url = "github:fokohetman/fokshell";


    nathan.url = "github:poolloverNathan/nixos";#"github:fokohetman/nathanfixyourself";

    stylix.url = "github:danth/stylix";
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

    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, home-manager, ... }@inputs:
    let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};

    # UNCOMMON / USER DEPENDENT
    hostname = "fokopi";
    username = "foko";
    timezone = "Europe/Warsaw";

    inherit (self) outputs;
  in {
    nixosConfigurations = {
      inherit system pkgs;
      "fokopc" = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs hostname;};
	      modules = [
	        {_module.args = {inherit username timezone inputs;};}
	        ./configuration.nix
          home-manager.nixosModules.default
          inputs.stylix.nixosModules.stylix
          (inputs.nathan.mkTailnet {})
          (inputs.nathan.mkNathan {canSudo = true;})
	      ];
      };
      "fokopi" = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs hostname;};
	      modules = [
    	    {_module.args = {inherit username timezone inputs;};}
	        ./configuration.nix
          home-manager.nixosModules.default
          inputs.stylix.nixosModules.stylix
          (inputs.nathan.mkTailnet {})
      	];
      };
      /*"fokoserver" = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs;};
        modules = [
          {_module.args = {inherit username timezone inputs hostname;};}
          ./fokoserver/nixos/configuration.nix
          home-manager.nixosModules.default
          inputs.stylix.nixosModules.stylix
          (inputs.nathan.mkTailnet {})
        ];
      };*/
    };
  };
}
