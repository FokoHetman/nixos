{
  description = "fokflakefokflakefokflakefokflakefokflakefokflakefokflakefokflakefokflakefokflakefokflake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    fokquote.url = "github:fokohetman/fok-quote";
    nixvim.url = "github:fokohetman/nixvim-foko";
    chess.url = "github:fokohetman/cli_chess";
    fokutils.url = "github:fokohetman/fok-utils";
    nathan.url = "github:poollovernathan/nixos";


    stylix.url = "github:danth/stylix";
#    ags.url = "github:Aylur/ags";

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
      ${hostname} = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs;};
	modules = [
	  {_module.args = {inherit username timezone inputs hostname;};}
	  ./nixos/configuration.nix
          home-manager.nixosModules.default
          inputs.stylix.nixosModules.stylix
          (inputs.nathan.mkTailnet {})
          #(inputs.nathan.nixosModules.nathan)
	];
      };
    };
  };
}
