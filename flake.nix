{
  description = "fokflakefokflakefokflakefokflakefokflakefokflakefokflakefokflakefokflakefokflakefokflake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    fokquote.url = "github:fokohetman/fok-quote";
    nixvim.url = "github:fokohetman/nixvim-foko";
    chess.url = "github:fokohetman/cli_chess";
    fokutils.url = "github:fokohetman/fok-utils";
    fokshell.url = "github:fokohetman/fokshell";

    nvf.url = "github:NotAShelf/nvf";

    nur.url = "github:nix-community/nur";
    nur.inputs.nixpkgs.follows = "nixpkgs";

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

    discord.url = "github:fokohetman/discord.nix";

  };

  outputs = { self, nixpkgs, home-manager, ... }@inputs:
    let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};

    # UNCOMMON / USER DEPENDENT
    hostname = "fokopc";
    username = "foko";
    timezone = "Europe/Warsaw";

    inherit (self) outputs;
  in rec {

    /*packages."${system}".default = 
    (inputs.nvf.lib.neovimConfiguration {
      pkgs = pkgs;
      modules = [ ./submodules/nvf-configuration.nix ];
    }).neovim;*/
    mkFoko = {uid ? 1825, entire ? false, canSudo}: defineUser {
      name = "foko";
      inherit uid canSudo;
    };
    nvim = (inputs.nvf.lib.neovimConfiguration {
      pkgs = pkgs;
      modules = [ ./submodules/nvf-configuration.nix ];
    }).neovim;
    fonts.rainworld = pkgs.stdenvNoCC.mkDerivation {
      name = "rainworld-font";
      dontConfigue = true;
      src = ./fonts/rainworld;
      installPhase = ''
        mkdir -p $out/share/fonts
        cp -R $src $out/share/fonts/opentype/
      '';
      meta = { description = "A [rainworld font](https://www.reddit.com/r/rainworld/comments/1bei8sy/i_created_a_fully_functional_typeface_for_every/#lightbox) mapped to use private use area"; };
    };



    nixosConfigurations = {
      inherit system pkgs;
      "fokopc" = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs hostname;};
	      modules = [
	        {_module.args = {inherit username timezone inputs nvim rainworld-font;};}
	        ./configuration.nix
          home-manager.nixosModules.default
          inputs.stylix.nixosModules.stylix
          inputs.discord.nixosModules.discord
          (inputs.nathan.mkTailnet {})
          (inputs.nathan.mkNathan {canSudo = true;})
	      ];
      };
      "fokopi" = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs hostname;};
	      modules = [
    	    {_module.args = {inherit username timezone inputs rainworld-font;};}
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

    # Thanks github:PoolloverNathan/nixos 

    defineUser = {
      uid,
      name,
      canSudo ? false,
      extraUserConfig ? {},
      userConfigFile ? null,
      extraHomeConfig ? {},
      extraConfigArgs ? {},
      imports ? [],
    }: args@{
      pkgs,
      lib,
      config,
      options,
      ...
    }:
    assert lib.assertMsg (extraConfigArgs != {} -> userConfigFile != null) "Cannot pass arguments to an unspecified config file";
    {
      inherit imports;
      config = let
        systemConfig = config.home-manager.users.${name}.system;
      in {
        users.users.${name} = {
          isNormalUser = !extraUserConfig.isSystemUser or false;
          inherit uid;
          extraGroups = if canSudo then lib.mkOverride (-100) ["wheel"] else [];
          inherit (systemConfig) shell hashedPassword;
          description = systemConfig.userDescription;
        } // extraUserConfig;
        systemd.services = lib.mapAttrs' (name': value: { name = "${name}-${name'}"; inherit value; }) systemConfig.services;
        home-manager.users.${name} = {
          options.system = {
            hashedPassword = lib.mkOption {
              description = ''
                The hash of your password. To generate a password hash, run `mkpasswd`. In most simple cases, you can also use `nixos passwd` to change your password.
                '';
              type = lib.types.nullOr (lib.types.passwdEntry lib.types.singleLineStr);
              default = null;
            };
            shell = lib.mkOption {
              description = ''
                Your default shell, to be used when logging in. Can be either a derivation (for `nix run`-like behavior) or a path (to run directly). If you pass a derivation that refers to an executable file directly, as opposed to the more common derivation with a `bin` directory, explicitly select `.outPath`.
                '';
              type = lib.types.pathInStore;
              default = pkgs.bashInteractive + /bin/bash;
            };
            userDescription = lib.mkOption {
              description = ''
                The description to give your user. This can be e.g. a longer or more common username.
                '';
              type = lib.types.passwdEntry lib.types.singleLineStr;
              default = "";
            };
            sshKeys = lib.mkOption {
              description = ''
                Public keys to allow SSH authorization for. If some are set, it will be legal to leave the password unspecified.
                '';
              type = with lib.types; listOf singleLineStr;
              default = [];
            };
            options = lib.mkOption {
              description = ''
                The outer system's options. You may not edit this!
              '';
              type = lib.types.anything;
            };
            config = lib.mkOption {
              description = ''
                The outer system's config. You may not edit this!
              '';
              type = lib.types.anything;
            };
            services = lib.mkOption {
              description = ''
                Services to run on a system level. This is passed (with a prefix applied) to [systemd.services] and expects the same content.
              '';
              type = lib.types.attrs;
              default = {};
            };
          };
          imports = [
            extraHomeConfig
            (if userConfigFile != null then import userConfigFile (args // extraConfigArgs) else {})
          ];
          config = {
            system.config = config;
            system.options = options;
            assertions = [
              {
                assertion = systemConfig.sshKeys != [] || systemConfig.hashedPassword != null;
                message = "User ${name} [${uid}] has no way to log in";
              }
            ];
          };
        };
      };
    };
  };
}
