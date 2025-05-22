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
    
    kmonad.url = "git+https://github.com/kmonad/kmonad?submodules=1&dir=nix";
    xmonad-contrib.url = "github:xmonad/xmonad-contrib";
    
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    discord.url = "github:fokohetman/discord.nix";

    blackmarket.url = "git+ssh://git@fokopi/~/blackmarket";

  };

  outputs = { self, nixpkgs, home-manager, ... }@inputs:
    let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};

    # UNCOMMON / USER DEPENDENT
    hostname = "fokopc"; #moved to nixosConfigurations
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
      inherit pkgs;
      modules = [./submodules/nvf-configuration.nix];
    }).neovim;
    
    pubKeys = [ # todo: organize them (merge foko@fokopc, regen paprykkania@gmail.com with your domain's email), configure fokopi's key properly
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPY/5ogOaBGG4kTwf5njUmQHLffcsgMBstS4Be/ym0Ky foko@fokopi"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKqbQ0IHO8eIhHTcF4ysTctNg09prlfj6wZaAWEaaSwg foko@fokopc"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAING43cVUOV9hmvkQNOKnYKcaBzamSFRnLGcLb0JlDlOZ paprykkania@gmail.com"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC8Ol/Mhqze9yugcUFjBDdzSsSjln3RSj/jzGRvBKUmXp7JAGCRhSyy2kfuhWrrrJcUKpajpFowmkNYxUiH4AAcuEBheT1MNtsGwfuBihZHMyrZM71eoEjtTT7pd+UYwHyXLbs/n0zaK3bFdfKb04Ufxq5mfw7Cjb+HTe6Zmr0k0ypxiui1pQWhzvKiMU4SEiXHYYlivswKKOZjs6/ohe8GSudg8iwFoxAH1ElxEtwwP1c3V0ju/Y0Nbv3ROSTzLkgE62o5BZz2ammMaUIqdKamJgASzBTAMP7+RDv3vKG4bkWNsL2KUS2Pt5GvuBiZ+SahwL5z0OlN2ixPoETS/7Qz2RMhwjKsTIoJUODXa5AZpBPYStmOFkHbzzkOiDaD/CgchXU8EiRVfMm8nAeEZZtsN2b+pDm0rusu8/2GfK16DgLHZdpa0fvyQ9JRrSGyjqGfkJLSJD0LOvD06zEqg/yf8KnAGlBDXYVUUg6ZUZXHVsXYz9dFmClZcG79d3y9XHk= foko@fokopc"
    ];

    fonts.rainworld = pkgs.callPackage({ pkgs }: pkgs.stdenv.mkDerivation {
      name = "rainworld-font";
      dontConfigure = true;
      src = ./fonts/rainworld;
      installPhase = ''
        runHook preInstall

        install -Dm644 $src/*.ttf -t $out/share/fonts/opentype

        runHook postInstall
        #mkdir -p $out/share/fonts
        #cp -R $src $out/share/fonts/opentype/
      '';
      meta = { description = "A [rainworld font](https://www.reddit.com/r/rainworld/comments/1bei8sy/i_created_a_fully_functional_typeface_for_every/#lightbox) mapped to use private use area"; };
    }) { inherit pkgs; };



    nixosConfigurations = {
      #inherit system pkgs;
      "fokopc" = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs hostname;};
	      modules = [
	        {_module.args = {inherit username timezone inputs nvim fonts pubKeys;};}
	        ./configuration.nix
          home-manager.nixosModules.default
          inputs.stylix.nixosModules.stylix
          inputs.discord.nixosModules.discord
          (inputs.nathan.mkTailnet {})
          (inputs.nathan.mkNathan {canSudo = true;})
	      ] ++ inputs.xmonad-contrib.nixosModules;
      };
      "fokopi" = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs hostname;};
        modules = [
    	    {_module.args = {inherit username timezone inputs nvim fonts pubKeys;};}
	        ./configuration.nix
          home-manager.nixosModules.default
          inputs.stylix.nixosModules.stylix
          (inputs.nathan.mkTailnet {})
          (inputs.nathan.mkNathan {canSudo = true;})
          {
            nixpkgs.hostPlatform  = "aarch64-linux";
            nixpkgs.buildPlatform = "x86_64-linux";
          }
      	];
      };
      "fokolaptop" = nixpkgs.lib.nixosSystem {
        hostname = "fokolaptop";
        specialArgs = { inherit inputs hostname;};
	      modules = [
	        {_module.args = {inherit username timezone inputs nvim fonts pubKeys;};}
	        ./configuration.nix
          home-manager.nixosModules.default
          inputs.stylix.nixosModules.stylix
          #inputs.discord.nixosModules.discord
          #(inputs.nathan.mkTailnet {})
          #(inputs.nathan.mkNathan {canSudo = true;})
	      ] ++ inputs.xmonad-contrib.nixosModules;
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
          openssh.authorizedKeys.keys = pubKeys;
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
