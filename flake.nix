{
  description = "fokflakefokflakefokflakefokflakefokflakefokflakefokflakefokflakefokflakefokflakefokflake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    nix-index-database.url = "github:nix-community/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";

    fokquote.url = "github:fokohetman/fok-quote";
    nixvim.url = "github:fokohetman/nixvim-foko";
    chess.url = "github:fokohetman/cli_chess";
    fokutils.url = "github:fokohetman/fok-utils";
    fokshell.url = "github:fokohetman/fokshell";
    fokshell.inputs.nixpkgs.follows = "nixpkgs";
    nvf.url = "github:NotAShelf/nvf/v0.8";

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

    nix-on-droid = {
      url = "github:nix-community/nix-on-droid";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  
    blackmarket.type = "git";
    blackmarket.url = "https://git.hetman.at/blackmarket";
    blackmarket.inputs.nixpkgs.follows = "nixpkgs";

    mobile-nixos = {
      url = "github:nixos/mobile-nixos"; # least stable mirror
      flake = false;
    };

    quickshell = {
      url = "git+https://git.outfoxxed.me/outfoxxed/quickshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = { self, nixpkgs, home-manager, nix-on-droid, ... }@inputs:
    let
    system = "x86_64-linux";
    aarch = "aarch64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
    pkgsAarch = import nixpkgs { system = aarch; config.allowUnfree = true;};

    # UNCOMMON / USER DEPENDENT
    hostname = "fokopc"; #moved to nixosConfigurations
    username = "foko";
    timezone = "Europe/Warsaw";

    inherit (self) outputs;
  in rec {

    mkMonster = xs: let x = toString xs; in
      pkgs.runCommandLocal "monster-mass" {} ''
        mkdir -p $out/bin
        for i in $(seq 1 ${x}); do
          echo "echo This is $i th Dimension of THE MONSTER" > $out/bin/monster-$i
          chmod +x $out/bin/monster-$i
        done
      '';

    /*packages."${system}".default = 
    (inputs.nvf.lib.neovimConfiguration {
      pkgs = pkgs;
      modules = [ ./submodules/nvf-configuration.nix ];
    }).neovim;*/
    mkFoko = {uid ? 1825, entire ? false, canSudo}: defineUser {
      name = "foko";
      inherit uid canSudo;
    };
    nvim.packages = rec {
      nvimBuilder = pkgs: (inputs.nvf.lib.neovimConfiguration {
        inherit pkgs;
        modules = [./submodules/nvf-configuration.nix];
      }).neovim;
      x86_64-linux = nvimBuilder pkgs;
      aarch64-linux = nvimBuilder pkgsAarch;
    };
    
    pubKeys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCjpL3XBTmxH6biR7rwIviQcA5q3WygfLriAv6+fLA566Bg7kmfHso3lXEm9zIf//EM+np15venbyg35kwf7jDbyPr80QrdQJFQacKL4KAWxj739uPdS5XCdwF2Lf4yHcOncdPz5vupgcQM7qlF/U3Xt2HoqXfb+7nnFZPgwiJ6xP81FltEQmhQRrj0vwK4aVu4VyZ2/7PqmAnbmo42OVYpkTcIjmFpOfnXpw/m3VqoLiNyDPra4LkhzL+umWEUqBtwqkZMG5rP/HNMll7u3AoCfbVwFJg4cjjUYy/uDE8PAZP9xrWQva6kCqH6YR4iUJpKXUMtQRqg1z+/e/QkjtehFnrwd4HOLK1+LBGgbQ4j7duyfblR5yKEYP3C8mGEKB5yqo3si26nzPxUzZvjT7XAoG02KypGHUeeZ9hJ7NbXdxaycmvRsRj4OSoXmexo1r6qY8RckPzAsqpdrDWFcoeePVcRR6Yc3P5dc7NDzXh0FQ55VViQTLqlDgdtETdPd17Vqa1RIFNr/sn0VMJJnXqno3ViEUl8b2LvRQBWKKJWy+oymTwnT3bxN9BFHpbEKK5zzOd4H8/qo7UtUBrCtr2WzSxnnmLQWwRguw9ysfkWdjMChNCwU4GjSBYy/VBkoO4sH9Phf1RNMRvoA6el4lRPn56qhIOeVKX+V0IzMeOoBw== foko@hetman.at"
    ];
    
    assets = ./assets;
    submodules = ./submodules;

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
	        {_module.args = {inherit mkMonster username timezone inputs fonts pubKeys; nvim = nvim.packages.x86_64-linux;};}
	        ./configuration.nix
          home-manager.nixosModules.default
          inputs.stylix.nixosModules.stylix
          inputs.discord.nixosModules.discord
          (inputs.nathan.mkTailnet {})
          (inputs.nathan.mkNathan {canSudo = true;})
	      ] ++ inputs.xmonad-contrib.nixosModules;
      };
      "fokopi" = let hostname = "fokopi";
      in nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs hostname;};
        pkgs = import nixpkgs { system = "aarch64-linux"; config.allowUnfree = true;};
        modules = [
    	    {_module.args = {inherit username timezone inputs fonts pubKeys; nvim = nvim.packages.aarch64-linux;};}
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
      "fokophone" = let hostname= "fokophone";
      in nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = { inherit inputs;};
        modules = [
          (import "${inputs.mobile-nixos}/lib/configuration.nix" {device="pine64-pinephone";})
          {_module.args = {inherit username timezone inputs hostname;};}
          ./hosts/fokopine/configuration.nix
          (inputs.mobile-nixos + /examples/phosh/phosh.nix)
        ];
      };
      "fokolaptop" = let hostname = "fokolaptop"; 
      in nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs hostname;};
	      modules = [
	        {_module.args = {inherit username timezone inputs fonts pubKeys; nvim = nvim.packages.x86_64-linux;};}
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
    nixOnDroidConfigurations.default = nix-on-droid.lib.nixOnDroidConfiguration rec {
      pkgs = import nixpkgs { system = "aarch64-linux"; };
      extraSpecialArgs = {
        inherit inputs;
      };
      modules = [
        {_module.args = {inherit inputs; nvim = nvim.packages.nvimBuilder pkgs;};}
        #inputs.discord.nixosModules.discord
        #inputs.stylix.nixOnDroidModules.stylix
        #./configuration.nix
        ./hosts/fokophone
      ];
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
