# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, lib, pkgs, hostname, username, timezone, inputs, nvim, fonts, pubKeys, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      submodules/secrets.nix
      submodules/networking.nix
      submodules/theme.nix
      #submodules/discord.nix
      submodules/rainworld.nix
      (./hosts + "/${hostname}")
      inputs.sops-nix.nixosModules.sops
      inputs.nix-index-database.nixosModules.nix-index
      { programs.nix-index-database.comma.enable = true; }
    ];


  sops.defaultSopsFile = ./secrets/secrets.yaml;
  sops.defaultSopsFormat = "yaml";
  sops.age.keyFile = "/home/${username}/.config/sops/age/keys.txt";
  sops.secrets.shrimp = { owner = username; };
  sops.secrets.ds_token = { owner = username; };
  sops.secrets.fok = {};
  sops.secrets.steam_key = { owner = username; };
  sops.secrets.steam_id  = { owner = username; };
  
  nix.buildMachines = if hostname != "fokopc" then [ {
	  hostName = "hetman.at:2136";
    #system = "x86_64-linux";
    protocol = "ssh-ng";
	  # if the builder supports building for multiple architectures, 
	  # replace the previous line by, e.g.
    systems = ["x86_64-linux" "aarch64-linux"];
	  maxJobs = 1;
	  speedFactor = 2;
	  supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
	  mandatoryFeatures = [ ];
	}] else [];

  nix.settings.extra-platforms = config.boot.binfmt.emulatedSystems;
	nix.distributedBuilds = true;
	# optional, useful when the builder has a faster internet connection than yours
	nix.extraOptions = ''
	  builders-use-substitutes = true
	'';
  nix.registry.blackmarket.flake = inputs.blackmarket;

  rainworld = {
    enable = true;
    steamkey_path = config.sops.secrets.steam_key.path;
    steamid_path = config.sops.secrets.steam_id.path;
  };
  monster = {
    enable = true;
    monsters."jerry" = {
      enable = true;
      startSize = 80;
      maxSize=100;
    };
  };
  /*discord = {
    enable = true;
    token_path = config.sops.secrets.ds_token.path;
    servers."Development" = {
      categories."Praetorians" = {
        channels = {
          "general" = {};
          "git-logs" = {permissions.roles."praetorians_access".sendMessages = false;};
        };
        permissions.roles."everyone".viewChannel = false;
        permissions.roles."praetorians_access".viewChannel = true;
      };
      categories."chessdb" = {
        channels = {
          "general" = {permissions.sync=true;};
          "git-logs" = {
            permissions.roles."everyone".viewChannel = false;
            permissions.roles."chessdb_access".sendMessages = false;
          };
        };
        permissions.roles."everyone".viewChannel = false;
        permissions.roles."chessdb_access".viewChannel = true;
      };
    };
    servers."TheTest" = {
      roles = {

      };
      categories."Text" = {
        channels = {
          "general" = {permissions.sync = true;};
          "memes" = {
            permissions.roles."memer".sendMessages = false;
          };
        };
        permissions = {
          roles."silly".sendMessages = false;
          roles."silly".viewChannel = true;
          roles."everyone".viewChannel = false;
        };
      };
      categories."Text2" = {
        channels = {
          "general" = {
            permissions.roles."everyone".sendMessages = true;
            permissions.sync = true;
          };
          "gallery" = {
            permissions.roles."silly".sendMessages = false;
            permissions.roles."memer".viewChannel = true;
            permissions.sync = false;
          };
        };
        permissions = {
          roles."memer".viewChannel = false;
          roles."silly".sendMessages = true;
        };
      };
    };
  };*/


  nixpkgs.config = if hostname!="fokopi" then {
    allowUnfree = true;
    cudaSupport = true;
  } else {};
  nix = {
    nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
    settings = {
      trusted-users = ["@wheel"] ++ (if hostname=="fokopc" then ["builder"] else []);
      experimental-features = [ "nix-command" "flakes" "pipe-operators"];
    };
    settings.secret-key-files = "/etc/nix/private-key";
  };



  # Use the systemd-boot EFI boot loader.
  boot = {
    loader = {
      grub = {
        enable = true;
        useOSProber = true;
        device = "nodev";
        efiSupport = true;
      };
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot";
      };
    };
    supportedFilesystems = ["ntfs"];
  };



  networking = {
    hostName = "${hostname}";
    wireless = {
      enable = true;
    };
    
    firewall.allowedTCPPorts = [22 25 44 80 443 2137 2138 5900 5901 8000 8080 25565];
    firewall.allowedUDPPorts = [5900 5901 25565];
    firewall.enable = true;

    #proxy.default = "http://user:password@proxy:port/";
    #proxy.noProxy = "127.0.0.1,localhost,internal.domain";
  };

  time.timeZone = "${timezone}";


  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  console = {
  #   font = "Lat2-Terminus16";
    keyMap = "pl";
  #   useXkbConfig = true; # use xkb.options in tty.
  };

  

  #hardware.pulseaudio.enable = true;
  security.rtkit.enable = true;


  users.groups.fok.gid = 2137;
  users.users = {
    root = {
    };
    ${username} = {
      isNormalUser = true;
      extraGroups = [ "wheel" "fok" "dialout" "firejail" "tty" "wireshark" ];
      packages = with pkgs; [
        
      ];
    };
  };


  home-manager = {
    extraSpecialArgs = { inherit inputs username fonts; };
    backupFileExtension = "backup";
  };
  xdg.portal = {
    enable = true;
    extraPortals = [pkgs.xdg-desktop-portal-gtk ];
  };


  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    packages = with pkgs; [

    ] ++ lib.attrsets.attrValues fonts;
  };

  services.pcscd.enable = true;
  programs.gnupg.agent = {
    enable = true;
    pinentryPackage = pkgs.pinentry-curses;
    enableSSHSupport = true;
  };


  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    nvim
    (inputs.blackmarket.legacyPackages.${system}.fok)

    asciinema_3

    feedbackd
    
    xcompmgr xdotool
    playerctl

    pkg-config

    haskellPackages.cabal-install
    haskellPackages.haskell-language-server
    haskellPackages.hlint
    haskellPackages.ghcid
    haskellPackages.ormolu
    haskellPackages.implicit-hie
    haskellPackages.X11
    texlab
  
    protonup-ng

    udiskie
    pinentry-curses

    ripgrep
    
    inputs.fokquote.packages.${system}.default
    inputs.chess.packages.${system}.default
    inputs.fokutils.packages.${system}.default
    #inputs.fokshell.packages.${system}.default


    alsa-lib


    sops

    neofetch
    bluez
        
    
    vim
    bat

    nmap wget git curl
    
    zip unzip

    xclip scrot grim slurp fuzzel libnotify dunst


    wl-clipboard wf-recorder imagemagick 

    nasm gcc rustc cargo ghc zig
    
    ncurses
    ffmpeg
    
    pulseaudio alsa-utils
    nixd
    
    (pkgs.writeShellScriptBin "foko-git" /*bash*/ ''
      #! ${pkgs.bash}/bin/bash
      case $1 in 
        register  ) ${pkgs.passh}/bin/passh -pgit ssh git@fokopi -t "gitserver newuser $2; exit";;
        newrepo   ) ${pkgs.passh}/bin/passh -pgit ssh git@fokopi -t "gitserver newrepo $2/$3; exit";;
        addbanner ) scp $4 git@fokopi:$2/$3/banner.png; exit;;
        describe  ) ${pkgs.passh}/bin/passh -pgit ssh git@fokopi -t "echo \"$4;$5;$6\" > $2/$3/description; exit";;
        attach    ) shift && scp $@ foko@fokopi:FokWeb/src/static/dynamic/ && echo "attached at ROOT/static/dynamic/";;
        *         ) echo "Bad Usage
                      register [username]
                      newrepo [username] [reponame]
                      addbanner [username] [reponame] [file]
                      describe [username] [reponame] [name] [desc] [tags [tag:color]]
                      attach [files]";;
        esac
    '')
    
    (pkgs.writeShellScriptBin "nixos" /*bash*/ ''
      #! {pkgs.bash}/bin/bash
      case $1 in
        sw      ) sudo nixos-rebuild switch;;
        test    ) sudo nixos-rebuild test;;
        edit    ) sudo lf /etc/nixos;;
	      up	    ) sudo nix flake update --flake /etc/nixos;;
        *       ) echo "Bad Usage";;
      esac

    '')
  ];

  environment.sessionVariables = {
    MOZ_ENABLE_WAYLAND = 1;
    STEAM_EXTRA_COMPAT_TOOLS_PATHS =
      "\${HOME}/.steam/root/compatibilitytools.d";
  };
  environment = {
    variables = {
      EDITOR = "nvim";
    };
    /*shellAliases = {
      ll = "colorls -l";
      ".." = "cd ..";
      la = "colorls -a";
      lla = "colorls -la";
      ls = "colorls";
    };*/
  };


  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:







  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "23.11"; # Did you read the comment?

}

