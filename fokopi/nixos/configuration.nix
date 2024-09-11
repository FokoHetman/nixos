# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, lib, pkgs, hostname, username, timezone, inputs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      submodules/hardware-configuration.nix
      submodules/secrets.nix
      submodules/networking.nix
      submodules/theme.nix
      inputs.sops-nix.nixosModules.sops
    ];







  nixpkgs.config.allowUnfree = true;
  
  nix.settings = {
    experimental-features = [ "nix-command" "flakes" ];
  };



  
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;








  # Use the systemd-boot EFI boot loader.
  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;
  boot.kernelPackages = pkgs.linuxPackages_rpi4;

  # boot.loader.efi.canTouchEfiVariables = true;
  
  networking= {
    hostName = "${hostname}";
    wireless = {
      enable = true;
    };
    firewall.allowedTCPPorts = [22 44 3000];
    firewall.allowedUDPPorts = [53];
    firewall.enable = true;

    #proxy.default = "http://user:password@proxy:port/";
    #proxy.noProxy = "127.0.0.1,localhost,internal.domain";
  };

  time.timeZone = "${timezone}";


  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkb.options in tty.
  # };


  services = {
    /*adguardhome = {
      openFirewall = true;
      port = 3000;
      settings = {
        # bind_port = adguardPort;
        schema_version = 20;
      };
      settings = {
	http = {
	  address = "127.0.0.1:53";
	};
	dns = {
	  upstream_dns = [
	    "8.8.8.8"
	  ];
	};
	filtering = {
	  protection_enabled = true;
	  filtering_enabled = true;
	};
	filters = map(url: { enabled = true; url = url; }) [
          "https://adguardteam.github.io/HostlistsRegistry/assets/filter_9.txt"  # The Big List of Hacked Malware Web Sites
          "https://adguardteam.github.io/HostlistsRegistry/assets/filter_11.txt"  # malicious url blocklist
        ];
      };
    };*/
    blueman.enable = true;

  };


  hardware.pulseaudio.enable = true;
  security.rtkit.enable = true;


  users.users = {
    root = {
    };
    git = {
      isNormalUser = true;
      packages = with pkgs; [
	(pkgs.writeShellScriptBin "gitserver" /*bash*/ '' # $1 - command $2 - 'user/repo' $3 - hash (soon tm)
	  case $1 in
	    newuser 	) mkdir $2;;
	    newrepo	) git --bare init $2;;
	  esac
	'')
      ];
    };
    ${username} = {
      isNormalUser = true;
      extraGroups = [ "wheel" ];
      packages = with pkgs; [
        
      ];
    };

  };
  home-manager = {
    extraSpecialArgs = { inherit inputs username; };
    users = {
      "${username}" = import ./../home-manager/home.nix;
    };
    backupFileExtension = "backup";
  };



  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [

    #inputs.nixvim.packages.${system}.default
    inputs.fokquote.packages.${system}.default
    inputs.chess.packages.${system}.default
    inputs.fokutils.packages.${system}.default

    sops

    unzip
    neofetch
    bluez


    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.

    wget
    git
    curl
    zip


    
    wl-clipboard


    imagemagick7

    nasm
    gcc
    rustc
    cargo

    ncurses

    ffmpeg
    

    
    (pkgs.writeShellScriptBin "nixos" /*bash*/ ''
      #! {pkgs.bash}/bin/bash
      case $1 in
        sw      ) sudo nixos-rebuild switch;;
        test      ) sudo nixos-rebuild test;;
        edit    ) sudo lf /etc/nixos;;
	up	) sudo nix flake update /etc/nixos;;
        *       ) echo "Bad Usage";;
      esac

    '')
    

    #(pkgs.writeShellScriptBin "theme_update" /*bash*/ ''
    #  cp /etc/nixos/nixos/wallpaper/pool/$(ls /etc/nixos/nixos/wallpaper/pool | shuf -n 1) /etc/nixos/nixos/wallpaper/base.jpg
    #  nixos-rebuild test
    #'')


  ];
  environment = {
    variables = {
      EDITOR = "nvim";
    };
    shellAliases = {
      ll = "ls -l";
      ".." = "cd ..";
      la = "ls -a";
      lla = "ls -la";
      ls = "ls";
    };
  };


  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh = {                                                    enable = true;                                                        ports = [ 22 ];                                                       settings = {                                                            PasswordAuthentication = true;                                        AllowUsers = null; # Allows all users by default. Can be [ "user1" "user2" ]
    };
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

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

