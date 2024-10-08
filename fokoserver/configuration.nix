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
      inputs.nix-minecraft.nixosModules.minecraft-servers
      submodules/minecraft.nix
    ];
  nixpkgs.overlays = [inputs.nix-minecraft.overlay];

  nixpkgs.config.allowUnfree = true;
  
  nix.settings = {
    experimental-features = [ "nix-command" "flakes" ];
  };



  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };

  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;



  # Use the systemd-boot EFI boot loader.
  boot.loader = {
    grub = {
      enable = true;
      device = "nodev";
    };
    efi.canTouchEfiVariables = true;
  };

  networking= {
    hostName = "${hostname}";
    wireless = {
      enable = true;
    };
    interfaces.enp5s0.wakeOnLan.enable=true;
    firewall.allowedTCPPorts = [22 44];
    firewall.allowedUDPPorts = [ config.services.tailscale.port ];
    firewall.trustedInterfaces = [ "tailscale0" ];
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
    tailscale.enable = true;
    blueman.enable = true;
  };



  users.users = {
    root = {
    };
    ${username} = {
      isNormalUser = true;
      extraGroups = [ "wheel" ];
      packages = with pkgs; [
        
      ];
    };
    foko = {
      isNormalUser = true;
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


    unzip
    neofetch
    bluez    

    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    nmap
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
      ll = "colorls -l";
      ".." = "cd ..";
      la = "colorls -a";
      lla = "colorls -la";
      ls = "colorls";
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
  # services.openssh.enable = true;

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

