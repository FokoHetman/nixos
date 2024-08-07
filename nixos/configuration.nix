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

  sops.defaultSopsFile = ../secrets/secrets.yaml;
  sops.defaultSopsFormat = "yaml";
  sops.age.keyFile = "/home/${username}/.config/sops/age/keys.txt";
  sops.secrets.shrimp = { };


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


  hardware.nvidia = {
    modesetting.enable=true;
    powerManagement.enable = false;

    powerManagement.finegrained=false;

    open=false;
    nvidiaSettings=true;

    package = config.boot.kernelPackages.nvidiaPackages.beta;
  };






  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = ["ntfs"];

  networking= {
    hostName = "${hostname}";
    wireless = {
      enable = true;
    };
    interfaces.enp5s0.wakeOnLan.enable=true;
    firewall.allowedTCPPorts = [22 44];
    firewall.allowedUDPPorts = [];
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
    xserver = {
      enable = true;
      xkb.layout = "pl";
      xkb.options = "eurosign:e,caps:escape";
      videoDrivers = ["nvidia"];
    };
    upower.enable=true;
    displayManager.sddm = {
      enable = true;
      wayland = {
        enable = true;
      };
    };
    blueman.enable = true;
    printing.enable = true;
    ollama = {
      enable = true;
      acceleration = "cuda";
    };
  };


  programs = {
    hyprland = {
      enable=true;
      package = inputs.hyprland.packages."${pkgs.system}".hyprland;
    };
    steam = {
      enable = true;
      gamescopeSession.enable = true;
    };
    gamemode.enable = true;
  };

  hardware.pulseaudio.enable = true;
  sound.enable = true;
  security.rtkit.enable = true;


  users.users = {
    root = {
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
  xdg.portal = {
    enable = true;
    extraPortals = [pkgs.xdg-desktop-portal-gtk ];
  };


  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [

    #inputs.nixvim.packages.${system}.default
    inputs.fokquote.packages.${system}.default
    inputs.chess.packages.${system}.default#.packages.${system}.default

    sops

    unzip
    neofetch
    bluez
    qemu
    blockbench
    blender
    godot_4
    vlc
    libvlc
    

    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    nmap
    wget
    git
    curl
    zip

    grim
    slurp
    fuzzel
    
    wl-clipboard
    wf-recorder
    libnotify

    imagemagick7
    dunst
    
    xdg-desktop-portal
    gtk3
    qt6ct

    nasm
    gcc
    rustc
    cargo

    ncurses

    ffmpeg
    
    pulseaudio
    
    texliveMedium
    tetex

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

