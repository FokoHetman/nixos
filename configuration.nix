# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, lib, pkgs, hostname, username, timezone, inputs, nvim, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      submodules/secrets.nix
      submodules/networking.nix
      submodules/theme.nix
      #submodules/discord.nix
      (./hosts + "/${hostname}")
      inputs.sops-nix.nixosModules.sops
    ];

  sops.defaultSopsFile = ./secrets/secrets.yaml;
  sops.defaultSopsFormat = "yaml";
  sops.age.keyFile = "/home/${username}/.config/sops/age/keys.txt";
  sops.secrets.shrimp = { owner = username; };
  sops.secrets.ds_token = { owner = username; };


  discord = {
    enable = true;
    token_path = config.sops.secrets.ds_token.path;
    servers."TheTest" = {
      categories."Text" = {
        channels = {
          "general" = {};
          "memes" = {};
        };
      };
      categories."Text2" = {
        channels = {
          "general" = {};
          "memes" = {};
        };
      };
    };
  };

  /* UDEV */
  services.udisks2.enable = true;
  services.udev.packages = [
    (pkgs.writeTextFile {
      name = "ssh-keys";
      text = ''
         ACTION=="add", ATTRS{idVendor}=="13fe", ATTRS{idProduct}=="4300", NAME=fokokeys
      '';
      destination = "/etc/udev/rules.d/66-keys.rules";
    })
  ];
  system.activationScripts."udiskie-activation".text = ''
echo "udiskie config"
if [ ! -f /home/${username}/.config/udiskie/config.yml ]; then 
  echo "writing config.."
  echo -e '
program_options:
  tray: true
  menu: nested
  notify: true
device_config:
  - device_file: /dev/loop*
    ignore: true
ignore_device:
  - id_uuid: BC76714876710504
  ' > /home/${username}/.config/udiskie/config.yml
fi
echo "finished"
'';


  systemd.services.sshkeys = let 
    keys = pkgs.writeShellScriptBin "fokokeys" ''
set -e

KEYS=(id_rsa id_ed25519)

SSH_ADD=${pkgs.openssh}/bin/ssh-add
USB_LABEL=fokokeys
USB_MOUNT=/run/media/${username}/$USB_LABEL
UMOUNT_BIN=${pkgs.udiskie}/bin/udiskie-umount




for key in "''\${KEYS[@]}"; do
  $SSH_ADD $USB_MOUNT/$key
  echo "Added $key to ssh-agent!"
done
#${pkgs.libnotify}/bin/notify-send "Added $SSH_KEY to ssh-agent!"

$UMOUNT_BIN $USB_MOUNT
    '';
  in {
    enable = true;
    description="SSH Keys loaded from USB";
    requires = ["run-media-${username}-fokokeys.mount"];
    after = ["run-media-${username}-fokokeys.mount"];
    confinement.packages = [pkgs.dbus];
    environment = {
      SSH_AUTH_SOCK="/run/user/1000/ssh-agent.socket";
      SSH_ASKPASS="${pkgs.x11_ssh_askpass}/libexec/x11-ssh-askpass";
      DISPLAY=":0";
    };
    serviceConfig = {
      User = "foko";
      ExecStart = "${keys}/bin/fokokeys";
    };
    
    wantedBy = ["run-media-${username}-fokokeys.mount"];
  };

  systemd.user.services.ssh-agent = {
    enable = true;
    description="The SSH Agent";
    environment = {
      DISPLAY = ":0";
      SSH_AUTH_SOCK="%t/ssh-agent.socket";
    };
    serviceConfig = {
      Type="simple";
      ExecStart="${pkgs.openssh}/bin/ssh-agent -D -a $SSH_AUTH_SOCK";
    };
    wantedBy=["default.target"];
  };




  


  
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.cudaSupport = true;
  nix = {
    nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
    settings = {
      trusted-users = ["@wheel"];
      experimental-features = [ "nix-command" "flakes" ];
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
    
    firewall.allowedTCPPorts = [22 44 2137];
    firewall.allowedUDPPorts = [];
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
      nathan.stylix.enable = false;
      nathan.catppuccin.enable = lib.mkForce false;		# stylix sucks
      "${username}" = import ./user/${username}/home.nix;
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
    nvim

    
    #spotify
    #obsidian
    protonup

    udiskie


    
    #neovim#inputs.nixvim.packages.${system}.default
    inputs.fokquote.packages.${system}.default
    inputs.chess.packages.${system}.default
    inputs.fokutils.packages.${system}.default
    inputs.fokshell.packages.${system}.default


    alsa-lib


    sops

    unzip
    neofetch
    bluez
        
    
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    bat

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

    imagemagick
    dunst
    
    


    nasm
    gcc
    rustc
    cargo
    ghc
    zig


    ncurses

    ffmpeg
    
    pulseaudio

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
        test      ) sudo nixos-rebuild test;;
        edit    ) sudo lf /etc/nixos;;
	up	) sudo nix flake update --flake /etc/nixos;;
        *       ) echo "Bad Usage";;
      esac

    '')
        

    #(pkgs.writeShellScriptBin "theme_update" /*bash*/ ''
    #  cp /etc/nixos/nixos/wallpaper/pool/$(ls /etc/nixos/nixos/wallpaper/pool | shuf -n 1) /etc/nixos/nixos/wallpaper/base.jpg
    #  nixos-rebuild test
    #'')


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
  services.openssh = {
    enable = true;
    ports = [ 22 ];
    settings = {
      PasswordAuthentication = true;
      AllowUsers = null; # Allows all users by default. Can be [ "user1" "user2" ]
      #UseDns = true;
      X11Forwarding = true;
      #PermitRootLogin = "prohibit-password"; # "yes", "without-password", "prohibit-password", "forced-commands-only", "no"
    };
  };






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

