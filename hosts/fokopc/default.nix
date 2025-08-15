{config, lib, pkgs, inputs, username, pubKeys, mkMonster, ...}:
{
  imports = [
    ./hardware-configuration.nix
    ../../submodules/keymanagement.nix
    ../../submodules/xmonad.nix
    ../../submodules/lambdagreet.nix
    ../../submodules/nathan-overrides.nix
  ];

  services = {
    xserver = {
      /*doesn't work either way ffs
      displayManager.sessionCommands = "${pkgs.xorg.xmodmap}/bin/xmodmap ${pkgs.writeText "xbk-layout" ''
        keysym e = e E ę
      ''}";*/
      enable = true;
      #xkb.layout = "pl";
      #xkb.options = "eurosign:e,caps:escape";
      videoDrivers = ["nvidia"];
      displayManager = lib.mkDefault {
        startx.enable = true;
        sx.enable = true;
      };
    };
    upower.enable=true;
    displayManager = {
      /*sddm = {
	      theme = "${import ../../submodules/sddm.nix { inherit pkgs; }}";
        enable = true;
        wayland = {
          enable = true;
        };
      };*/
    };
    blueman.enable = true;
    printing.enable = true;
    /*ollama = {
      enable = true;
      acceleration = "cuda";
    };*/
  };

  environment.systemPackages = with pkgs; [
    #inputs.hyprland.packages.${system}.hyprland
  ];

  programs = {
    /*hyprland = {
      enable=true;
      package = inputs.hyprland.packages."${pkgs.system}".hyprland;
    };*/
    steam = {
      enable = true;
      gamescopeSession.enable = true;
    };
    gamemode.enable = true;
  };

  home-manager.users."${username}" = import ../../user/${username}/home.nix;
  users.groups.builder = {};
  users.users = {
    foko = {
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFs8Toyc7bQ9n6LV7czYtpCj6Ki5hItivcuWY21+iPfo nathan@nathanpc"
      ] ++ pubKeys;
    };
    builder = {
      group = "builder";
      isNormalUser = true;
      openssh.authorizedKeys.keys =  [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFs8Toyc7bQ9n6LV7czYtpCj6Ki5hItivcuWY21+iPfo nathan@nathanpc"
      ] ++ pubKeys;
    };
  };
  services.openssh = {
    enable = true;
    ports = [ 22 ];
    settings = {
      PasswordAuthentication = false;
      AllowUsers = null; # Allows all users by default. Can be [ "user1" "user2" ]
      #UseDns = true;
      X11Forwarding = true;
      PermitRootLogin = "no"; # "yes", "without-password", "prohibit-password", "forced-commands-only", "no"
    };
  };




  systemd.services.nvidia-control-devices = {
    wantedBy = [ "multi-user.target" ];
    serviceConfig.ExecStart = "${pkgs.linuxPackages.nvidia_x11.bin}/bin/nvidia-smi";
  };
  networking.interfaces.enp6s0.wakeOnLan.enable=true;


  environment.defaultPackages = with pkgs; [
    #(mkMonster 196883)

    zathura
    texliveMedium

    cudatoolkit
    qemu

    vlc
    libvlc

    xdg-desktop-portal
    gtk3
    qt6ct
    libsForQt5.qt5.qtgraphicaleffects
  ];

}
