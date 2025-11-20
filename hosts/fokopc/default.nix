{config, lib, pkgs, inputs, username, pubKeys, fonts, mkMonster, ...}:
{
  imports = [
    ./hardware-configuration.nix
    ../../submodules/keymanagement.nix
    ../../submodules/sessions.nix
    ../../submodules/xmonad.nix
    ../../submodules/lambdagreet.nix
    ../../submodules/nathan-overrides.nix
    ../../submodules/proxychains.nix
  ];

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [ xdg-desktop-portal-termfilechooser xdg-desktop-portal-hyprland ];
    config = {
      #common.default = ["gtk"];
    };
  };


  

  networking.firewall.allowedTCPPorts = [ 47984 47989 47990 48010 ];
  networking.firewall.allowedUDPPortRanges = [
    { from = 47998; to = 48000; }
    { from = 8000; to = 8010; }
  ];
  services = {
    sunshine = {
      enable = false;
      settings = {
        sunshine_name = "fokopc";
      };
      autoStart = true;
      capSysAdmin = true;
    };
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

    

    firejail.enable = true;
    wireshark.enable = true;
    /*hyprland = {
      enable=true;
      package = inputs.hyprland.packages."${pkgs.system}".hyprland;
    };*/
    steam = {
      enable = true;
      gamescopeSession.enable = true;
    };
    gamemode.enable = true;
    gamescope.enable = true;
  };

  home-manager.users."${username}" = import ../../user/${username}/home.nix;
  users.groups.builder = {};
  users.groups.guest   = {};
  users.users = {
    toast = {
      isNormalUser = true;
      group = "guest";
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM3xlUW2U02zcGS++Z/GIK6nda3t/e46y4u39CBhpmas toast@laptop"
      ] ++ pubKeys;
      packages = with pkgs; [
        kitty
        blender
        rsync
        x11vnc
        (pkgs.writeShellScriptBin "vnc" ''
        if [ "$1" == "help" ]; then
          echo "use this command to start a VNC session."
          echo "in order to connect to it, use your ssh with `-L 5901:localhost:5901` argument."
          echo "use some VNC connection app to connect to it, use `localhost:0` on port 5901 to connect."
          echo "I configured it to just use my xmonad configuration, if it doesn't work like you want it to I can install ya flux or xfce or anything simple really sooo."
          echo ""
          echo "that's all I think"
        else
          x11vnc -create -rfbport 5901
        fi
        '')
      ];
    };
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
    qt6Packages.qt6ct
    libsForQt5.qt5.qtgraphicaleffects
  ];# ++ inputs.blackmarket.monster.monster (import ../../monster.nix);

}
