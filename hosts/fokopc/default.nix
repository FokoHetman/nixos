{config, lib, pkgs, inputs, username, pubKeys, ...}:
{
  imports = [
    ./hardware-configuration.nix
    ../../submodules/keymanagement.nix
    ../../submodules/xmonad.nix
    ../../submodules/nathan-overrides.nix
  ];

  services = {
    xserver = {
      enable = true;
      xkb.layout = "pl";
      xkb.options = "eurosign:e,caps:escape";
      videoDrivers = ["nvidia"];
    };
    upower.enable=true;
    displayManager = {
      sddm = {
	      theme = "${import ../../submodules/sddm.nix { inherit pkgs; }}";
        enable = true;
        wayland = {
          enable = true;
        };
      };
    };
    blueman.enable = true;
    printing.enable = true;
    /*ollama = {
      enable = true;
      acceleration = "cuda";
    };*/
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


  users.groups.builder = {};
  users.users = {
    foko = {
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAING43cVUOV9hmvkQNOKnYKcaBzamSFRnLGcLb0JlDlOZ paprykkania@gmail.com"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFs8Toyc7bQ9n6LV7czYtpCj6Ki5hItivcuWY21+iPfo nathan@nathanpc"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILYuSLogAXOTv1yZsaj2QuplHQ7Io5SYr6oALRtsnM1n u0_a246@localhost"
      ] ++ pubKeys;
    };
    builder = {
      group = "builder";
      isNormalUser = true;
      openssh.authorizedKeys.keys =  [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAING43cVUOV9hmvkQNOKnYKcaBzamSFRnLGcLb0JlDlOZ paprykkania@gmail.com"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFs8Toyc7bQ9n6LV7czYtpCj6Ki5hItivcuWY21+iPfo nathan@nathanpc"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILYuSLogAXOTv1yZsaj2QuplHQ7Io5SYr6oALRtsnM1n u0_a246@localhost"
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
  networking.interfaces.enp5s0.wakeOnLan.enable=true;


  environment.defaultPackages = with pkgs; [

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
