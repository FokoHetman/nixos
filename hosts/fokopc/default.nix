{config, lib, pkgs, inputs, username, ...}:
{
  imports = [
    ./hardware-configuration.nix
  ];


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



  users.users = {
    foko = {
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAING43cVUOV9hmvkQNOKnYKcaBzamSFRnLGcLb0JlDlOZ paprykkania@gmail.com"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFs8Toyc7bQ9n6LV7czYtpCj6Ki5hItivcuWY21+iPfo nathan@nathanpc"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILYuSLogAXOTv1yZsaj2QuplHQ7Io5SYr6oALRtsnM1n u0_a246@localhost"
      ];
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

    blockbench
    #godot_4

    vlc
    libvlc

    wineWowPackages.waylandFull

    xdg-desktop-portal
    gtk3
    qt6ct
    libsForQt5.qt5.qtgraphicaleffects
  ];

}
