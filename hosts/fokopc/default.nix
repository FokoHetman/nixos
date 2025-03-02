{config, lib, pkgs, inputs, ...}:
{
  imports = [
    ./hardware-configuration.nix
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




  systemd.services.nvidia-control-devices = {
    wantedBy = [ "multi-user.target" ];
    serviceConfig.ExecStart = "${pkgs.linuxPackages.nvidia_x11.bin}/bin/nvidia-smi";
  };
  networking.interfaces.enp5s0.wakeOnLan.enable=true;


  environment.defaultPackages = with pkgs; [

    zathura

    cudatoolkit
    qemu

    blockbench
    godot_4

    vlc
    libvlc

    wineWowPackages.waylandFull

    xdg-desktop-portal
    gtk3
    qt6ct
    libsForQt5.qt5.qtgraphicaleffects
  ];

}
