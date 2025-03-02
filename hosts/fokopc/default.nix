inputs:
{config, lib, pkgs, ...}:
{
  imports = [
    ./hardware-configuration.nix
  ];


  environment.defaultPackages = with pkgs; [
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
