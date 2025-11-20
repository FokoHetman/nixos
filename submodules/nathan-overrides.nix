{lib, pkgs, ...} :
{
  home-manager.users.nathan.stylix.enable = false;
  home-manager.users.nathan.catppuccin.enable = lib.mkForce false; # now it breaks because of catppuccin desync lmfao
  #home-manager.users.nathan.home.packages = lib.mkForce [];
}
