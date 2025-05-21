{lib, ...} :
{
  home-manager.users.nathan.stylix.enable = false;
  home-manager.users.nathan.catppuccin.enable = lib.mkForce false; # now it breaks because of catppuccin desync lmfao
}
