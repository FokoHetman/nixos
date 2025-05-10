{pkgs, ...}:
{
  environment.etc.xmobar.source = ../assets/xmonad/src/xmobar;

  services.xserver.displayManager.sessionCommands = ''
    ${pkgs.xcompmgr}/bin/xcompmgr &
  '';
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    extraPackages = haskellPackages: [
      haskellPackages.dbus
      haskellPackages.List
      haskellPackages.monad-logger
    ];
    config = ../assets/xmonad/src/Main.hs;
    enableConfiguredRecompile = true;
  };
  programs.i3lock = {
    enable = true;
    package = pkgs.i3lock-fancy-rapid;
  };
}
