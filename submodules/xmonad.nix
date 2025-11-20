{pkgs, ...}:
{
  environment.etc.xmobar.source = ../assets/xmobar;

  /*services.xserver.displayManager.sessionCommands = ''
    ${pkgs.xcompmgr}/bin/xcompmgr &
  '';*/
  services.xserver = {
    xkb.layout = "pl,ru";
    #xkbOptions = "grp:win_space_toggle";
    /*windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: with haskellPackages; [
        dbus
        List
        monad-logger
        xmobar
      ];
      #config = ../assets/xmonad/xmonad.hs;
      enableConfiguredRecompile = true;
    };*/
  };
  programs.i3lock = {
    enable = true;
    package = pkgs.i3lock-fancy-rapid;
  };
  environment.systemPackages = with pkgs; [
    (pkgs.callPackage ../assets/xmonad {shell=false;})
    (pkgs.writeShellScriptBin "cxmobar" ''
      while :
      do
        custom-xmobar
      done
    '')
    (pkgs.callPackage ../assets/xmobar {shell=false;})
    #haskellPackages.xmobar
    haskellPackages.monad-logger
    haskellPackages.dbus
    haskellPackages.List
    pango
    xorg.xwininfo
    xorg.libX11
    xorg.libX11.dev
    xorg.libXft
    xorg.libXft.dev
    xorg.libXext
    xorg.libXrandr
    xorg.libXrender
    xorg.libXinerama
    xorg.libXScrnSaver
    feh
    (pkgs.writeShellScriptBin "layout-sw" ''
      case $(setxkbmap -query | grep -oP "(?<=layout:).*" | tr -d [:space:]) in
        pl  ) setxkbmap ru;;
        ru  ) setxkbmap pl;;
        *   ) setxkbmap pl;; # fallback
      esac
    '')
  ];
}
