{pkgs, self, ...}: {
  imports = [
    self.nixosModules.fcitx
    self.nixosModules.xmobar
  ];
  programs.i3lock = {
    enable = true;
    package = pkgs.i3lock-fancy-rapid;
  };
  environment.systemPackages = with pkgs; [
    haskellPackages.monad-logger
    haskellPackages.dbus
    haskellPackages.List
    pango
    playerctl
    xdotool
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
      case $(fcitx5-remote -n) in
        keyboard-pl   ) fcitx5-remote -s keyboard-ru;;
        keyboard-ru   ) fcitx5-remote -s mozc;;
        mozx          ) fcitx5-remote -s keyboard-pl;;
        *             ) fcitx5-remote -s keyboard-pl;; # fallback
      esac
    '')
    (pkgs.writeShellScriptBin "xlayout-sw" ''
      case $(setxkbmap -query | grep -oP "(?<=layout:).*" | tr -d [:space:]) in
        pl  ) setxkbmap ru;;
        ru  ) setxkbmap pl;;
        *   ) setxkbmap pl;; # fallback
      esac
    '')
    
    #https://www.reddit.com/r/xmonad/comments/j5419h/gif_screen_capture/ | thank you, roboboticus.
    slop
    killall
  ] ++ (with self.packages.${system}; [
    xmonad
    xmobar
    ffmpegxcb
    screencast
    layout-sw
    xlayout-sw
  ]);
}
