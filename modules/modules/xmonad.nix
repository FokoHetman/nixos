{self, inputs, ...}: {
  flake.nixosModules.xmonad = {pkgs, ...}: {
    imports = [
      self.nixosModules.fcitx
      self.nixosModules.xmobar
    ] ++ inputs.xmonad-contrib.nixosModules;
    programs.i3lock = {
      enable = true;
      package = pkgs.i3lock-fancy-rapid;
    };
    environment.systemPackages = with pkgs; [
      xcompmgr
      haskellPackages.monad-logger
      haskellPackages.dbus
      haskellPackages.List
      pango
      playerctl
      xdotool
      feh
      slop
      killall
      udiskie
    ] ++ (with pkgs.xorg; [
      xhost
      xwininfo
      libX11
      libX11.dev
      libXft
      libXft.dev
      libXext
      libXrandr
      libXrender
      libXinerama
      libXScrnSaver
    ]) ++ (with self.packages.${system}; [
        xmonad
        xmobar
        ffmpegxcb
        screencast
        layout-sw
        xlayout-sw
      ]);
  };
}
