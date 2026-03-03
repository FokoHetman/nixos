{inputs, self, ...}: {
  flake.nixosModules.xmonad = { pkgs, ...}: let
      myXmobar = pkgs.callPackage ./xmobar {shell=false;};
      myXmonad = pkgs.callPackage ./xmonad {shell=false;};
    in {
    #environment.etc.xmobar.source = ../assets/xmobar;
    services.xserver = {
      xkb.layout = "pl,ru,jp";
    };
    i18n.inputMethod = {
      enabled = "fcitx5";
      fcitx5.addons = with pkgs; [ fcitx5-mozc fcitx5-gtk kdePackages.fcitx5-qt];
      fcitx5.settings = {
        globalOptions = {
          Hotkey = {
            TriggerKeys = "Ctrl+Alt+space";
          };
        };
        inputMethod = {
          GroupOrder."0" = "Default";
          "Groups/0" = {
            Name = "Default";
            "Default Layout" = "pl";
            DefaultIM = "keyboard-pl";
          };
          "Groups/0/Items/0".Name = "keyboard-pl";
          "Groups/0/Items/1".Name = "keyboard-ru";
          "Groups/0/Items/2".Name = "mozc";
        };
      };
    };
    programs.i3lock = {
      enable = true;
      package = pkgs.i3lock-fancy-rapid;
    };
    systemd.user.services.xmobar = {
      enable = true;
      description = "My XMobar";
      after = ["graphical-session.target"];
      wantedBy = [];
      path = with pkgs; [playerctl xdotool];
      serviceConfig = {
        ExecStart = "${myXmobar}/bin/custom-xmobar";
        Restart = "on-failure";
        RestartSec = 1;
        Environment = [
          "DISPLAY=:0"
          "XAUTHORITY=%h/.Xauthority"
          "DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/%U/bus"
        ];
      };
    };
    environment.systemPackages = with pkgs; [
      myXmonad
      myXmobar
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
      killall
    ];
  };
}
