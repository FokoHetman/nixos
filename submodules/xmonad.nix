{pkgs, ...}: let
  myXmobar = pkgs.callPackage ../assets/xmobar {shell=false;};
in {
  environment.etc.xmobar.source = ../assets/xmobar;

  /*services.xserver.displayManager.sessionCommands = ''
    ${pkgs.xcompmgr}/bin/xcompmgr &
  '';*/
  services.xserver = {
    xkb.layout = "pl,ru,jp";
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
    (pkgs.callPackage ../assets/xmonad {shell=false;})
    /*(pkgs.writeShellScriptBin "cxmobar" ''
      while :
      do
        custom-xmobar
      done
    '')*/
    #haskellPackages.xmobar
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
    (pkgs.writeShellScriptBin "screencast" ''
set -u
expectedExitCode=0
checkError() {
    local exitCode=$?
    if [ "$exitCode" -eq "$expectedExitCode" ]
    then
        expectedExitCode=0
    else
        exit "$exitCode"
    fi
}
trap "checkError" ERR

exec > ~/.screencast.log 2>&1

all=false

while getopts "a" opt
do
    case "$opt" in
        a) all=true ;;
        *) exit 1   ;;
    esac
done

tmpfile="$(mktemp -t screencast-XXXXXXX)"
palette="$(mktemp -t palette-XXXXXXX)"
tmpfile_mkv="''${tmpfile}.mkv"
palette_png="''${palette}.png"

cleanup() {
    rm -f "$tmpfile" "$tmpfile_mkv" "$palette" "$palette_png"
}
trap "cleanup" EXIT QUIT TERM


fname=$(${zenity}/bin/zenity --entry --title="GIF Name" --text="Provide the name for the recorder GIF.")

mkdir -p "$HOME/Pictures"
output="$HOME/Pictures/$fname"

screenRegion() {
    xrandr \
        | grep \
              --max-count=1 \
              --only-matching \
              --extended-regexp \
              '[0-9]+x[0-9]+\+[0-9]+\+[0-9]+' \
        | tr x+ ' '
}

selectRegion() {
    slop --format "%w %h %x %y"
}

if "$all"
then
    region=$(screenRegion)
else
    region=$(selectRegion)
fi

read -r W H X Y < <(echo "$region")

# External process may stop the recording via signal (e.g. "killall ffmpeg"),
# in which case ffmpeg exits with code 255.
expectedExitCode=255
ffmpeg -y -f x11grab -s "$W"x"$H" -i :0.0+$X,$Y "$tmpfile_mkv"
expectedExitCode=0

#notify-send 'generating palette'
ffmpeg -y -i "$tmpfile_mkv" -vf fps=10,palettegen "$palette_png"

notify-send 'Generating GIF.'
ffmpeg -y -i "$tmpfile_mkv" -i "$palette_png" -filter_complex "paletteuse" $output.gif

mv $tmpfile_mkv $output.mkv
echo "$output"
notify-send 'Saved at $output.gif.'
echo $output.gif | ${pkgs.xclip}/bin/xclip -selection clipboard
    '')
    slop
    (ffmpeg.override {
      withXcb = true;
    })
    killall
  ];
}
