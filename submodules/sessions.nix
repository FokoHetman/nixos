{pkgs, username, ...}:
let 
  label = "sessions";
in
{
  services.udisks2.enable = true;
  services.udev.packages = [
    (pkgs.writeTextFile {
      name = "ssh-keys";
      text = ''
         ACTION=="add", ATTRS{idVendor}=="13fe", ATTRS{idProduct}=="4300", NAME="${label}"
      '';
      destination = "/etc/udev/rules.d/66-keys.rules";
    })
  ];

  systemd.services.sshkeys = let 
    notify = pkgs.writers.writeDashBin "notify" ''
          export PATH=${with pkgs; lib.makeBinPath [ libnotify procps ]}:$PATH

          # get the logged-in X user
          xorg_pid=$(pgrep -x X | head)
          display=$(ps --no-headers -o cmd:1 -p "$xorg_pid" | grep -Po ' \K(:[0-9]) ')
          uid=$(ps --no-headers -o uid:1 -p "$xorg_pid")

          /run/wrappers/bin/sudo -u "#$uid" "DISPLAY=$display" \
            "DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/$uid/bus" \
            notify-send "$@" || true
        '';
    keys = pkgs.writeShellScriptBin "${label}" ''
export $(dbus-launch)
set -e

FIREFOX=${pkgs.firefox}/bin/firefox
USB_LABEL=${label}
USB_MOUNT=/run/media/${username}/$USB_LABEL
UMOUNT_BIN=${pkgs.udiskie}/bin/udiskie-umount

COOKIES_SOURCE=$USB_MOUNT/cookies.sqlite

PROFILE_DIR=$(mktemp -dt firefox-sessionizer-XXXXXX)
PROFILE_NAME="sessionized-$(date +%s)"
mkdir -p "$PROFILE_DIR"
echo "user_pref(\"browser.shell.checkDefaultBrowser\", false);" > "$PROFILE_DIR/user.js"
cp "$COOKIES_SOURCE" "$PROFILE_DIR/cookies.sqlite"
firefox --no-remote --profile "$PROFILE_DIR" &

FIREFOX_PID=$!
wait $FIREFOX_PID

rm -rf "$PROFILE_DIR"


$UMOUNT_BIN $USB_MOUNT


    '';
  in {
    enable = true;
    description="A cool USB Session Manager";
    requires = ["run-media-${username}-${label}.mount"];
    after = ["run-media-${username}-${label}.mount"];
    confinement.packages = [pkgs.dbus];
    
    environment = {
      DISPLAY=":0";
      XAUTHORITY="~/.Xauthority";
    };
    serviceConfig = {
      User = username;
      ExecStart = "${keys}/bin/${label}";
    };
    
    wantedBy = ["run-media-${username}-${label}.mount"];
  };
}
