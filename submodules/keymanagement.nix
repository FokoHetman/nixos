{ pkgs, username, ...}:
let
  label = "fokokeys";     # modularize this entire module
in {
  /* UDEV */
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

KEYS=(id_rsa id_ed25519)

SSH_ADD=${pkgs.openssh}/bin/ssh-add
USB_LABEL=${label}
USB_MOUNT=/run/media/${username}/$USB_LABEL
UMOUNT_BIN=${pkgs.udiskie}/bin/udiskie-umount

for key in "''\${KEYS[@]}"; do
  $SSH_ADD $USB_MOUNT/$key
  echo "Added $key to ssh-agent!"
  ${notify}/bin/notify "Imported $key identities!"
done

$UMOUNT_BIN $USB_MOUNT


    '';
  in {
    enable = true;
    description="SSH Keys loaded from USB";
    requires = ["run-media-${username}-${label}.mount"];
    after = ["run-media-${username}-${label}.mount"];
    confinement.packages = [pkgs.dbus];
    
    environment = {
      SSH_AUTH_SOCK="/run/user/1000/ssh-agent.socket";
      SSH_ASKPASS="${pkgs.x11_ssh_askpass}/libexec/x11-ssh-askpass";
      DISPLAY=":0";
      XAUTHORITY="~/.Xauthority";
    };
    serviceConfig = {
      User = "foko";
      ExecStart = "${keys}/bin/${label}";
    };
    
    wantedBy = ["run-media-${username}-${label}.mount"];
  };

  systemd.user.services.ssh-agent = {
    enable = true;
    description="The SSH Agent";
    environment = {
      DISPLAY = ":0";
      SSH_AUTH_SOCK="%t/ssh-agent.socket";
    };
    serviceConfig = {
      Type="simple";
      ExecStart="${pkgs.openssh}/bin/ssh-agent -D -a $SSH_AUTH_SOCK";
    };
    wantedBy=["default.target"];
  };
}
