{self, ...}: {
  flake.nixosModules.user-toast = {config, pkgs, lib, ...}: {
    imports = [self.nixosModules.guest];
    services.openssh.settings.AllowUsers = ["toast"];
    users.users.toast = {
      isNormalUser = true;
      group = "guest";
      openssh.authorizedKeys.keys = self.globals.toastssh;
      packages = with pkgs; [
        kitty
        blender
        rsync
        x11vnc
        (pkgs.writeShellScriptBin "vnc" ''
        if [ "$1" == "help" ]; then
          echo "use this command to start a VNC session."
          echo "in order to connect to it, use your ssh with `-L 5901:localhost:5901` argument."
          echo "use some VNC connection app to connect to it, use `localhost:0` on port 5901 to connect."
          echo "I configured it to just use my xmonad configuration, if it doesn't work like you want it to I can install ya flux or xfce or anything simple really sooo."
          echo ""
          echo "that's all I think"
        else
          x11vnc -create -rfbport 5901
        fi
        '')
      ];
    };
  };
}
