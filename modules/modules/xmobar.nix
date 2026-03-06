{self, ...}: {
  flake.nixosModules.xmobar = {pkgs,...}: {
    systemd.user.services.xmobar = {
      enable = true;
      description = "My XMobar";
      after = ["graphical-session.target"];
      wantedBy = [];
      path = with pkgs; [playerctl xdotool];
      serviceConfig = {
        ExecStart = "${self.packages.${pkgs.system}.xmobar}/bin/custom-xmobar";
        Restart = "on-failure";
        RestartSec = 1;
        Environment = [
          "DISPLAY=:0"
          "XAUTHORITY=%h/.Xauthority"
          "DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/%U/bus"
        ];
      };
    };
  };
}
