{pkgs, inputs, ...}:
let
  lambdagreet = inputs.blackmarket.legacyPackages.${pkgs.system}.lambdagreet
in
{
  systemd.services.lambdagreet = {
    description = "Lambda Greet";
    wantedBy = [ "multi-user.target" ];
    after = [ "systemd-user-sessions.service" ];
    before = [ "getty@tty1.service" ];

    serviceConfig = {
      ExecStart = "${lambdagreet}/bin/greeter";
      StandardInput = "tty";
      StandardOutput = "tty";
      TTYPath = "/dev/tty1";
      Restart = "always";
    };

    user = "root";
  };

  services.getty.enable = false;
}
