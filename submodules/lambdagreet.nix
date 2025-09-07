{pkgs, inputs, ...}:
let
  lambdagreet = inputs.blackmarket.legacyPackages.${pkgs.system}.lambdagreet;
in
{
  systemd.services.lambdagreet = {
    description = "Lambda Greet";
    wantedBy = [ "multi-user.target" ];
    after = [ "systemd-user-sessions.service" ];
    before = [ "getty@tty2.service" ];

    serviceConfig = {
      ExecStart = "${lambdagreet}/bin/greeter";
      StandardInput = "tty";
      StandardOutput = "tty";
      StandardError = "journal";
      TTYPath = "/dev/tty2";
      TTYReset = "yes";
      TTYVHangup = "yes";
      Restart = "always";
      PAMName = "login";
    };
  };
  systemd.services."getty@tty2".enable = false;
}
