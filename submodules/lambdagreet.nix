{pkgs, inputs, ...}:
let
  lambdagreet = inputs.blackmarket.legacyPackages.${pkgs.system}.lambdagreet;
in
{
  systemd.services.lambdagreet = {
    description = "Lambda Greet";
    wantedBy = [ "multi-user.target" ];
    after = [ "systemd-user-sessions.service" ];
    before = [ "getty@tty1.service" ];

    serviceConfig = {
      ExecStart = "${pkgs.bash}/bin/bash -l -c ${lambdagreet}/bin/greeter";
      StandardInput = "tty";
      StandardOutput = "tty";
      StandardError = "journal";
      TTYPath = "/dev/tty1";
      TTYReset = "yes";
      TTYVHangup = "yes";
      Restart = "always";
      PAMName = "login";
    };
  };
  systemd.services."getty@tty1".enable = false;
}
