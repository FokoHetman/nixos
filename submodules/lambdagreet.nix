{config, pkgs, inputs, ...}:
let
  #lambdagreet = inputs.blackmarket.legacyPackages.${pkgs.system}.lambdagreet;
  tty = "1";
in
{
  /*systemd.services.lambdagreet = {
    description = "Lambda Greet";
    wantedBy = [ "multi-user.target" ];
    after = [ "multi-user.target" ];
    before = [ "getty@tty${tty}.service" ];
    conflicts = [ "getty@tty${tty}.service" ];

    serviceConfig = {
      ExecStart = "/bin/sh -c 'exec setsid ${lambdagreet}/bin/greeter' </dev/tty${tty}";
      StandardInput = "tty";
      StandardOutput = "tty";
      StandardError = "file:/home/foko/lambdalogs.txt";
      TTYPath = "/dev/tty${tty}";
      TTYReset = "yes";
      TTYVHangup = "yes";
      Type = "simple";
      User = "root";
      PAMName = "login";
      #Restart = "always";
      Environment = [
        "SYSTEMD_LOG_LEVEL=debug"
        "XDG_DATA_DIRS=${config.environment.sessionVariables.XDG_DATA_DIRS}"
        "XDG_VTNR=${tty}"
      ];
      UMask = "0022";
    };
    restartIfChanged = false;
  };
  #systemd.services."getty@tty${tty}".enable = false;*/
}
