{config, pkgs, inputs, ...}:
let
  lambdagreet = inputs.blackmarket.legacyPackages.${pkgs.system}.lambdagreet;
  tty = "1";
in
{
  systemd.services.lambdagreet = {
    description = "Lambda Greet";
    wantedBy = [ "multi-user.target" ];
    after = [ "multi-user.target" ];
    before = [ "getty@tty${tty}.service" ];

    serviceConfig = {
      ExecStart = "${lambdagreet}/bin/greeter";
      StandardInput = "tty";
      StandardOutput = "tty";
      StandardError = "file:/home/foko/lambdalogs.txt";
      TTYPath = "/dev/tty${tty}";
      TTYReset = "yes";
      TTYVHangup = "yes";
      PAMName = "login";
      #Restart = "always";
      Environment = [
        "XDG_DATA_DIRS=${config.environment.sessionVariables.XDG_DATA_DIRS}"
        "XDG_VTNR=${tty}"
      ];
    };
  };
  systemd.services."getty@tty${tty}".enable = false;
}
