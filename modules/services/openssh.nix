{...}: {
  flake.nixosModules.openssh = {config, pkgs, lib, ...}: {
    services.openssh = {
      enable = true;
      ports = [ 22 ];
      settings = {
        PasswordAuthentication = false;
        AllowUsers = [];
        X11Forwarding = true;
        PermitRootLogin = "no";
      };
    };
  };
}
