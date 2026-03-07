{...}: {
  flake.nixosModules.openssh = {config, pkgs, lib, ...}: let sshports = [ 22 ]; in {
    networking.firewall.allowedTCPPorts = sshports;
    services.openssh = {
      enable = true;
      ports = sshports;
      settings = {
        PasswordAuthentication = false;
        AllowUsers = [];
        X11Forwarding = true;
        PermitRootLogin = "no";
      };
    };
  };
}
