{...}: {
  flake.nixosModules.fokmail = {...}: {
    networking.firewall.allowedTCPPorts = [ 25 2525 ];
  };
}
