{...}: {
  flake.nixosModules.fokmail = {...}: {
    networking.firewall.allowedTCPPorts = [ 25 ];
  };
}
