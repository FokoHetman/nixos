{...}: {
  flake.nixosModules.scp = {...}: {
    networking.firewall.allowedTCPPorts = [ 7777 ];
    networking.firewall.allowedUDPPorts = [ 7777 ];
  };
}
