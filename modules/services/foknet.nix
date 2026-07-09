_: {
  flake.nixosModules.foknet = _: {
    networking.firewall.allowedTCPPorts = [ 2137 2138 ];
  };
}
