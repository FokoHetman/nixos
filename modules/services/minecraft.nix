{...}: {
  flake.nixosModules.minecraft = {...}: {
    networking.firewall.allowedTCPPorts = [ 25565 ];
  };
}
