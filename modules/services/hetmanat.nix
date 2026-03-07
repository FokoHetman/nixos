{self, ...}: {
  flake.nixosModules.hetmanat = {...}: {
    imports = [
      self.nixosModules.nginx
    ];
    users.groups.hetmanat = {};
    networking.firewall.allowedTCPPorts = [ 2137 ];
    users.users = {
      nginx.extraGroups = ["hetmanat"];
      foko.extraGroups = [ "hetmanat" ];
    };
  };
}
