{self, ...}: {
  flake.nixosModules.radicle = {config, pkgs, lib, ...}: {
    services.radicle = {
      enable = true;
      publicKey = lib.head self.globals.fokossh;
      privateKeyFile = config.sops.secrets.ssh_rsa.path;
      settings = {
        web.pinned.repositories = [];
        node = {
          alias = "foko";
          peers.type = "dynamic";
          seedPolicy = {
            default = "block";
          };
        };
      };
    };
  };
}
