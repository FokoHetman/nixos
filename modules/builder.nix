{self, ...}: {
  flake.nixosModules.builder = {...}: {
    nix.settings.trusted-users = ["builder"];
    users.groups.builder = {};
    users.users.builder = {
      group = "builder";
      isNormalUser = true;
      openssh.authorizedKeys.keys =  self.globals.fokossh ++ self.globals.nathanssh ++ self.globals.toastssh;
    };
  };
}
