{self, ...}: {
  flake.nixosModules.user-solina = {config, pkgs, lib, ...}: {
    services.openssh.settings.AllowUsers = ["solina"];
    imports = [self.nixosModules.guest];
    users.users.toast = {
      isNormalUser = true;
      group = "guest";
      openssh.authorizedKeys.keys = self.globals.solinassh;
      packages = with pkgs; [
        git
        nodejs
      ];
    };
  };
}
