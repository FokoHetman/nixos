{self, ...}: {
  flake.nixosModules.gitserver = {pkgs, ...}: {
    services.openssh.settings.AllowUsers = ["git"];
    users.users.git = {
      isNormalUser = true;
      packages = with pkgs; [
        (pkgs.writeShellScriptBin "gitserver" /*bash*/ '' # $1 - command $2 - 'user/repo' $3 - hash (soon tm)
          case $1 in
            newuser ) mkdir $2;;
            newrepo) git --bare init $2;;
          esac
        '')
      ];
      openssh.authorizedKeys.keys = self.globals.fokossh;
    };
  };
}
