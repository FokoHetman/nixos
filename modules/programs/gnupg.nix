{...}: {
  flake.nixosModules.gnupg = {pkgs, ...}: {
    programs.gnupg.agent = {
      enable = true;
      pinentryPackage = pkgs.pinentry-curses;
      enableSSHSupport = true;
    };
  };
}
