{lib, self, ...}: {
  flake.homeModules.ghci = {pkgs, ...}: {
    home.file.".ghci" = {
      text = ''
        :set prompt "\955> "
      '';
    };
  };
}
