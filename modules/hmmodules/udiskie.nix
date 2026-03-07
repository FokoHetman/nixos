{lib, self, ...}: {
  flake.homeModules.udiskie = {pkgs, ...}: {
    home.file.".config/udiskie/config.yml" = {
      source = ./dots/udiskie.yml;
    };
  };
}
