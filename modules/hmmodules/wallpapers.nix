{lib, self, ...}: {
  flake.homeModules.wallpapers = {pkgs, ...}: {
    home.file = lib.attrsets.mapAttrs' 
      (name: value: lib.attrsets.nameValuePair (".config/wallpapers/def/" + name) {source = pkgs.fetchurl { inherit (value) url sha256;};}) 
      self.globals.wallpapers;
  };
}
