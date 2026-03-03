{
  description = "A templating and modularization utility.";
  inputs = {
    nixpkgs-lib.url = "github:nix-community/nixpkgs.lib";
  };
  outputs = {self, nixpkgs-lib, ...}: let inherit (nixpkgs-lib) lib; in {
    lib.makeConfig = {self,inputs}: {imports}: let 
      default = {templates = {};};
      flake' = default // builtins.foldl' lib.recursiveUpdate {} (map (x : (import x {inherit inputs self lib;}).flake) imports);
      flake = (builtins.mapAttrs (name: value: value.default) flake'.templates) // flake';
    in builtins.foldl' lib.recursiveUpdate {} (lib.mapAttrsToList (name: value: value.builder flake.${name}) flake.templates);
    lib.import-tree = path:
      lib.fileset.toList (lib.fileset.fileFilter (file: file.hasExt "nix" 
        && !(lib.hasPrefix "_" file.name 
          || lib.hasPrefix "." file.name
          || lib.elem file.name ["default.nix" "shell.nix"]
        )) path);
  };
}
