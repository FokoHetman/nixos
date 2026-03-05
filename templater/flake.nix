{
  description = "A templating and modularization utility.";
  inputs = {
    nixpkgs-lib.url = "github:nix-community/nixpkgs.lib";
  };
  outputs = {self, nixpkgs-lib, ...}: let inherit (nixpkgs-lib) lib; in {
    architectures = {
      default = ["x86_64-linux" "aarch64-linux"];
    };
    lib.makeConfig = {self,inputs}: {imports, architectures, nixpkgsConfig, templates}: let 
      templates' = templates {inherit lib inputs architectures nixpkgsConfig;};
      default = {};
      preprocess = x : builtins.foldl' lib.recursiveUpdate {} (lib.attrValues (lib.mapAttrs (name: value: if (lib.hasAttr name x) then {${name}=value.preconf x.${name};} else {}) templates'));
      subdefinitions = map (x: preprocess x.flake) (lib.filter (lib.hasAttr "flake") (map (x : import x {inherit inputs self lib architectures nixpkgsConfig;}) imports));
      flake' = default // builtins.foldl' lib.recursiveUpdate {} subdefinitions;
      flake = (builtins.mapAttrs (name: value: value.default) templates') // flake';
    in builtins.foldl' lib.recursiveUpdate {} (lib.mapAttrsToList (name: value: value.builder flake.${name}) templates');
    lib.import-tree = path:
      lib.fileset.toList (lib.fileset.fileFilter (file: file.hasExt "nix" 
        && !(lib.hasPrefix "_" file.name 
          || lib.hasPrefix "." file.name
          || lib.elem file.name ["default.nix" "shell.nix"]
        )) path);
  };
}
