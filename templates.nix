{lib, inputs, architectures, nixpkgsConfig, ...}:
let 
  mkPkgs = arch : import inputs.nixpkgs ({ system = arch; }//nixpkgsConfig);
  inheritance = name: def: {
    preconf = x: x;
    builder = x: {${name} = x;};
    default = def;
  };
in rec {
  perSystem = {
    preconf = x: builtins.mapAttrs (name: f: builtins.listToAttrs (map (arch: {name = arch; value = f {pkgs=mkPkgs arch;};}) architectures)) x;
    builder = x: x;
    default = {};
  };
  nixosModules = inheritance "nixosModules" {};
  nixosConfigurations = inheritance "nixosConfigurations" {};
  homeModules = inheritance "homeModules" {};
  homeConfigurations = inheritance "homeConfigurations" {};
  globals = inheritance "globals" {};
}
