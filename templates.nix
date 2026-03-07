{lib, inputs, architectures, nixpkgsConfig, ...}:
  let mkPkgs = arch : import inputs.nixpkgs ({ system = arch; }//nixpkgsConfig);
in rec {
  perSystem = {
    preconf = x: builtins.mapAttrs (name: f: builtins.listToAttrs (map (arch: {name = arch; value = f {pkgs=mkPkgs arch;};}) architectures)) x;
    builder = x: x;
    default = {};
  };
  nixosModules = {
    preconf = x: x;
    builder = modules : {nixosModules = modules;};
    default = {};
  };
  nixosConfigurations = {
    preconf = x: x;
    builder = configurations : {nixosConfigurations = configurations;};
    default = {};
  };
  homeModules = {
    preconf = x: x;
    builder = modules: {homeModules = modules;};
    default = {};
  };
  homeConfigurations = {
    preconf = x: x;
    builder = configurations: {homeConfigurations = configurations;};
    default = {};
  };
  globals = {
    preconf = x: x;
    builder = x: {globals = x;};
    default = {};
  };
  /*flake.perSystem.packages = {pkgs, ...}: {
    hello = pkgs.hello;
  };
  /*flake.templates.hjemConfiguration = {
    builder = configurations : {nixosConfigurations = configurations;};
    default = {};
  };*/
}
