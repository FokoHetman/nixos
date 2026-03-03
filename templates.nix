{lib, ...}:
{
  flake.templates.nixosModules = {
    builder = modules : {nixosModules = modules;};
    default = {};
  };
  flake.templates.nixosConfigurations = {
    builder = configurations : {nixosConfigurations = configurations;};
    default = {};
  };
  /*flake.templates.perSystem = {
    builder = modules : 
  };
  flake.templates.hjemConfiguration = {
    builder = configurations : {nixosConfigurations = configurations;};
    default = {};
  };*/
}
