{inputs, ...}: {
  flake.nixosModules.tailnet = {...}: {
    imports = [(inputs.nathan.mkTailnet {})];
  };
}
