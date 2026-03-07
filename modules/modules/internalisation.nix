{...}: {
  flake.nixosModules.internalisation = {...}: {
    console = {
      keyMap = "pl";
    };
  };
}
