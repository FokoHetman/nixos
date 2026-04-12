{lib,...}: {
  flake.nixosModules.nixpkgs = {...}: {
    nixpkgs.config.allowUnfree = true;
    nixpkgs.config.permittedInsecurePackages = [ "cisco-packet-tracer-8.2.2" ];
    nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
      "blender"
    ];
  };
}
