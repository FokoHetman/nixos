{inputs,...}: {
  flake.perSystem.packages = {pkgs,...}: {blender=(import inputs.nixpkgs {inherit (pkgs) system;config.allowUnfree=true;}).blender.override {cudaSupport=true;};};
}
