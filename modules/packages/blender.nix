{inputs,...}: {
  flake.perSystem.packages = {pkgs,...}: {blender=(import inputs.nixpkgs-unstable {inherit (pkgs) system;config.allowUnfree=true;}).blender.override {cudaSupport=true;};};
}
