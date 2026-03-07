{...}: {
  flake.homeModules.nixpkgs = {...}: {
    nixpkgs = {
      overlays = [
      ];
      config = {
        packageOverrides = self: rec {
          blender = self.blender.override {
            cudaSupport = true;
          };
        };
        allowUnfree = true;
        allowUnfreePredicate = _: true;
        permittedInsecurePackages = [
          "libsoup-2.74.3"
        ];
      };
    };
  };
}
