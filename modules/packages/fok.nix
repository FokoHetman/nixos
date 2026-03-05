{inputs, self, ...}: {
  flake.nixosModules.packages-fok = { pkgs, ...}: {
    environment.systemPackages = [
      inputs.blackmarket.legacyPackages.${pkgs.system}.fokquote
      inputs.blackmarket.legacyPackages.${pkgs.system}.fok
    ];
  };
}
