{inputs, self, ...}: {
  flake.nixosModules.packages-fok = { pkgs, ...}: {
    environment.systemPackages = [
      inputs.blackmarket.fokquote
      inputs.blackmarket.fok
    ];
  };
}
