{inputs, ...}: {
  flake.nixosModules.fokshell = {pkgs,...}: {
    environment.systemPackages = [inputs.fokshell.packages.${pkgs.system}.hetmanshell];
  };
}
