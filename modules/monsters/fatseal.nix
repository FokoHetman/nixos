{inputs,...}: {
  flake.nixosModules.monsters-fat-seal = {pkgs,system,...}: {
    imports = [inputs.blackmarket.nixosModules.monster.default];
    monster = {
      enable = true;
      monsters."fatass-seal" = {
        enable = true;
        startSize = 100;
        maxSize=20000;
      };
    };
  };
}
