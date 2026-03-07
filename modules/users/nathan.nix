{inputs, ...}: {
  flake.nixosModules.user-nathan = {config, pkgs, lib, ...}: {
    services.openssh.settings.AllowUsers = ["nathan"];
    imports = [
      (inputs.nathan.mkNathan {canSudo = true;})
    ];
  };
}
