{inputs, self, ...}: {
  flake.nixosModules.user-nathan = inputs.nathan.mkNathan {canSudo = true;};
}
