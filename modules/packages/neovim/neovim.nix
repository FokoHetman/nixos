{inputs, lib, ...}: {
  flake.perSystem.packages = {pkgs, ...}: {
    nvim = (inputs.nvf.lib.neovimConfiguration {
      inherit pkgs;
      modules = [./_nvf-configuration.nix];
    }).neovim;
  };
}
