{inputs, ...}: {
  perSystem = {pkgs, ...}: {
    packages.nvim = inputs.nvf.lib.neovimConfiguration {
      inherit pkgs;
      modules = [./submodules/nvf-configuration.nix];
    }.neovim;
  };
}
