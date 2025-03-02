{pkgs, lib, ...}:
{
  vim = {
    vimAlias = true;
    viAlias = true;
    #package = inputs.;
    theme = {
      enable = true;
      name = "catppuccin";
      style = "dark";
    };
    languages = {
      enableLSP = true;
      enableTreesitter = true;

      cpp.enable = true;
      lua.enable = true;
      rust.enable = true;
      ts.enable = true;
      nix.enable = true;
    };

    lsp.enable = true;

  };
}
