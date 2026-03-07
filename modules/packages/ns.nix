{...}: {
  flake.perSystem.packages = {pkgs, ...}: {
    ns = pkgs.writeShellApplication {
        name = "ns";
        runtimeInputs = with pkgs; [
          fzf
          nix-search-tv
        ];
        # ignore checks since i didn't write this
        checkPhase = "";
        text = builtins.readFile "${pkgs.nix-search-tv.src}/nixpkgs.sh";
      };
  };
}
