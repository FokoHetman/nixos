{...}: {
  flake.nixosModules.dev-haskell = {pkgs, ...}: {
    environment.systemPackages = with pkgs.haskellPackages; [
      (pkgs.ghc)
      cabal-install
      haskell-language-server
      hlint
      ghcid
      ormolu
      implicit-hie
      X11
    ];
  };
}
