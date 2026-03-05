{lib, ...}: {
  perSystem.packages = {pkgs,...}: let 
    mkFont = name: src: (pkgs.callPackage({pkgs}: pkgs.stdenv.mkDerivation {
      inherit name src;
      dontConfigure = true;
      installPhase = ''
        runHook preInstall
        install -Dm644 $src/*.ttf -t $out/share/fonts/opentype
        runHook postInstall
      '';
    }) { inherit pkgs; });
  in {
    fonts = lib.readDir ./fonts 
      |> lib.attrNames
      |> map (x: {name= x; value= mkFont x ./fonts/${x};});
  };
}
