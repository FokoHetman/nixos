{pkgs ? import <nixpkgs> {}}:
pkgs.callPackage ./default.nix {shell=true;}
/*pkgs.mkShell {
  packages = with pkgs; [
    (callPackage ./default.nix {})
  ];
}*/
