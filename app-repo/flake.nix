{
  description = "Collection of individual flakes such as games and balblablabla";
  inputs = {
    5dchess.link = "path:5dchess";
  };

  outputs = {self, nixpkgs, home-manager, ... }@inputs: {};
}
