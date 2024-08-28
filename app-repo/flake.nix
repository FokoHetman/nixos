{
  description = "Collection of individual flakes such as games and balblablabla";
  inputs = {
    "5dchess".url = "path:5dchess";
  };

  outputs = { ... }@inputs: {
    apps = [
      inputs."5dchess".self
    ];
  };
}
