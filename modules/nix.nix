{inputs, ...}: {
  flake.nixosModules.nix = {config, ...}: {
    imports = [
      inputs.nix-index-database.nixosModules.nix-index
      { programs.nix-index-database.comma.enable = true; }
    ];
    nix = {
      nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
      settings = {
        trusted-users = ["@wheel"];
        experimental-features = [ "nix-command" "flakes" "pipe-operators"];
      };
      settings.secret-key-files = "/etc/nix/private-key";
    };
    nix.settings.extra-platforms = config.boot.binfmt.emulatedSystems;
    nix.settings.trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "cache.nixos-cuda.org:74DUi4Ye579gUqzH4ziL9IyiJBlDpMRn9MBN8oNan9M="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
    nix.settings.substituters = [
      "https://cache.nixos-cuda.org"
      "https://cache.iog.io"
      "https://nix-community.cachix.org"
      "https://cache.nixos.org/"
    ];
    nix.distributedBuilds = true;
  	nix.extraOptions = ''
	    builders-use-substitutes = true
	  '';
    nix.registry.blackmarket.flake = inputs.blackmarket;
  };
}
