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
    ];
    nix.settings.substituters = [
      "https://cache.iog.io"
    ];
    nix.distributedBuilds = true;
  	nix.extraOptions = ''
	    builders-use-substitutes = true
	  '';
    nix.registry.blackmarket.flake = inputs.blackmarket;
  };
}
