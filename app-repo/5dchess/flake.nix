{
	inputs.nixpkgs.url = github:nixos/nixpkgs;
	outputs = { self, nixpkgs }: {
		self = builtins.toPath self;
		defaultApp = nixpkgs.lib.mapAttrs (system: pkgs: {
			type = "app";
			program = "${pkgs.writers.writeBash "5dchess" ''
				${pkgs.wine64}/bin/wine64 ${self}/5dchesswithmultiversetimetravel.exe
			''}";
		}) nixpkgs.legacyPackages;
	};
}
