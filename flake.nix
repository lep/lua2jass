{
    inputs = {
	nixpkgs.url = "github:NixOS/nixpkgs";
	flake-utils.url = "github:numtide/flake-utils";
    };

    outputs = { self, nixpkgs, flake-utils }:
	flake-utils.lib.eachSystem (flake-utils.lib.defaultSystems ++ [flake-utils.lib.system.aarch64-darwin]) (system:
            let pkgs = import nixpkgs { inherit system; };

                ghcPackages = pkgs.haskellPackages.ghcWithPackages (ps: [
                    ps.data-interval
                    ps.PSQueue
                    ps.aeson
                ]);

		pythonPackages = pkgs.python3.withPackages( ps: [
		]);

            in {
		devShell = pkgs.mkShell {
		    buildInputs = [
			pythonPackages
			pkgs.lua
                        #ghcPackages
		    ];
		};
            });
}



