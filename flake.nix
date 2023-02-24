{
    inputs = {
	nixpkgs.url = "github:NixOS/nixpkgs";
	flake-utils.url = "github:numtide/flake-utils";
    };

    outputs = { self, nixpkgs, flake-utils }:
	flake-utils.lib.eachDefaultSystem (system:
            let pkgs = import nixpkgs { inherit system; };

                ghcPackages = pkgs.haskellPackages.ghcWithPackages (ps: [
                    ps.language-lua
		    ps.aeson
                ]);

		pythonPackages = pkgs.python3.withPackages( ps: [
		    ps.tappy
		]);

            in {
		devShell = pkgs.mkShell {
		    shellHook = ''
			function lli {
			    runhaskell compile.hs $1 | python interpreter.py /dev/stdin
			}
		    '';
		    buildInputs = [
			pythonPackages
                        ghcPackages
			pkgs.lua5_3_compat
			pkgs.jq
		    ];
		};
            }
        );
}
