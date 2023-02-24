{
    inputs = {
	nixpkgs.url = "github:NixOS/nixpkgs";
	flake-utils.url = "github:numtide/flake-utils";

        pjass.url = "github:lep/pjass";
        pjass.inputs.nixpkgs.follows = "nixpkgs";
        pjass.inputs.flake-utils.follows = "flake-utils";
    };

    outputs = { self, nixpkgs, flake-utils, pjass }:
	flake-utils.lib.eachDefaultSystem (system:
            let pkgs = import nixpkgs { inherit system; };
                pjass-drv = pjass.defaultPackage.${system};

                ghcPackages = pkgs.haskellPackages.ghcWithPackages (ps: [
                    ps.language-lua
		    ps.aeson
		    ps.optparse-applicative
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
                        pjass-drv
		    ];
		};
            }
        );
}
