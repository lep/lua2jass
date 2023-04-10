{
    inputs = {
	nixpkgs.url = "github:NixOS/nixpkgs";
	flake-utils.url = "github:numtide/flake-utils";

        pjass.url = "github:lep/pjass";
        pjass.inputs.nixpkgs.follows = "nixpkgs";
        pjass.inputs.flake-utils.follows = "flake-utils";

        common-j.url = "github:lep/common-j";
        common-j.inputs.nixpkgs.follows = "nixpkgs";
        common-j.inputs.flake-utils.follows = "flake-utils";

	wc3.url = "git+file:/Users/lep/dev/wc3-mapping";
        wc3.inputs.nixpkgs.follows = "nixpkgs";
        wc3.inputs.flake-utils.follows = "flake-utils";
	wc3.inputs.common-j.follows = "common-j";
    };

    outputs = { self, nixpkgs, flake-utils, pjass, common-j, wc3 }:
	flake-utils.lib.eachDefaultSystem (system:
            let pkgs = import nixpkgs { inherit system; };
		packageName = "lua2jass";

                pjass-drv = pjass.defaultPackage.${system};
		#wc3-shell = wc3.devShell.${system};
		wc3-drv = wc3.packages.${system};

                ghcPackages = pkgs.haskellPackages.ghcWithPackages (ps: [
                    ps.language-lua
		    ps.aeson
		    ps.optparse-applicative
		    ps.megaparsec
		    ps.parser-combinators
                    ps.file-embed
                ]);

		pythonPackages = pkgs.python3.withPackages( ps: [
		    ps.tappy
		]);

		lua2jass = pkgs.stdenv.mkDerivation {
		    name = "lua2jass";
		    src = self;
		    buildPhase = ''
			ls runtime
			${ghcPackages}/bin/ghc -O Main.hs -o lua2jass
		    '';

		    installPhase = ''
			mkdir -p $out/bin/
			install -t $out/bin lua2jass
		    '';
		};

            in rec {
		packages = {
		    ${packageName} = lua2jass;
		};

		defaultPackage = packages.${packageName};

		devShell = pkgs.mkShell {
		    env = {
		        commonj = "${common-j}/common.j";
		    };
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
			pkgs.shellcheck
                        pjass-drv
			wc3-drv.jhcr-start
			wc3-drv.jhcr-update
			wc3-drv.wc3
		    ];
		};
            }
        );
}
