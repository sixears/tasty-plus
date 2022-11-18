{
  description = "Additional utilities for working with Tasty";

  inputs = {
    nixpkgs.url       = github:nixos/nixpkgs/be44bf67; # nixos-22.05 2022-10-15
    build-utils.url   = github:sixears/flake-build-utils/r1.0.0.11;

    exited.url        = github:sixears/exited/r1.0.4.16;
    more-unicode.url  = github:sixears/more-unicode/r0.0.17.8;
  };

  outputs = { self, nixpkgs, build-utils
            , exited, more-unicode }:
    build-utils.lib.hOutputs self nixpkgs "tasty-plus" {
#      deps = { inherit exited more-unicode; };
      ghc = p: p.ghc8107; # for tfmt
      callPackage = { mkDerivation, lib, system
                    , base, base-unicode-symbols, data-textual, deepseq
                    , directory, mtl, optparse-applicative, safe, tasty
                    , tasty-hunit, tasty-quickcheck, temporary, text
                    , text-printer
                    }:
        let
          pkg = build-utils.lib.flake-def-pkg system;
        in
          mkDerivation {
            pname = "tasty-plus";
            version = "1.5.2.16";
            src = ./.;
            libraryHaskellDepends = [
              base base-unicode-symbols data-textual deepseq directory mtl
              optparse-applicative safe tasty tasty-hunit tasty-quickcheck
              temporary text text-printer
            ] ++ map (p: pkg p) [ exited more-unicode ];
            testHaskellDepends = [ base optparse-applicative ];
            description = "Additional utilities for working with Tasty";
            license = lib.licenses.mit;
          };
    };
}
