{
  description = "Additional utilities for working with Tasty";

  inputs = {
    nixpkgs.url       = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url   = "github:sixears/flake-build-utils/r1.0.0.6";

    exited.url        = "github:sixears/exited/r1.0.4.10";
    more-unicode.url  = "github:sixears/more-unicode/r0.0.17.6";
  };

  outputs = { self, nixpkgs, build-utils
            , exited, more-unicode }:
    build-utils.lib.hOutputs self nixpkgs "tasty-plus" {
      deps = {
        inherit exited more-unicode;
      };
      ghc = p: p.ghc8107; # for tfmt
    };
}
