/*

to run the benchmark using nix
  - build with nix
    - run `nix-build` to build the executable (do not run this inside nix-shell!)
    - run `nix-shell` to drop into an environment with z3 and build-time dependencies
    - run `./result/bin/liquid-benchmark`
  - build with cabal
    - run `nix-shell` to drop into an environment with z3 and build-time dependencies
    - run `cabal v2-run` (this will build only the current package, since nix put all the deps into the environment)

warning:
  - bitrot is possible
  - this nix-expression doesn't explicitly version all of the liquid-haskell dependencies, relying instead on the git-submodules in the current directory
  - if the git submodules are upgrade to more recent versions that do not work with packages in the pinned version of nixpkgs, or the overrides specified below, then this will no longer build

*/
{ config ? { allowBroken = true; }, ... }:
let
  nixpkgs = import (
    builtins.fetchTarball {
      # fetch latest nixpkgs https://github.com/NixOS/nixpkgs-channels/tree/nixos-20.03 as of Tue 28 Jul 2020 08:19:11 PM UTC
      url = "https://github.com/NixOS/nixpkgs-channels/archive/eeb91b03a5cef25c3931bdd4438f006a293adef9.tar.gz";
      sha256 = "00cqfmfry5rjhz4kyybr4jc4vzslkk3csy28w9k1qlyn8arpqv3s";
    }
  ) { inherit config; };
  # function to make sure a haskell package has z3 at build-time and test-time
  usingZ3 = pkg: nixpkgs.haskell.lib.overrideCabal pkg (old: { buildTools = old.buildTools or [] ++ [ nixpkgs.z3 ]; });
  # override haskell compiler version, add and override dependencies in nixpkgs
  haskellPackages = nixpkgs.haskell.packages."ghc8101".override (
    old: {
      all-cabal-hashes = nixpkgs.fetchurl {
        # fetch latest cabal hashes https://github.com/commercialhaskell/all-cabal-hashes/tree/hackage as of Tue 28 Jul 2020 08:19:11 PM UTC
        url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/e965da883deac5dc1aec0291df2aa6b78b8fd6a8.tar.gz";
        sha256 = "17wkxh8a2zps3ip56kvgvwq0082g2mhasp0x2w7fkn35yij7j4kz";
      };
      overrides = self: super: with nixpkgs.haskell.lib; rec {
        #mkDerivation = args: super.mkDerivation (args // {
        #  jailbreak = true;
        #  haddock = false;
        #  doCheck = false;
        #});
        # build liquidhaskell from the submodules

        liquid-base = self.callCabal2nix "liquid-base" ./liquid-base/liquid-base {};
        liquid-fixpoint = self.callCabal2nix "liquid-fixpoint" ./liquidhaskell/liquid-fixpoint {};
        liquidhaskell = dontHaddock (dontCheck (self.callCabal2nix "liquidhaskell" ./liquidhaskell {}));

        # some dependencies of liquidhaskell had problems with version ranges or tests

        optics = doJailbreak (dontCheck (self.callHackage "optics" "0.3" {}));
        optics-core = doJailbreak (dontCheck (self.callHackage "optics-core" "0.3" {}));
        optics-extra = doJailbreak (dontCheck (self.callHackage "optics-extra" "0.3" {}));
        optics-th = doJailbreak (dontCheck (self.callHackage "optics-th" "0.3" {}));

        doctest = doJailbreak (dontCheck (self.callHackage "doctest" "0.16.3" {}));
        ChasingBottoms = doJailbreak (dontCheck super.ChasingBottoms);
        unoredered-containers = doJailbreak (dontCheck super.unoredered-containers);
        Diff = doJailbreak (dontCheck super.Diff);
        text-format = doJailbreak (dontCheck super.text-format);

        tasty-rerun = self.callHackage "hello" "1.0" {}; # hack to disable tests for a bunch of packages

        hashable = self.callHackage "hashable" "1.3.0.0" {}; # oof
      };
    }
  );
  # function to bring devtools in to a package environment
  devtools = old: { nativeBuildInputs = old.nativeBuildInputs ++ [ nixpkgs.cabal-install nixpkgs.ghcid ]; }; # ghc and hpack are automatically included
  # ignore files specified by gitignore in nix-build
  source = nixpkgs.nix-gitignore.gitignoreSource [] ./.;
  # use overridden-haskellPackages to call gitignored-source
  drv = usingZ3 (haskellPackages.callCabal2nix "liquidhaskell-benchmark" source {});
in
if nixpkgs.lib.inNixShell then drv.env.overrideAttrs devtools else drv
