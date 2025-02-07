{
  description = "wire-bloodhound-tests dev environment flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat.url = "https://flakehub.com/f/edolstra/flake-compat/1.tar.gz";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };

        # Haskell packages helper functions
        disableTests = pkgs.haskell.lib.dontCheck;
        enableTests = pkgs.haskell.lib.doCheck;
        unbreak = pkgs.haskell.lib.markUnbroken;
        jailBreak = pkgs.haskell.lib.doJailbreak;

        haskellPackagesOverlay = {
          overrides = self: super: {
            bloodhound = disableTests (
              self.callCabal2nix "bloodhound" (pkgs.fetchFromGitHub {
                owner = "wireapp";
                repo = "bloodhound";
                rev = "f34f19fefa26fca3c96e310d8c0c697ff6ec97e5";
                sha256 = "sha256-zSBC55f5rQKAxuSVaRZcLuL4SVEcfWFsiVvZL1dGL6U=";
              }) { }
            );

            # It's a lie that this packes in not broken for GHC 9.6. However,
            # we need it for the Nix env to evaluate, but we take care to not
            # build it by disabling tests for wire-bloodhound-tests.
            quickcheck-arbitrary-template = unbreak super.quickcheck-arbitrary-template;

            wire-bloodhound-tests = super.developPackage {
              root = ./.;
            };
          };
        };

        haskellPackages = pkgs.haskellPackages.override haskellPackagesOverlay;

        formatters = [
          (pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.ormolu)
          (pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.cabal-fmt)
          pkgs.nixpkgs-fmt
          pkgs.treefmt
          pkgs.shellcheck
        ];

        treefmt-command = pkgs.writeShellApplication {
          name = "nix-fmt-treefmt";
          text = ''
            exec ${pkgs.treefmt}/bin/treefmt --config-file ./treefmt.toml "$@"
          '';
          runtimeInputs = formatters;
        };

        statix-command = pkgs.writeShellApplication {
          name = "statix-check";
          runtimeInputs = [ pkgs.statix ];
          text = ''
            statix check ${toString ./.} || exit 1
            echo "Statix check passed!"
          '';
        };
        ghcVersions = [
          "ghc92"
          "ghc94"
          "ghc96"
          "ghc98"
          "ghc910"
        ];

        haskellPackagesFor =
          ghc_version: (pkgs.haskell.packages.${ghc_version}).override haskellPackagesOverlay;
        additionalDevShells = builtins.listToAttrs (
          map (ghc_version: {
            name = ghc_version;
            value = (haskellPackagesFor ghc_version).shellFor {
              packages = p: [ p.wire-bloodhound-tests ];
              withHoogle = true;
              buildInputs = [
                pkgs.ghcid
                pkgs.statix
                statix-command
                # We need to be careful to not rebuild HLS, because that would be expensive.
                (pkgs.haskell-language-server.override {
                  supportedGhcVersions = [ (pkgs.lib.removePrefix "ghc" ghc_version) ];
                })
              ] ++ formatters;
            };
          }) ghcVersions
        );
      in
      rec {
        packages.default = haskellPackages.wire-bloodhound-tests;

        # Development shell with required dependencies
        devShells = {
          default = additionalDevShells.ghc96;
        } // additionalDevShells;

        formatter = treefmt-command;

        checks = {
          statix =
            pkgs.runCommand "statix-check"
              {
                buildInputs = [ statix-command ];
              }
              ''
                statix-check > $out
              '';
        };
      }
    );
}
