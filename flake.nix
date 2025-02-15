{
  description = "build & dev environment for beginning Haskellers";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs =
    inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {

      systems = [
        "aarch64-darwin"
        "x86_64-linux"
      ];

      perSystem =
        { pkgs, system, ... }:
        let
          haskellPackages = pkgs.haskellPackages;
        in
        {
          devShells.default = pkgs.mkShell {
            buildInputs = [
              pkgs.zlib
              haskellPackages.cabal-install
              haskellPackages.ghc

              haskellPackages.fourmolu
              haskellPackages.ghcid
              haskellPackages.haskell-language-server
            ];
            packages = with pkgs; [
              (python3.withPackages (
                p: with p; [
                  sphinx
                  furo
                ]
              ))
            ];
          };

          formatter = pkgs.nixfmt-rfc-style;
        };
    };
}
