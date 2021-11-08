let
  sources = import nix/sources.nix {};
  haskell-nix = (import sources."haskell.nix" {});
  nixpkgs = haskell-nix.pkgs;
  gitignore = (import sources."gitignore.nix" {
    inherit (nixpkgs) lib;
  }).gitignoreSource;

  src = nixpkgs.lib.cleanSourceWith {
    name = "matplotlib";
    src = gitignore ./.;
  };
in
nixpkgs.haskell-nix.stackProject {
  inherit src;
  modules = [({pkgs, ...}: {
    packages.matplotlib.components.library.build-tools =
      [ pkgs.buildPackages.python39Packages.matplotlib
        pkgs.buildPackages.python39Packages.scipy
        pkgs.buildPackages.texlive.combined.scheme-small ];
    packages.matplotlib.components.tests.matplotlib-test.build-tools =
      [ pkgs.buildPackages.python39Packages.matplotlib
        pkgs.buildPackages.python39Packages.scipy
        pkgs.buildPackages.texlive.combined.scheme-small ];
    doHaddock = false;
  })];
}
