(import ./default.nix).shellFor {
  tools = {
    cabal = "latest";
    hpack = "latest";
    hlint = "latest";
    ormolu = "latest";
    haskell-language-server = "latest";
  };

  # Prevents cabal from choosing alternate plans, so that
  # *all* dependencies are provided by Nix.
  exactDeps = true;
}
