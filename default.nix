{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
let
  inherit (haskellPackages) cabal blazeHtml digestiveFunctors digestiveFunctorsHeist
  digestiveFunctorsSnap filepath heist highlightingKate lens
  MonadCatchIOTransformers mtl snap snapCore snapLoaderStatic
  snapServer text time transformers xmlhtml
  ;

in cabal.mkDerivation (self: {
  pname = "learn-snap";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    blazeHtml digestiveFunctors digestiveFunctorsHeist
    digestiveFunctorsSnap filepath heist highlightingKate lens
    MonadCatchIOTransformers mtl snap snapCore snapLoaderStatic
    snapServer text time transformers xmlhtml
  ];
  meta = {
    description = "Demoing Heist with Digestive Functors";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
