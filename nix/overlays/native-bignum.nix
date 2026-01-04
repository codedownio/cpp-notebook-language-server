{ compiler-nix-name }:

final: prev: {
  haskell-nix = let
    shouldPatch = name: compiler: prev.lib.hasPrefix compiler-nix-name name;

    overrideCompiler = name: compiler: (compiler.override {
      enableNativeBignum = true;
    });
  in
    prev.lib.recursiveUpdate prev.haskell-nix {
      compiler = prev.lib.mapAttrs overrideCompiler (prev.lib.filterAttrs shouldPatch prev.haskell-nix.compiler);
    };
}
