{ compiler-nix-name
, gitignore
, system
}:

final: prev: {
  hixProject = compiler-nix-name:
    final.haskell-nix.hix.project {
      src = gitignore.lib.gitignoreSource ../../.;
      evalSystem = system;
      inherit compiler-nix-name;

      modules = [{
        packages.unix.components.library.configureFlags = [''-f os-string''];
        packages.directory.components.library.configureFlags = [''-f os-string''];

        packages.cpp-notebook-language-server.components.exes.cpp-notebook-language-server.dontStrip = false;
      } (
        prev.lib.optionalAttrs prev.stdenv.isDarwin (import ../macos-modules.nix { pkgs = prev; })
      )];
    };
}
