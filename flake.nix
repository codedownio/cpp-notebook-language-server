{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix/9c5956641f45b6b02607e318485aad01c18e65b0";
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-25.11";

  outputs = { self, flake-utils, gitignore, haskellNix, nixpkgs }:
    flake-utils.lib.eachSystem ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"] (system:
      let
        compiler-nix-name = "ghc9122";

        overlays = [
          haskellNix.overlay

          # Set enableNativeBignum flag on compiler
          (import ./nix/overlays/native-bignum.nix { inherit compiler-nix-name; })

          # Configure hixProject
          (import ./nix/overlays/hix-project.nix { inherit compiler-nix-name gitignore system; })
        ];

        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };

        flake = (pkgs.hixProject compiler-nix-name).flake {};
        flakeStatic = (pkgs.pkgsCross.musl64.hixProject compiler-nix-name).flake {};
        flakeDarwin = (pkgs.pkgsCross.aarch64-darwin.hixProject compiler-nix-name).flake {};
        flakeAarch64Linux = (pkgs.pkgsCross.aarch64-multiplatform.hixProject compiler-nix-name).flake {};
        flakeStaticAarch64Linux = (pkgs.pkgsCross.aarch64-multiplatform-musl.hixProject compiler-nix-name).flake {};

        packageForGitHub = packageForGitHub' system;

        packageForGitHub' = systemToUse: cnls: pkgs.runCommand "cpp-notebook-language-server-${cnls.version}-${systemToUse}" {} ''
          name="cpp-notebook-language-server-${cnls.version}-${systemToUse}"

          mkdir -p to_zip
          cp -r ${cnls}/* to_zip
          mkdir -p $out
          tar -czvf $out/$name.tar.gz -C to_zip .
        '';

        # Build our minimal parser (unwrapped)
        cling-parser-unwrapped = pkgs.clangStdenv.mkDerivation {
          pname = "minimal-cling-parser";
          version = "0.1.0";

          src = ./cling-parser;

          nativeBuildInputs = [ pkgs.cmake ];
          buildInputs = with pkgs; [
            cling.unwrapped
            llvmPackages_18.llvm
          ];

          cmakeBuildType = "Release";

          meta = with pkgs.lib; {
            description = "Minimal C++ parser using Cling interpreter";
            license = licenses.bsd3;
            platforms = platforms.unix;
            mainProgram = "cling-parser";
          };
        };

        # Wrapped version with proper cling flags
        cling-parser = cling-parser-unwrapped.overrideAttrs (oldAttrs: {
          nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [ pkgs.makeWrapper ];

          # cling-parser needs a collection of flags to start up properly, so wrap it by default.
          # We'll provide the unwrapped version as a passthru
          flags = pkgs.cling.flags ++ [
            "-resource-dir"
            "${pkgs.cling.unwrapped}"
            "-L"
            "${pkgs.cling.unwrapped}/lib"
            "-l"
            "${pkgs.cling.unwrapped}/lib/cling.so"
          ];

          fixupPhase = ''
            runHook preFixup

            wrapProgram $out/bin/cling-parser \
              --argv0 $out/bin/.cling-parser-wrapped \
              --add-flags "$flags"

            runHook postFixup
          '';

          passthru = (oldAttrs.passthru or { }) // {
            unwrapped = cling-parser-unwrapped;
          };
        });

        mkWrapped = cnls: pkgs.runCommand "cpp-notebook-language-server-${cnls.version}-wrapped" {
          nativeBuildInputs = [ pkgs.makeWrapper ];
        } ''
          mkdir -p $out/bin
          makeWrapper ${cnls}/bin/cpp-notebook-language-server $out/bin/cpp-notebook-language-server \
            --prefix PATH : ${cling-parser}/bin
        '';

      in
        {
          devShells = {
            default = pkgs.mkShell {
              NIX_PATH = "nixpkgs=${pkgs.path}";
              buildInputs = with pkgs; [
                haskell.compiler.ghc9122

                pcre
                zlib

                cling-parser
              ];
            };
          };

          packages = (rec {
            inherit (pkgs) cabal2nix stack;

            default = static;

            inherit cling-parser;

            dynamicWrapped = mkWrapped dynamic;
            staticWrapped = mkWrapped static;

            static = flakeStatic.packages."cpp-notebook-language-server:exe:cpp-notebook-language-server";
            dynamic = flake.packages."cpp-notebook-language-server:exe:cpp-notebook-language-server";
            darwin = flakeDarwin.packages."cpp-notebook-language-server:exe:cpp-notebook-language-server";
            aarch64Linux = let
              executable = flakeAarch64Linux.packages."cpp-notebook-language-server:exe:cpp-notebook-language-server";
              libs = pkgs.callPackage ./nix/dynamic-aarch64-closure.nix {
                inherit executable;
                executableName = "cpp-notebook-language-server";
                pkgsCross = pkgs.pkgsCross.aarch64-multiplatform;
              };
            in pkgs.runCommand "cpp-notebook-language-server-aarch64-dynamic" { passthru = { inherit (executable) version; }; } ''
                 mkdir -p $out/bin
                 cp -r ${executable}/bin/* $out/bin
                 cp -r ${libs}/lib/* $out/bin
               '';
            staticAarch64Linux = flakeStaticAarch64Linux.packages."cpp-notebook-language-server:exe:cpp-notebook-language-server";

            grandCombinedGithubArtifacts = pkgs.symlinkJoin {
              name = "cpp-notebook-language-server-grand-combined-artifacts";
              paths = [
                (packageForGitHub' "x86_64-linux" self.packages.x86_64-linux.static)
                (packageForGitHub' "aarch64-linux" aarch64Linux)
                (packageForGitHub' "x86_64-darwin" self.packages.x86_64-darwin.dynamic)
                (packageForGitHub' "aarch64-darwin" self.packages.aarch64-darwin.dynamic)
              ];
            };

            # Print a trivial PATH that we can use to run kernel and LSP tests, to ensure
            # they aren't depending on anything on the test machine's PATH.
            print-basic-path = pkgs.writeShellScriptBin "basic-path.sh" ''
              echo ${pkgs.lib.makeBinPath (with pkgs; [coreutils bash])}
            '';

            # No GMP (we test the dynamic builds to make sure GMP doesn't end up in the static builds)
            verify-no-gmp = pkgs.writeShellScriptBin "verify-no-gmp.sh" ''
              echo "Checking for libgmp in ${dynamic}/bin/cpp-notebook-language-server"
              (echo "$(ldd ${dynamic}/bin/cpp-notebook-language-server)" | grep libgmp) && exit 1

              exit 0
            '';

            nixpkgsPath = pkgs.writeShellScriptBin "nixpkgsPath.sh" "echo -n ${pkgs.path}";
          });

          inherit flake;
        }
    );

  # nixConfig = {
  #   # This sets the flake to use the IOG nix cache.
  #   # Nix should ask for permission before using it,
  #   # but remove it here if you do not want it to.
  #   extra-substituters = ["https://cache.iog.io"];
  #   extra-tcpped-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
  #   allow-import-from-derivation = "true";
  # };
}
