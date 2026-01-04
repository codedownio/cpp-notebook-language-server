{ patchelf
, pkgsCross

, executable
, executableName
}:

let
  closure = pkgsCross.closureInfo { rootPaths = [executable]; };
in

# This will gather all shared library dependencies of the executable.
# It uses readelf to find the necessary libs, and then searches the Nix closure to find them.
# It copies them all to $out/lib.
# It also rewrites the RPATH to '$ORIGIN', since we intend to bundle these all together.
# We also have to recursively do the same process on the shared libraries themselves to get
# everything we need.
#
# It would have been nice to just use ldd to get all this information, but that is tricky to
# use for cross-compilation. Although possibly it will just work now that we have set
# boot.binfmt.emulatedSystems = [ "aarch64-linux" ]; ?

pkgsCross.runCommand "runner-deps.sh" {} ''
  echo "Looking at: ${executable}/bin/${executableName}"

  # Read the pre-computed closure
  echo "Building library search paths from closure..."
  LIB_PATHS=""
  while IFS= read -r path; do
    if [ -d "$path/lib" ]; then
      LIB_PATHS="$LIB_PATHS:$path/lib"
    fi
  done < ${closure}/store-paths

  mkdir -p $out/lib

  # Keep track of processed libraries to avoid infinite loops
  PROCESSED_LIBS=""

  # Recursive function to resolve dependencies
  resolve_deps() {
    local binary_or_lib="$1"
    echo "Analyzing: $binary_or_lib"

    local needed_libs=$(${pkgsCross.binutils}/bin/readelf -d "$binary_or_lib" | grep NEEDED | sed 's/.*\[\(.*\)\]/\1/')

    for lib in $needed_libs; do
      # Skip if already processed
      if echo "$PROCESSED_LIBS" | grep -q " $lib "; then
        echo "  Already processed: $lib"
        continue
      fi

      echo "Looking for: $lib"
      local found=""
      for libpath in $(echo $LIB_PATHS | tr ':' ' '); do
        if [ -f "$libpath/$lib" ]; then
          echo "  Found: $libpath/$lib"
          found="$libpath/$lib"

          # Only copy if not already copied
          if [ ! -f "$out/lib/$lib" ]; then
            cp "$libpath/$lib" $out/lib/

            # Set the rpath on all of these to /lib, since we're packaging them all together there
            chmod u+w $out/lib/$lib
            ${patchelf}/bin/patchelf --set-rpath '$ORIGIN' $out/lib/$lib
          fi

          # Mark as processed
          PROCESSED_LIBS="$PROCESSED_LIBS $lib "

          # Recurse into this library's dependencies
          resolve_deps "$found"
          break
        fi
      done

      if [[ -z "$found" ]]; then
        echo "  ERROR: didn't find lib: $lib"
        exit 1
      fi
    done
  }

  echo "Resolving libraries recursively:"
  resolve_deps "${executable}/bin/${executableName}"

  echo "All dependencies resolved!"
''
