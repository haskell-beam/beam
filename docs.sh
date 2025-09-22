#!/bin/sh
set -e

dir=$(mktemp -d dist-docs.XXXXXX)

# assumes cabal 2.4 or later
cabal v2-haddock beam-postgres --builddir="$dir" --haddock-for-hackage --enable-doc