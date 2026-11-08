#!/usr/bin/env bash
# Put the pinned actionlint into <dir> (the one install every workflow uses, through .github/actions/install-actionlint):
# its release tarball through fetch-verified.sh (a cache hit is re-verified, a miss downloaded with a real retry
# budget, the checksum always checked), then the binary out of it.
#
# When GitHub's release CDN fails even that (2026-11-08: 504s from 08:54 to past 11:00 UTC, a cold cache on every
# check branch), the same version is built from a source that is not GitHub's CDN: `go install` through
# proxy.golang.org, whose module hashes the Go checksum database verifies, or on a runner with nix (the NixOS
# ci-runner) the nixos-26.05 channel's actionlint (channels.nixos.org and cache.nixos.org; the registry's bare
# `nixpkgs` is github:NixOS/nixpkgs, GitHub again). Either must report exactly ACTIONLINT_VERSION, or the install
# fails: a channel that moved past the pin fails loudly rather than linting with another actionlint. Only the
# verified release tarball is ever cached: the action saves on `source=release` alone.
# Usage: scripts/ci/install-actionlint.sh <dir>   (env ACTIONLINT_VERSION, ACTIONLINT_SHA256; GITHUB_OUTPUT gets source=)
# Env: ACTIONLINT_GO, ACTIONLINT_NIX (the fallbacks' commands, default go and nix; the test names absent ones).
# Tested by scripts/ci/install-actionlint-test.sh.
set -euo pipefail

dir="${1:?usage: install-actionlint.sh <dir>}"
: "${ACTIONLINT_VERSION:?}" "${ACTIONLINT_SHA256:?}"
archive="actionlint_${ACTIONLINT_VERSION}_linux_amd64.tar.gz"
source_used() { [ -n "${GITHUB_OUTPUT:-}" ] && echo "source=$1" >> "$GITHUB_OUTPUT"; return 0; }

if "$(dirname "${BASH_SOURCE[0]}")/fetch-verified.sh" "$dir/$archive" \
  "https://github.com/rhysd/actionlint/releases/download/v$ACTIONLINT_VERSION/$archive" "$ACTIONLINT_SHA256"; then
  tar -xzf "$dir/$archive" -C "$dir" actionlint
  source_used release
  exit 0
fi

echo "actionlint $ACTIONLINT_VERSION: GitHub's release download failed -- building it from another source" >&2
rm -f "$dir/actionlint"
go="${ACTIONLINT_GO:-go}" nix="${ACTIONLINT_NIX:-nix}"
if command -v "$go" > /dev/null; then
  if GOBIN="$dir" GOPROXY=https://proxy.golang.org GOSUMDB=sum.golang.org GONOSUMDB='' GOFLAGS='' \
    "$go" install "github.com/rhysd/actionlint/cmd/actionlint@v$ACTIONLINT_VERSION"; then
    source_used go
  fi
fi
if [ ! -x "$dir/actionlint" ] && command -v "$nix" > /dev/null; then
  if built=$("$nix" --extra-experimental-features 'nix-command flakes' build --no-link --print-out-paths \
    'nixpkgs/nixos-26.05#actionlint'); then
    cp "$built/bin/actionlint" "$dir/actionlint"
    source_used nix
  fi
fi
if [ ! -x "$dir/actionlint" ]; then
  echo "::error::actionlint $ACTIONLINT_VERSION: no release download, and neither go nor nix could build it" >&2
  exit 1
fi
reported=$("$dir/actionlint" -version | head -1)
if [ "$reported" != "$ACTIONLINT_VERSION" ]; then
  rm -f "$dir/actionlint"
  echo "::error::actionlint: the fallback built '$reported', not the pinned $ACTIONLINT_VERSION" >&2
  exit 1
fi
echo "actionlint $ACTIONLINT_VERSION: built from the fallback source, version verified"
