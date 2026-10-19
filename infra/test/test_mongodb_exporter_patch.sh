#!/usr/bin/env bash
# WHAT MONGO-1'S mongodb_exporter EMITS FOR MongoDB 8.3's opLatencies HISTOGRAM, asked of the
# exporter source mongo-1 actually builds: nixpkgs' pinned src with the module's `patches` applied.
#
# 0.51.0 skips serverStatus' `histograms` (PMM-14337) but not the singular `histogram` arrays 8.3
# added under opLatencies, so every bucket flattened to one name and label set: 64 "was collected
# before with the same name and label values" errors a scrape, ~187k journal lines a day, and a
# served value of 0. oplatencies_histogram_test.go is dropped into the patched tree and run with the
# fleet's own Go and vendored modules, so a patch that stops applying, or stops skipping, fails here.
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
infra="$(cd "$here/.." && pwd)"
nix_flags=(--extra-experimental-features 'nix-command flakes')
attr="path:$infra#nixosConfigurations.mongo-1.config.fleet.mongodbExporter.package"

if ! command -v nix >/dev/null 2>&1; then
  echo "  FAILED nix is not on PATH, so the exporter patch was not checked."
  exit 1
fi

# Go FOR THIS MACHINE, not mongo-1's x86_64-linux one, from the same pinned nixpkgs (`--inputs-from`).
paths="$(timeout "${EXPORTER_FETCH_TIMEOUT:-600}" nix "${nix_flags[@]}" build --no-link --print-out-paths \
  --inputs-from "path:$infra" "$attr.src" "$attr.goModules" nixpkgs#go)" || {
  echo "  FAILED could not fetch the exporter's source, modules or Go"; exit 1; }
src="$(sed -n 1p <<<"$paths")"; modules="$(sed -n 2p <<<"$paths")"; go="$(sed -n 3p <<<"$paths")/bin/go"
patches="$(nix "${nix_flags[@]}" eval --raw "$attr" --apply 'p: builtins.concatStringsSep "\n" (map (x: "${x}") (p.patches or [ ]))')" || {
  echo "  FAILED could not read the exporter's patches"; exit 1; }

work="$(mktemp -d)"
trap 'chmod -R u+w "$work"; rm -rf "$work"' EXIT
cp -R "$src" "$work/src" && chmod -R u+w "$work/src"
while IFS= read -r patch; do
  [ -n "$patch" ] || continue
  if ! patch -s -p1 -d "$work/src" < "$patch"; then
    echo "  FAILED $(basename "$patch") no longer applies to $(basename "$src")"; exit 1
  fi
done <<<"$patches"
cp -R "$modules" "$work/src/vendor" && chmod -R u+w "$work/src/vendor"
cp "$here/mongodb-exporter/oplatencies_histogram_test.go" "$work/src/exporter/"

if out="$(cd "$work/src" && GOFLAGS=-mod=vendor GOCACHE="$work/cache" GOPATH="$work/gopath" GOTOOLCHAIN=local \
    "$go" test -count=1 -run TestOpLatenciesHistogramIsSkippedLikeHistograms ./exporter/ 2>&1)"; then
  echo "  ok   the exporter skips MongoDB 8.3's opLatencies histogram arrays"
else
  echo "  FAILED the exporter still emits MongoDB 8.3's opLatencies histogram:"
  sed 's/^/    /' <<<"$out" | tail -20
  exit 1
fi
