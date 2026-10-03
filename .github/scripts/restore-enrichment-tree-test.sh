#!/usr/bin/env bash
# restore-enrichment-tree.sh against a stub `gh` whose release holds, or lacks, the tree.
# Run: bash .github/scripts/restore-enrichment-tree-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m restore-enrichment-tree.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/src/test/resources/fixtures/enrichment-uk/api.themoviedb.org"
printf 'recorded\n' > "$work/src/test/resources/fixtures/enrichment-uk/api.themoviedb.org/search"
( cd "$work/src" && tar -cf - test | zstd -q -c > "$work/enrichment-uk.tar.zst" )
printf 'not an archive' > "$work/enrichment-uk-broken.tar.zst"

# `release download` hands over $STUB_ASSET when set, as a release holding it would.
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
if [ "$1 $2" = "release download" ]; then
  [ -n "${STUB_ASSET:-}" ] || exit 1
  while [ "$#" -gt 0 ]; do [ "$1" = "--dir" ] && dir="$2"; shift; done
  cp "$STUB_ASSET" "$dir/"
  exit 0
fi
exit 1
STUB
chmod +x "$work/bin/gh"

# restore <label> <mode> [asset] -> exit status; the stage is $work/stage-<label>, the output $work/out-<label>
restore() {
  STUB_ASSET="${3:-}" PATH="$work/bin:$PATH" FIXTURE_RELEASE_TAG=convergence-fixtures \
    bash "$REPO_ROOT/.github/scripts/restore-enrichment-tree.sh" uk "$2" "$work/stage-$1" > "$work/out-$1" 2>&1
  echo $?
}

check "a release holding the tree stages it" "0" "$(restore pinned hermetic "$work/enrichment-uk.tar.zst")"
check "...unpacked under its own paths, outside the workspace" "recorded" \
  "$(cat "$work/stage-pinned/test/resources/fixtures/enrichment-uk/api.themoviedb.org/search" 2>/dev/null)"
check "a replay leg whose pinned tree is gone fails, distinctly" "3" "$(restore unpinned hermetic)"
check "...and says why" "true" "$(grep -q 'a hermetic leg replays nothing else' "$work/out-unpinned" && echo true || echo false)"
check "a recording with no tree anywhere goes on to fetch live" "0" "$(restore cold record)"
check "...having staged nothing" "0" "$(find "$work/stage-cold" -type f | wc -l | tr -d ' ')"
status="$(restore broken record "$work/enrichment-uk-broken.tar.zst")"
check "an archive that will not unpack fails, and not as a missing pin" "true" \
  "$([ "$status" -ne 0 ] && [ "$status" -ne 3 ] && echo true || echo false)"

spec_summary
