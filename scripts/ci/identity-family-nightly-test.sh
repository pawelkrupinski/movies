#!/usr/bin/env bash
# scripts/ci/identity-family-nightly.sh end to end against a stub `sbt` and `gh`: a night with no stored cache walks every
# family and stores nothing without a store; with a store it stores the cache and its commit; a stored cache of the same
# commit walks with a reach file; a walk that wrote no verdict fails (exit 2) and stores nothing.
# Run: bash scripts/ci/identity-family-nightly-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m identity-family-nightly.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin"
# sbt: logs its last argument and, unless $NO_VERDICT, writes the walk's two tables and the verdict line.
cat > "$work/bin/sbt" <<'STUB'
#!/usr/bin/env bash
echo "${@: -1}" >> "$WORK/sbt-calls"
[ -z "${NO_VERDICT:-}" ] || exit 1
out=$(sed -E 's/.* --out ([^ ]+).*/\1/' <<< "${@: -1}")
printf 'kind\tcountry\n' > "$out/family-flips-de.tsv"; printf 'family\tseconds\n' > "$out/family-walks-de.tsv"; echo '[]' > "$(sed -E 's/.* --cache ([^ ]+).*/\1/' <<< "${@: -1}")"
echo "## de family check: 3 families, shard 1/2, 3 walked, 0 reused, 0 flip(s)"
STUB
# gh: logs every call; `release view --json` answers $ASSETS; `release download` writes a gzip of one line.
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
echo "$*" >> "$WORK/calls"
case "$1 $2" in
  "release view") if [[ "$*" == *--json* ]]; then printf '%s\n' $ASSETS; else [ -n "${RELEASE_EXISTS:-}" ] || [ -f "$WORK/exists" ]; fi ;;
  "release create") touch "$WORK/exists" ;;
  "release download")
    for ((i = 1; i <= $#; i++)); do [ "${!i}" = -O ] && { j=$((i + 1)); dest="${!j}"; }; done
    case "${@: -1}" in *) printf '%s\n' "${CACHED_SHA:-x}" | gzip -c > "$dest" ;; esac ;;
  *) exit 0 ;;
esac
STUB
# git and python3: the cached commit is never fetched, and the reach (scripts/ci/family_reach.py, tested on its own) is a file.
printf '#!/usr/bin/env bash\nexit 0\n' > "$work/bin/git"
cat > "$work/bin/python3" <<'STUB'
#!/usr/bin/env bash
out=$(sed -E 's/.* --out ([^ ]+).*/\1/' <<< "$*")
echo '*' > "$out"; echo "reach: ALL (stub)"
STUB
chmod +x "$work/bin/sbt" "$work/bin/gh" "$work/bin/git" "$work/bin/python3"
sha=0123456789abcdef0123456789abcdef01234567
export PATH="$work/bin:$PATH" WORK="$work" RUNNER_TEMP="$work" GH_RELEASE_RETRY_DELAY=0 GITHUB_SHA="$sha"
night() { # <extra env assignments...> -> exit code; sbt's line in $WORK/sbt-calls, gh's in $WORK/calls
  rm -rf "$work/out" "$work/sbt-calls" "$work/calls" "$work/exists"; : > "$work/sbt-calls"; : > "$work/calls"
  env "$@" bash "$REPO_ROOT/scripts/ci/identity-family-nightly.sh" de "$work/out" > "$work/log" 2>&1
  echo $?
}

check "no store: every family walks, the shard and levels reach the walk, nothing is stored" "0 1 1 0 0" \
  "$(night FAMILY_SHARD=1/2) $(grep -c -- '--shard 1/2 --levels 1 ' "$work/sbt-calls") $(grep -c -v -- '--reach' "$work/sbt-calls") $(grep -c -- '--reach' "$work/sbt-calls") $(grep -c '^release' "$work/calls")"
check "...and the flips and walks tables are left for the artifact, the cache not" "1 1 0" \
  "$(test -f "$work/out/family-flips-de.tsv" && echo 1) $(test -f "$work/out/family-walks-de.tsv" && echo 1) $(test -f "$work/out/cache.json" && echo 1 || echo 0)"
check "a store with no cache yet: the release is created, the cache and its commit are stored" "0 1 2" \
  "$(night FAMILY_STORE=s FAMILY_SHARD=1/2 ASSETS=) $(grep -c '^release create s ' "$work/calls") $(grep -c '^release upload s .*family-cache-de-1of2-L1' "$work/calls")"
check "a stored cache with its commit walks with the reach file" "0 1" \
  "$(night FAMILY_STORE=s FAMILY_SHARD=1/2 RELEASE_EXISTS=1 CACHED_SHA="$sha" ASSETS="family-cache-de-1of2-L1.json.gz family-cache-de-1of2-L1.sha.gz") $(grep -c -- '--reach ' "$work/sbt-calls")"
check "a walk that wrote no verdict fails, said, and stores nothing" "2 0 1" \
  "$(night FAMILY_STORE=s FAMILY_SHARD=1/2 NO_VERDICT=1) $(grep -c '^release upload' "$work/calls") $(grep -c '^family explorer (de, shard 1/2): no verdict' "$work/log")"
check "a shard that is not i/N fails before the walk" "2 0" "$(night FAMILY_SHARD=3) $(wc -l < "$work/sbt-calls" | tr -d ' ')"

spec_summary
