#!/usr/bin/env bash
# scripts/ci/family-explorer-publish.sh end to end against a stub `gh`: the joined country file, the ranked list, the new
# flips and a dated copy go to the release (created when there is none); a country with a shard missing uploads no file of
# its own; the dated copies beyond the newest 30 go.
# Run: bash scripts/ci/family-explorer-publish-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m family-explorer-publish.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin"
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
echo "$*" >> "$WORK/calls"
jqexpr=""
for ((i = 1; i <= $#; i++)); do [ "${!i}" = --jq ] && { j=$((i + 1)); jqexpr="${!j}"; }; done
case "$1 $2" in
  "release view") if [[ "$*" == *--json* ]]; then jq -r "$jqexpr" <<< "$ASSETS"; else [ -n "${RELEASE_EXISTS:-}" ]; fi ;;
  *) exit 0 ;;
esac
STUB
chmod +x "$work/bin/gh"
export PATH="$work/bin:$PATH" WORK="$work" GH_RELEASE_RETRY_DELAY=0 GITHUB_RUN_ID=77 ASSETS='{"assets":[]}'
header='kind\tcountry\tseed\tslice\tvenue\trawTitle\ttakes\tfirstSeen\towner\tnote\n'
row='family\tde\t\t\tKino Stub\tNo Such Film\tnone -> tmdb:1 -> none\t\t\tfinal add/remove [lattice 2^3] fam rules: join:same-title\n'
shard() { mkdir -p "$work/shards/identity-family-$1-$2of$3"; printf "$header$4" > "$work/shards/identity-family-$1-$2of$3/family-flips-$1.tsv"; }
publish() { # <expect> -> exit code
  rm -rf "$work/calls" "$work/temp"; mkdir -p "$work/temp"; : > "$work/calls"; : > "$work/summary"
  RUNNER_TEMP="$work/temp" GITHUB_STEP_SUMMARY="$work/summary" bash "$REPO_ROOT/scripts/ci/family-explorer-publish.sh" "$work/shards" "$1" > "$work/out" 2>&1
  echo $?; }

shard de 1 1 "$row"
uploaded='^release upload identity-family-explorer .*family-flips-de.tsv .*family-flips-new.tsv .*family-flips-ranked-.*-77.tsv .*family-flips-ranked.tsv --clobber$'
code=$(publish de=1)
check "a complete country is published with its ranking, the new flips and a dated copy, into a release it creates" "0 1 1 1 2 1" \
  "$code $(grep -c '^release create identity-family-explorer --prerelease' "$work/calls") $(grep -c "$uploaded" "$work/calls") $(grep -c 'join:same-title' "$work/temp/family-published/family-flips-ranked.tsv") $(grep -c 'baseline' "$work/summary") $(grep -c 'join:same-title 1' "$work/summary")"
check "a country with a shard missing uploads no file of its own" "0 0 1" \
  "$(publish de=1,uk=2) $(grep -c 'family-flips-uk.tsv' "$work/calls") $(grep -c 'uk: shard(s) 1 2 of 2 missing' "$work/summary")"
asset() { printf '{"name":"family-flips-ranked-2026-10-%02d-%s.tsv","createdAt":"2026-10-%02dT02:07:00Z"}' "$1" "$1" "$1"; }
ASSETS="{\"assets\":[$(for d in $(seq 1 32); do printf '%s,' "$(asset "$d")"; done)$(asset 33 | sed 's/-33-33/-33-33/')]}"
check "the dated copies beyond the newest 30 go, the oldest first" "0 3 1" \
  "$(RELEASE_EXISTS=1 publish de=1) $(grep -c '^release delete-asset' "$work/calls") $(grep -c '^release delete-asset identity-family-explorer family-flips-ranked-2026-10-01-1.tsv -y$' "$work/calls")"

spec_summary
