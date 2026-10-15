#!/usr/bin/env bash
# restore-country-corpus.sh against a stub `gh` serving a fake release and recording run.
# Run: bash scripts/restore-country-corpus-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m restore-country-corpus.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
stub="$work/bin"; release="$work/release"; runs="$work/runs"
mkdir -p "$stub" "$release" "$runs"

# The stub answers the three reads the script makes: release assets by pattern, a run's artifact
# by name, and the release's asset listing (for the fills). Anything absent answers as gh does.
cat > "$stub/gh" <<'STUB'
#!/usr/bin/env bash
dir=""; pattern=""; name=""; run=""
args=("$@")
for ((i = 0; i < ${#args[@]}; i++)); do
  case "${args[$i]}" in
    --dir) dir="${args[$((i + 1))]}" ;;
    --pattern) pattern="${args[$((i + 1))]}" ;;
    --name) name="${args[$((i + 1))]}" ;;
  esac
done
case "$1 $2" in
  "release view") ls -1 "$STUB_RELEASE"; exit 0 ;;
  "release download")
    matches=(); for f in "$STUB_RELEASE"/$pattern; do [ -e "$f" ] && matches+=("$f"); done
    [ "${#matches[@]}" -gt 0 ] || { echo "no assets match the file pattern" >&2; exit 1; }
    mkdir -p "$dir"; cp "${matches[@]}" "$dir/"; exit 0 ;;
  "run download")
    run="$3"; src="$STUB_RUNS/$run/$name"
    [ -d "$src" ] || { echo "no artifact matches any of the names or patterns provided" >&2; exit 1; }
    mkdir -p "$dir"; cp "$src"/* "$dir/"; exit 0 ;;
esac
echo "unexpected gh $*" >&2; exit 2
STUB
chmod +x "$stub/gh"

# A recorded pair for `de`: corpus artifact of run 42, tree asset, one fill.
src="$work/src"
mkdir -p "$src/test/resources/fixtures/corpus" "$src/test/resources/fixtures/enrichment-de/tmdb"
echo corpus  | gzip > "$src/test/resources/fixtures/corpus/cinema-scrapes-de.json.gz"
echo '{}'    > "$src/test/resources/fixtures/corpus/prod-coverage-de.json"
echo answer  > "$src/test/resources/fixtures/enrichment-de/tmdb/one.json"
mkdir -p "$runs/42/scrape-fixtures-de"
tar -C "$src" -czf "$runs/42/scrape-fixtures-de/scrapes-de.tar.gz" test/resources/fixtures/corpus
tar -C "$src" -czf "$release/enrichment-de-42.tar.gz" test/resources/fixtures/enrichment-de
fill="$work/fill"; mkdir -p "$fill/test/resources/fixtures/enrichment-de/tmdb"
echo filled > "$fill/test/resources/fixtures/enrichment-de/tmdb/two.json"
tar -C "$fill" -cf - test | zstd -q -c > "$release/fill-de-42-77-1-convergence.tar.zst"
echo "42 2026-10-11T02:00:00Z enrichment-de-42.tar.gz" > "$release/hermetic-de.txt"

# restore <code> -> "<exit status>|<stderr's last line>"; the destination is $work/out.
restore() {
  local err status
  rm -rf "$work/out"; mkdir -p "$work/out/test/resources/fixtures/enrichment-$1"
  echo stale > "$work/out/test/resources/fixtures/enrichment-$1/stale.json"
  err=$(PATH="$stub:$PATH" STUB_RELEASE="$release" STUB_RUNS="$runs" RESTORE_ROOT="$work/out" \
        CONVERGENCE_FILL_LIST_BACKOFF=0 bash "$REPO_ROOT/scripts/restore-country-corpus.sh" "$1" 2>&1 >/dev/null)
  status=$?
  echo "$status|$(printf '%s\n' "$err" | grep -v '^$' | tail -1)"
}

out="$work/out/test/resources/fixtures"
verdict=$(restore de)
check "the pinned pair restores" "0|" "${verdict%%|*}|"
check "the corpus lands where CorpusFixture looks" "corpus" "$(gzip -dc "$out/corpus/cinema-scrapes-de.json.gz" 2>/dev/null)"
check "the tree lands as the suite's fixture directory" "answer" "$(cat "$out/enrichment-de/tmdb/one.json" 2>/dev/null)"
check "the pair's fill is laid over the tree" "filled" "$(cat "$out/enrichment-de/tmdb/two.json" 2>/dev/null)"
check "a local tree is replaced, never merged into" "absent" "$([ -e "$out/enrichment-de/stale.json" ] && echo present || echo absent)"

verdict=$(restore uk)
check "no pinned pair fails, naming the recorder" "1" "${verdict%%|*}"
case "$verdict" in *'Record scrape fixtures'*) named=yes ;; *) named=no ;; esac
check "…and says how to record one" "yes" "$named"

echo "43 2026-10-11T02:00:00Z enrichment-es-43.tar.gz" > "$release/hermetic-es.txt"
verdict=$(restore es)
check "an expired corpus artifact fails rather than replaying anything else" "1" "${verdict%%|*}"
case "$verdict" in *'is gone'*) named=yes ;; *) named=no ;; esac
check "…and says the artifact is gone" "yes" "$named"

echo "44 2026-10-11T02:00:00Z" > "$release/hermetic-us.txt"
verdict=$(restore us)
check "a pin without a tree fails as malformed" "1|yes" "${verdict%%|*}|$([[ $verdict == *malformed* ]] && echo yes || echo no)"

status=$(bash "$REPO_ROOT/scripts/restore-country-corpus.sh" xx >/dev/null 2>&1; echo $?)
check "an unknown country is a usage error" "64" "$status"

spec_summary
