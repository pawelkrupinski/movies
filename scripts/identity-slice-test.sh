#!/usr/bin/env bash
# scripts/identity-slice: a log line read into its listing and build, each ref laid out once with the tool's files
# dropped in and run on ONE recording, the two refs' takes diffed and tallied against the labels, a ref the clone does
# not hold refused by name. java, sbt and gh are stubs; the repository is a scratch one.
# Run: bash scripts/identity-slice-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
# the scratch repository below is built only through the isolating helper: run from a hook, a bare git would act on
# the repository being pushed
. "$REPO_ROOT/scripts/scratch-git.sh"

printf '\033[36m▸\033[0m identity-slice\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin"

# A repository holding the tool (its scripts and the files it drops into a ref's tree) at two commits.
repo="$work/repo"
mkdir -p "$repo/scripts" "$repo/worker/src/test/scala/scripts" "$repo/worker/src/test/scala/services/identity" \
  "$repo/worker/src/fixtures/scala/tools" "$repo/common/src" "$repo/testkit/src" "$repo/test/resources/fixtures/identity-unmatched"
cp "$REPO_ROOT/scripts/identity-slice" "$REPO_ROOT/scripts/identity-explain" "$REPO_ROOT/scripts/identity-tools.sh" "$REPO_ROOT/scripts/identity_slice_diff.py" "$repo/scripts/"
for f in worker/src/test/scala/scripts/IdentitySlice.scala worker/src/test/scala/scripts/IdentityExplain.scala \
  worker/src/test/scala/services/identity/IdentitySlicing.scala worker/src/fixtures/scala/tools/RecordedCorpus.scala \
  worker/src/fixtures/scala/tools/SliceHttp.scala worker/src/fixtures/scala/tools/MemoLookups.scala worker/src/fixtures/scala/tools/CatalogueFill.scala \
  worker/src/fixtures/scala/tools/FiledAnswers.scala worker/src/fixtures/scala/tools/ProdMirror.scala; do
  echo "// $f" > "$repo/$f"
done
echo "build" > "$repo/build.sbt"
# the mirror's freshness check: records how it was asked, answers as $FRESHNESS_EXIT says
mkdir -p "$repo/scripts/local-mirror"
cat > "$repo/scripts/local-mirror/freshness" <<'STUB'
#!/usr/bin/env bash
echo "freshness $*" >> "$STUB/calls"
echo "de  kinowo_de_prod_mirror: synced …"
exit "${FRESHNESS_EXIT:-0}"
STUB
chmod +x "$repo/scripts/local-mirror/freshness"
printf 'country\tvenue\trawTitle\tfilm\tverdict\tnote\n' > "$repo/test/resources/fixtures/identity-unmatched/labels.tsv"
scratch_repo "$repo" && scratch_git "$repo" add -A && scratch_git "$repo" commit -qm base
echo "// head" >> "$repo/build.sbt"
scratch_git "$repo" commit -qam head
base=$(scratch_git "$repo" rev-parse HEAD~1) head=$(scratch_git "$repo" rev-parse HEAD)

# A recording already in the cache: nothing is downloaded.
cache="$work/cache"
mkdir -p "$cache/recordings/777/pair/test/resources/fixtures/corpus" "$cache/recordings/777/pair/test/resources/fixtures/enrichment-de"
echo corpus > "$cache/recordings/777/pair/test/resources/fixtures/corpus/cinema-scrapes-de.json.gz"
echo '{"recordedAt":"2026-10-14T05:00:00Z"}' > "$cache/recordings/777/pair/test/resources/fixtures/corpus/prod-coverage-de.json"

# sbt prints a classpath, as `export` does; java records its arguments and writes the takes the base and head refs take
cat > "$work/bin/sbt" <<'STUB'
#!/usr/bin/env bash
echo "sbt $PWD" >> "$STUB/calls"
echo "$PWD/worker/target/scala-3.9.0/test-classes:/lib/a.jar"
STUB
cat > "$work/bin/java" <<STUB
#!/usr/bin/env bash
echo "java \$PWD \$*" >> "$work/calls"
out="" ref=""
while [ "\$#" -gt 0 ]; do case "\$1" in --out) out=\$2 ;; --ref) ref=\$2 ;; esac; shift; done
[ -n "\$out" ] || { echo "explained"; exit 0; }
h='selector\tkey\tvenue\trawTitle\tfilm\ttitle\tbasis\tconfidence\tmodel\trules\tblocker\trunnerUp\tverdict\texplanation'
{ printf "\$h\n"
  if [ "\$ref" = "$base" ]; then
    printf 'k\tKino|Lalka\tKino\tLalka\ttmdb:1\tLalka (1968)\tOwnMatch\t0.9\ttmdb:1\t\t\t\tWRONG\twhy\n'
    printf 'k\tKino|Emil\tKino\tEmil\ttmdb:2\tEmil (2001)\tOwnMatch\t0.9\ttmdb:2\t\t\t\tRIGHT\twhy\n'
    printf 'k\tKino|Sinn\tKino\tSinn\tnone\t\tNoCandidate\t0\tnone\t\t\t\t-\twhy\n'
  else
    printf 'k\tKino|Lalka\tKino\tLalka\tnone\t\tBelowThreshold\t0.1\tnone\t\t\t\t-\twhy not\n'
    printf 'k\tKino|Emil\tKino\tEmil\tnone\t\tBelowThreshold\t0.1\tnone\t\t\t\t-\twhy not\n'
    printf 'k\tKino|Sinn\tKino\tSinn\ttmdb:9\tSinn (1990)\tOwnMatch\t0.8\ttmdb:9\t\t\t\tWRONG\twhy\n'
  fi; } > "\$out"
echo "decided"
[ -z "\${STUB_INCOMPLETE:-}" ] || { echo "SLICE INCOMPLETE" >&2; exit 3; }
STUB
chmod +x "$work/bin/sbt" "$work/bin/java"

slice() { STUB="$work" PATH="$work/bin:$PATH" KINOWO_IDENTITY_CACHE="$cache" "$repo/scripts/identity-slice" "$@" 2> "$work/err"; }

out=$(slice de --recording 777 --key 'Kino|Lalka' --ref HEAD~1 --ref HEAD)
check "both refs are laid out once in the cache" "2" "$(find "$cache/refs" -mindepth 1 -maxdepth 1 -type d | wc -l | tr -d ' ')"
check "...with the tool's files dropped in" "// worker/src/test/scala/scripts/IdentitySlice.scala" "$(cat "$cache/refs/$base/worker/src/test/scala/scripts/IdentitySlice.scala")"
check "each ref decides on the SAME recording" "2" "$(grep -c -- "--recording $cache/recordings/777/pair/test/resources/fixtures" "$work/calls")"
check "...each reading prod's filed answers from this checkout's capture" "2" "$(grep -c -- "--families $repo/test/resources/fixtures/identity-unmatched/de.json.gz" "$work/calls")"
check "...each under its own commit" "1" "$(grep -c -- "--ref $head" "$work/calls")"
check "the moves are tallied against the labels" "TALLY cured 1, lost 1, new-wrong 1, won 0, moved (unjudged) 0" "$(grep '^TALLY' <<< "$out")"
check "...each move shown with its verdict at both refs" "1" "$(grep -c 'Kino | Emil: tmdb:2 .Emil (2001). via OwnMatch \[RIGHT\] -> none via BelowThreshold \[-\]' <<< "$out")"

rm -f "$work/calls"
slice --log '{"country":"de","venue":"Kino","listing":"Lalka","build":"'"$base"'","projection_mode":"changed","kind":"lost"}' --recording 777 > /dev/null
check "a log line names the listing exactly" "1" "$(grep -c -- '--key Kino|Lalka' "$work/calls")"
check "...and decides it at the build that logged it" "1" "$(grep -c -- "--ref $base" "$work/calls")"

check "a log line naming no listing is refused" "2" "$(slice --log '{"country":"de"}' > /dev/null; echo $?)"
check "a ref the clone does not hold is refused by name" "1" "$(slice de --recording 777 --key 'Kino|Lalka' --ref 0123456789abcdef > /dev/null; grep -c 'no commit 0123456789abcdef' "$work/err")"
check "a batch file is handed to the JVM by its absolute path" "1" "$(cd "$work" && printf 'Kino\tLalka\n' > b.tsv && slice de --recording 777 --batch b.tsv --ref HEAD > /dev/null; grep -c -- "--batch $work/b.tsv" "$work/calls")"
out=$(cd "$work" && STUB_INCOMPLETE=1 slice de --recording 777 --batch b.tsv --ref HEAD~1 --ref HEAD)
check "a batch whose slices met a request they lack still diffs the two refs, the incompleteness said at each ref" "1 2" \
  "$(grep -c '^TALLY' <<< "$out") $(grep -c 'incomplete' "$work/err")"

rm -f "$work/calls" "$work/sbt-calls"
check "--compiles lays a ref out with the tool files and compiles it, deciding nothing" "0 0" \
  "$(slice --compiles HEAD~1 > /dev/null; echo $?) $(grep -c . "$work/calls" 2>/dev/null || echo 0)"

# --prod-mirror: prod's state from the LOCAL mirror, its freshness checked against the recording first
rm -f "$work/calls"
slice de --recording 777 --key 'Kino|Lalka' --prod-mirror > /dev/null
check "--prod-mirror checks the mirror's freshness against the recording's moment" "1" \
  "$(grep -c -- '^freshness de --quiet .*--not-before 2026-10-14T05:00:00Z' "$work/calls")"
check "...then hands the slice the LOCAL mirror, never prod" "1" "$(grep -c -- '--prod-mirror mongodb://127.0.0.1:28017/' "$work/calls")"
check "...the mirror reader dropped into a ref's tree" "1" "$(slice de --recording 777 --key 'Kino|Lalka' --ref HEAD --prod-mirror > /dev/null
  grep -c ProdMirror "$cache/refs/$head/worker/src/fixtures/scala/tools/ProdMirror.scala")"
rm -f "$work/calls"
FRESHNESS_EXIT=3 slice de --recording 777 --key 'Kino|Lalka' --prod-mirror de > /dev/null
check "a stale mirror is WARNed loudly, and read" "1 1" "$(grep -c 'THE PROD MIRROR IS NOT FRESH' "$work/err") $(grep -c -- '--prod-mirror' "$work/calls")"
rm -f "$work/calls"
check "no mirror at all refuses, never falling back to prod" "2 0" \
  "$(FRESHNESS_EXIT=2 slice de --recording 777 --key 'Kino|Lalka' --prod-mirror > /dev/null; echo $?) $(grep -c '^java' "$work/calls" 2> /dev/null)"
check "another country's mirror is refused" "2" "$(slice de --recording 777 --key 'Kino|Lalka' --prod-mirror uk > /dev/null; echo $?)"
check "...and a slice without it reads no mirror" "0" "$(rm -f "$work/calls"; slice de --recording 777 --key 'Kino|Lalka' > /dev/null; grep -c -- 'freshness\|--prod-mirror' "$work/calls")"

# --explain: the WHY trace of a listing the unmatched-cluster capture holds (scripts/identity-explain is its alias)
rm -f "$work/calls"
mkdir -p "$work/capture"; echo c > "$work/capture/de.json.gz"; printf 'country\n' > "$work/capture/labels.tsv"
slice de --explain --fixture "$work/capture" --venue Kino --title Lalka --ref HEAD > /dev/null
check "--explain replays the capture it is handed, judged by its labels" "1" \
  "$(grep -c -- "scripts.IdentityExplain de --fixture $work/capture/de.json.gz --labels $work/capture/labels.tsv --venue Kino --title Lalka" "$work/calls")"
check "...under the ref's code, the explainer dropped into its tree" "1 // worker/src/test/scala/scripts/IdentityExplain.scala" \
  "$(grep -c "java $cache/refs/$head " "$work/calls") $(cat "$cache/refs/$head/worker/src/test/scala/scripts/IdentityExplain.scala")"
check "...and no recording is fetched for it" "0" "$(grep -c -- '--recording' "$work/calls")"
rm -f "$work/calls"
STUB="$work" PATH="$work/bin:$PATH" KINOWO_IDENTITY_CACHE="$cache" "$repo/scripts/identity-explain" de --fixture "$work/capture" --venue Kino > /dev/null 2>&1
check "scripts/identity-explain is identity-slice --explain" "1" \
  "$(grep -c -- "scripts.IdentityExplain de --fixture $work/capture/de.json.gz --labels $work/capture/labels.tsv --venue Kino" "$work/calls")"

spec_summary
