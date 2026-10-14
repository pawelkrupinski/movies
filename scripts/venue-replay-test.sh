#!/usr/bin/env bash
# scripts/venue-replay: each ref laid out once with the tool's files dropped in and read on ONE recording, the two
# refs' listings diffed field by field, a replay's missing requests (exit 3) reported without hiding the listings, and
# --prod skipped when the local mirror is not there. java, sbt, gh and the freshness check are stubs; the repository is
# a scratch one.
# Run: bash scripts/venue-replay-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
# the scratch repository below is built only through the isolating helper: run from a hook, a bare git would act on
# the repository being pushed
. "$REPO_ROOT/scripts/scratch-git.sh"

printf '\033[36m▸\033[0m venue-replay\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin"

# A repository holding the tool (its scripts and the files it drops into a ref's tree) at two commits.
repo="$work/repo"
mkdir -p "$repo/scripts/local-mirror" "$repo/worker/src/test/scala/scripts" "$repo/worker/src/fixtures/scala/tools" "$repo/common/src" "$repo/testkit/src"
cp "$REPO_ROOT/scripts/venue-replay" "$REPO_ROOT/scripts/venue-replay-prod.js" "$REPO_ROOT/scripts/identity-tools.sh" \
  "$REPO_ROOT/scripts/venue_replay_diff.py" "$repo/scripts/"
for f in worker/src/test/scala/scripts/VenueReplay.scala worker/src/fixtures/scala/tools/RecordedCorpus.scala \
  worker/src/fixtures/scala/tools/CinemaSlotInvariants.scala worker/src/fixtures/scala/tools/SearchQueryMarkers.scala; do
  echo "// $f" > "$repo/$f"
done
# the mirror is not there: --prod says so and goes on
printf '#!/usr/bin/env bash\necho "no mirror" >&2\nexit 2\n' > "$repo/scripts/local-mirror/freshness"
chmod +x "$repo/scripts/local-mirror/freshness"
echo "build" > "$repo/build.sbt"
scratch_repo "$repo" && scratch_git "$repo" add -A && scratch_git "$repo" commit -qm base
echo "// head" >> "$repo/build.sbt"
scratch_git "$repo" commit -qam head
base=$(scratch_git "$repo" rev-parse HEAD~1) head=$(scratch_git "$repo" rev-parse HEAD)

# A recording already in the cache: nothing is downloaded.
cache="$work/cache"
mkdir -p "$cache/recordings/777/pair/test/resources/fixtures/corpus" "$cache/recordings/777/pair/test/resources/fixtures/enrichment-pl"
echo corpus > "$cache/recordings/777/pair/test/resources/fixtures/corpus/cinema-scrapes-pl.json.gz"

# sbt prints a classpath, as `export` does; java records its arguments and writes the listings the base and head refs read
cat > "$work/bin/sbt" <<'STUB'
#!/usr/bin/env bash
echo "$PWD/worker/target/scala-3.9.0/test-classes:/lib/a.jar"
STUB
cat > "$work/bin/java" <<STUB
#!/usr/bin/env bash
echo "java \$PWD \$*" >> "$work/calls"
out="" ref=""
while [ "\$#" -gt 0 ]; do case "\$1" in --out) out=\$2 ;; --ref) ref=\$2 ;; esac; shift; done
{ printf 'raw\tclean\tsynopsis\turl\n'
  if [ "\$ref" = HEAD~1 ]; then printf 'Trzy\tTrzy\tScenariusz: X\thttps://k/trzy\nObcy\tObcy\t-\thttps://k/obcy\n'
  else printf 'Trzy\tTrzy\tPo rozstaniu\thttps://k/trzy\nRoza\tRoza\t-\thttps://k/roza\n'; fi; } > "\$out"
echo "listings at \$ref"
[ -z "\${JAVA_EXIT:-}" ] || { echo "FAIL: the recorded trees lack 1 request(s)" >&2; exit "\$JAVA_EXIT"; }
STUB
chmod +x "$work/bin/sbt" "$work/bin/java"

replay() { STUB="$work" PATH="$work/bin:$PATH" KINOWO_IDENTITY_CACHE="$cache" "$repo/scripts/venue-replay" "$@" 2> "$work/err"; }

out=$(replay pl "ziemi rybnickiej" --recording 777 --title trzy --ref HEAD~1 --ref HEAD)
check "both refs are laid out once in the cache" "2" "$(find "$cache/refs" -mindepth 1 -maxdepth 1 -type d | wc -l | tr -d ' ')"
check "...with the tool's files dropped in" "// worker/src/test/scala/scripts/VenueReplay.scala // worker/src/fixtures/scala/tools/SearchQueryMarkers.scala" \
  "$(cat "$cache/refs/$base/worker/src/test/scala/scripts/VenueReplay.scala" "$cache/refs/$head/worker/src/fixtures/scala/tools/SearchQueryMarkers.scala" | tr '\n' ' ' | sed 's/ $//')"
check "each ref reads the SAME recording" "2" "$(grep -c -- "--corpus $cache/recordings/777/pair/test/resources/fixtures/corpus/cinema-scrapes-pl.json.gz --recording $cache/recordings/777/pair/test/resources/fixtures" "$work/calls")"
check "...the venue and the selectors handed through" "2" "$(grep -c -- "scripts.VenueReplay pl --venue ziemi rybnickiej .*--title trzy" "$work/calls")"
check "both refs' listings are printed" "1 1" "$(grep -c '^listings at HEAD~1$' <<< "$out") $(grep -c '^listings at HEAD$' <<< "$out")"
check "a listing only one ref reads is named" "- Obcy: only at HEAD~1|+ Roza: only at HEAD" "$(grep -E '^[-+] ' <<< "$out" | tr '\n' '|' | sed 's/|$//')"
check "...and a field the two read differently, before and after" "~ Trzy|synopsis: 'Scenariusz: X'|→ 'Po rozstaniu'" \
  "$(grep -A2 '^~ Trzy' <<< "$out" | sed 's/^ *//' | tr '\n' '|' | sed 's/|$//')"

rm -f "$work/calls"
out=$(JAVA_EXIT=3 replay pl kino --recording 777); status=$?
check "a replay missing a recorded request exits 3" "3" "$status"
check "...after printing what it did read" "listings at ." "$out"

out=$(JAVA_EXIT=1 replay pl kino --recording 777 --ref HEAD~1 --ref HEAD); status=$?
check "a replay that fails otherwise stops at that ref" "1 1" "$status $(grep -c 'listings at' <<< "$out")"

rm -f "$work/calls"
replay pl kino --recording 777 --prod > /dev/null
check "--prod with no local mirror is skipped, by name" "1 0" "$(grep -c 'no local prod mirror for pl' "$work/err") $(grep -c -- '--prod' "$work/calls")"

check "a fixture tree is handed over by its absolute path" "1" \
  "$(mkdir -p "$work/tree" && cd "$work" && replay pl kino --recording 777 --fixtures tree > /dev/null; grep -c -- "--fixtures $work/tree" "$work/calls")"
check "a fixture tree that is not there is refused" "2" "$(replay pl kino --recording 777 --fixtures "$work/nope" > /dev/null; echo $?)"
check "an unknown argument is refused" "2" "$(replay pl kino --wat > /dev/null; echo $?)"

spec_summary
