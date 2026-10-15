#!/usr/bin/env bash
# scripts/identity-judge-cards: the capture it is pointed at (this checkout's, a directory, a run's artifacts) handed to
# scripts.IdentityJudgeCards with the judgements that judge it, the options passed through, and a capture or compile
# that fails stopping it before the JVM. sbt, java and gh are stubs; the repository is a scratch copy of the scripts.
# Run: bash scripts/identity-judge-cards-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m identity-judge-cards\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin"

repo="$work/repo"
mkdir -p "$repo/scripts" "$repo/test/resources/fixtures/identity-unmatched" "$repo/worker/src" "$repo/common/src" \
  "$repo/identity/src" "$repo/testkit/src"
cp "$REPO_ROOT/scripts/identity-judge-cards" "$REPO_ROOT/scripts/identity-tools.sh" "$REPO_ROOT/scripts/tool-conventions.sh" "$REPO_ROOT/scripts/vault-env.sh" "$repo/scripts/"
echo pl > "$repo/test/resources/fixtures/identity-unmatched/pl.json.gz"
echo labels-here > "$repo/test/resources/fixtures/identity-judgements.tsv"
echo build > "$repo/build.sbt"

cat > "$work/bin/sbt" <<'STUB'
#!/usr/bin/env bash
echo "sbt $*" >> "$STUB/calls"
[ -z "${SBT_FAILS:-}" ] || { echo "[error] compile failed"; exit 1; }
echo "$PWD/worker/target/scala-3.9.0/test-classes:/lib/a.jar"
STUB
cat > "$work/bin/java" <<'STUB'
#!/usr/bin/env bash
echo "java $PWD $*" >> "$STUB/calls"
echo "cards written"
STUB
# `run download` copies the named artifact's tree into --dir
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
while [ "$#" -gt 0 ]; do case "$1" in --dir) dir="$2" ;; --name) name="$2" ;; esac; shift; done
[ -d "$STUB/artifacts/$name" ] || { echo "no artifact matches any of the names or patterns provided" >&2; exit 1; }
mkdir -p "$dir"; cp -R "$STUB/artifacts/$name/." "$dir/"
STUB
chmod +x "$work/bin/sbt" "$work/bin/java" "$work/bin/gh"
mkdir -p "$work/artifacts/identity-capture-de/x"; echo de > "$work/artifacts/identity-capture-de/x/de.json.gz"

cards() { (cd "$work" && env -u TMDB_API_KEY STUB="$work" PATH="$work/bin:$PATH" KINOWO_IDENTITY_CACHE="$work/cache" \
  "$repo/scripts/identity-judge-cards" "$@" > "$work/out" 2> "$work/err"); echo $?; }
java_line() { grep '^java' "$work/calls" | tail -1; }

check "this checkout's capture, judged by this checkout's judgements" "0" "$(cards --country pl)"
check "...handed to IdentityJudgeCards, run from the repository" \
  "java $repo -Xmx6g -Duser.language=en -Duser.country=US --sun-misc-unsafe-memory-access=allow -cp $repo/worker/target/scala-3.9.0/test-classes:/lib/a.jar scripts.IdentityJudgeCards --fixtures $repo/test/resources/fixtures/identity-unmatched --labels $repo/test/resources/fixtures/identity-judgements.tsv --country pl" \
  "$(java_line)"
check "...saying a record the capture never read stays unknown without a TMDB key" "1" "$(grep -c 'no TMDB_API_KEY' "$work/err")"

mkdir -p "$work/capture"; echo c > "$work/capture/pl.json.gz"; echo labels-there > "$work/capture/identity-judgements.tsv"
cards --fixtures "$work/capture" --country pl --out out-dir --all --offline > /dev/null
check "a capture directory is judged by its own judgements; --out made absolute, --all and --offline passed" \
  "--fixtures $work/capture --labels $work/capture/identity-judgements.tsv --country pl --out $work/out-dir --all --offline" \
  "$(java_line | sed 's/.*scripts.IdentityJudgeCards //')"
check "...--out created" "yes" "$([ -d "$work/out-dir" ] && echo yes || echo no)"
check "...and --offline asks for no key" "0" "$(grep -c 'no TMDB_API_KEY' "$work/err")"

check "a run's capture is downloaded for each country asked" "0" "$(cards --fixtures 987654 --country de)"
check "...into the cache, judged by this checkout's judgements" \
  "--fixtures $work/cache/captures/run-987654 --labels $work/cache/captures/run-987654/identity-judgements.tsv --country de" \
  "$(java_line | sed 's/.*scripts.IdentityJudgeCards //')"
check "...those judgements this checkout's" "labels-here" "$(cat "$work/cache/captures/run-987654/identity-judgements.tsv")"

rm -f "$work/calls"; touch "$work/calls"
check "a run lacking a country's capture stops before the JVM" "1 0" \
  "$(cards --fixtures 987654 --country de,uk) $(grep -c '^java' "$work/calls")"
rm -f "$work/calls" "$repo/target/identity-tools/classpath"; touch "$work/calls"
check "a compile that fails stops before the JVM" "1 0" "$(SBT_FAILS=1 cards --country pl) $(grep -c '^java' "$work/calls")"
check "--countries, the other tools' spelling, is the same flag" "0 --country pl,de" \
  "$(cards --countries PL --country=de) $(java_line | sed 's/.*scripts.IdentityJudgeCards .*--labels [^ ]* //')"
check "a country no tool knows is a usage error, before any download" "2 1" "$(cards --countries pl,xx) $(grep -c 'unknown country xx' "$work/err")"
check "an unknown argument is a usage error" "2" "$(cards --bogus)"
check "--help prints the usage" "0 1" "$(cards --help) $(grep -q -- '--fixtures SRC' "$work/out" && echo 1)"

spec_summary
