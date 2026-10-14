#!/usr/bin/env bash
# identity-tools.sh (what identity-slice --explain and identity-judge-cards share): a capture fetched once into the shared
# cache from a run's artifacts or a git ref, and a tree's classpath compiled only when a source is newer.
# Run: bash scripts/identity-tools-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
# the scratch repository below is built only through the isolating helper: run from a hook, a bare git would act on
# the repository being pushed
. "$REPO_ROOT/scripts/scratch-git.sh"

printf '\033[36m▸\033[0m identity-tools.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/artifacts/identity-capture-de/test/resources/fixtures/identity-unmatched"
echo de-capture > "$work/artifacts/identity-capture-de/test/resources/fixtures/identity-unmatched/de.json.gz"

# `run download` copies the named artifact's tree into --dir; sbt prints a classpath, as `export` does
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
echo "gh $*" >> "$STUB/calls"
while [ "$#" -gt 0 ]; do case "$1" in --dir) dir="$2" ;; --name) name="$2" ;; esac; shift; done
[ -d "$STUB/artifacts/$name" ] || exit 1
mkdir -p "$dir"; cp -R "$STUB/artifacts/$name/." "$dir/"
STUB
cat > "$work/bin/sbt" <<'STUB'
#!/usr/bin/env bash
echo "sbt $*" >> "$STUB/calls"
echo "$PWD/worker/target/scala-3.9.0/test-classes:/lib/a.jar"
STUB
chmod +x "$work/bin/gh" "$work/bin/sbt"

# A repository holding a capture at one commit, to read by ref.
repo="$work/repo"
mkdir -p "$repo/test/resources/fixtures/identity-unmatched"
echo pl-at-ref > "$repo/test/resources/fixtures/identity-unmatched/pl.json.gz"
echo "legacy-labels" > "$repo/test/resources/fixtures/identity-unmatched/labels.tsv"
scratch_repo "$repo" && scratch_git "$repo" add -A && scratch_git "$repo" commit -qm "capture, judged by labels.tsv"
legacy=$(scratch_git "$repo" rev-parse HEAD)
scratch_git "$repo" rm -q test/resources/fixtures/identity-unmatched/labels.tsv
echo "labels-at-ref" > "$repo/test/resources/fixtures/identity-judgements.tsv"
scratch_git "$repo" add -A && scratch_git "$repo" commit -qm "the judgements, one file"
echo "labels-here" > "$repo/test/resources/fixtures/identity-judgements.tsv"

tools() {
  STUB="$work" PATH="$work/bin:$PATH" KINOWO_IDENTITY_CACHE="$work/cache" bash -c \
    '. "$1"; IDENTITY_REPO="$2"; shift 2; "$@"' _ "$REPO_ROOT/scripts/identity-tools.sh" "$repo" "$@" 2> "$work/err"
}

check "no source is this checkout's capture" "$repo/test/resources/fixtures/identity-unmatched" "$(tools identity_capture "" pl)"
check "a run id downloads that country's capture into the cache" "de-capture" "$(cat "$(tools identity_capture 987654 de)/de.json.gz")"
check "...judged by this checkout's judgements" "labels-here" "$(cat "$work/cache/captures/run-987654/identity-judgements.tsv")"
rm -f "$work/calls"
tools identity_capture 987654 de > /dev/null
check "...and is not downloaded again" "0" "$(grep -c 'run download' "$work/calls" 2>/dev/null || echo 0)"
check "a ref's capture is read from git, without a checkout" "pl-at-ref" "$(cat "$(tools identity_capture HEAD pl)/pl.json.gz")"
check "...with that ref's judgements" "labels-at-ref" "$(cat "$(tools identity_capture HEAD pl)/identity-judgements.tsv")"
check "...a ref from before the judgements were one file: its labels.tsv" "legacy-labels" \
  "$(cat "$(tools identity_capture "$legacy" pl)/identity-judgements.tsv")"
check "this checkout's capture is judged by this checkout's judgements" "$repo/test/resources/fixtures/identity-judgements.tsv" \
  "$(tools identity_judgements "$(tools identity_capture "" pl)")"
check "a ref that is none fails" "2" "$(tools identity_capture no-such-ref pl > /dev/null; echo $?)"

tree="$work/tree"
mkdir -p "$tree/common/src" "$tree/identity/src" "$tree/worker/src" "$tree/testkit/src"
echo 'object A' > "$tree/worker/src/A.scala"
check "a tree's classpath is compiled and exported" "$tree/worker/target/scala-3.9.0/test-classes:/lib/a.jar" "$(tools identity_classpath "$tree")"
rm -f "$work/calls"
check "...then read from the cache" "$tree/worker/target/scala-3.9.0/test-classes:/lib/a.jar" "$(tools identity_classpath "$tree")"
check "...sbt not run while no source is newer" "0" "$(grep -c '^sbt' "$work/calls" 2>/dev/null || echo 0)"
touch -t 209901010000 "$tree/worker/src/A.scala"
tools identity_classpath "$tree" > /dev/null
check "...and compiled again once a source changes" "1" "$(grep -c '^sbt' "$work/calls" 2>/dev/null || echo 0)"

# The default recording is the newest run holding the country's corpus: the newest run has no UK artifact, an older does.
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
case "$1" in
  run) printf '300\n200\n100\n' ;;
  # the jq filter names the artifact wanted; the stub prints it only for a run that holds it
  api) want=$(grep -o 'scrape-fixtures-[a-z]*' <<<"$*")
       case "$2" in */runs/200/*) held="scrape-fixtures-de" ;; */runs/100/*) held="scrape-fixtures-de scrape-fixtures-uk" ;; *) held="" ;; esac
       case " $held " in *" $want "*) echo "$want" ;; esac ;;
esac
STUB
check "no country: the newest successful recording" "300" "$(tools identity_newest_recording)"
check "a country: the newest holding its corpus" "100" "$(tools identity_newest_recording uk)"
check "...saying which newer ones it skipped" "2" "$(grep -c 'skipped: it holds no uk corpus' "$work/err")"
check "a country the newest holds is the newest" "200" "$(tools identity_newest_recording de | head -1)"
check "a country no recent run holds fails" "1" "$(tools identity_newest_recording es > /dev/null; echo $?)"

spec_summary
