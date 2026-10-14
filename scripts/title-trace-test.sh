#!/usr/bin/env bash
# scripts/title-trace: the raw title handed to ONE JVM whole, each ref laid out once with the tracer's files dropped in,
# two refs' traces printed and diffed to the steps that moved, a call naming no country refused. java and sbt are stubs;
# the repository is a scratch one.
# Run: bash scripts/title-trace-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"

printf '\033[36m▸\033[0m title-trace\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin"

repo="$work/repo"
mkdir -p "$repo/scripts" "$repo/worker/src/test/scala/services/identity" "$repo/worker/src/fixtures/scala/tools" "$repo/common/src" "$repo/identity/src" "$repo/testkit/src"
cp "$REPO_ROOT/scripts/title-trace" "$REPO_ROOT/scripts/identity-tools.sh" "$repo/scripts/"
for f in worker/src/test/scala/services/identity/TitleTrace.scala worker/src/fixtures/scala/tools/SearchQueryMarkers.scala \
  worker/src/fixtures/scala/tools/CinemaSlotInvariants.scala; do
  echo "// $f" > "$repo/$f"
done
echo "rule a" > "$repo/build.sbt"
scratch_repo "$repo" && scratch_git "$repo" add -A && scratch_git "$repo" commit -qm base
echo "rule b" > "$repo/build.sbt"
scratch_git "$repo" commit -qam head
base=$(scratch_git "$repo" rev-parse HEAD~1)

cat > "$work/bin/sbt" <<'STUB'
#!/usr/bin/env bash
echo "$PWD/worker/target/scala-3.9.0/test-classes:/lib/a.jar"
STUB
# java prints a trace whose search step is the tree's build.sbt: the code a ref holds
cat > "$work/bin/java" <<STUB
#!/usr/bin/env bash
{ printf 'java %s' "\$PWD"; printf ' <%s>' "\$@"; echo; } >> "$work/calls"
case " \$* " in *" --batch "*) # a batch: two billings, the second's search step the tree's build.sbt
  echo "### pl	Kino Muza	Amator"; echo "3. search: same"
  echo "### pl	Kino Muza	Obsession (18)"; echo "3. search: \$(cat build.sbt)"; exit ;;
esac
echo "1. ingest"
echo "3. search: \$(cat build.sbt)"
STUB
chmod +x "$work/bin/sbt" "$work/bin/java"

trace() { PATH="$work/bin:$PATH" KINOWO_IDENTITY_CACHE="$work/cache" "$repo/scripts/title-trace" "$@" 2> "$work/err"; }

out=$(trace "Pokaz przyjazny osobom z ADHD: Kandydaci śmierci" --country pl --venue "Kino Muza")
check "without --ref, this checkout's code traces the title whole" "1" \
  "$(grep -c "java $repo .*<services.identity.TitleTrace> <Pokaz przyjazny osobom z ADHD: Kandydaci śmierci> <--country> <pl> <--venue> <Kino Muza>" "$work/calls")"
check "...and prints its trace" "3. search: rule b" "$(grep '^3\.' <<< "$out")"

rm -f "$work/calls"
out=$(trace "Obsession (18)" --country uk --ref HEAD~1 --ref HEAD)
check "each ref is laid out once in the cache" "2" "$(find "$work/cache/refs" -mindepth 1 -maxdepth 1 -type d | wc -l | tr -d ' ')"
check "...with the tracer dropped in" "// worker/src/test/scala/services/identity/TitleTrace.scala" \
  "$(cat "$work/cache/refs/$base/worker/src/test/scala/services/identity/TitleTrace.scala")"
check "...each traced in its own JVM" "2" "$(grep -c "TitleTrace> <Obsession (18)>" "$work/calls")"
check "the diff names the step that moved" "-3. search: rule a|+3. search: rule b" "$(grep -E '^[-+]3\.' <<< "$out" | paste -sd'|' -)"

out=$(trace "Obsession (18)" --country uk --ref HEAD --ref HEAD)
check "two refs of one code say so" "1" "$(grep -c 'every step the same' <<< "$out")"
printf 'pl\tKino Muza\tAmator\npl\tKino Muza\tObsession (18)\n' > "$work/batch.tsv"
rm -f "$work/calls"
out=$(trace --batch "$work/batch.tsv" --ref HEAD~1 --ref HEAD)
check "a batch is handed to one JVM per ref" "2" "$(grep -c "TitleTrace> <--batch> <$work/batch.tsv>" "$work/calls")"
check "...and names only the billing that moved, with its step" "### pl	Kino Muza	Obsession (18)|-3. search: rule a|+3. search: rule b" \
  "$(grep -E '^(###|[-+]3)' <<< "$out" | paste -sd'|' -)"
check "...and counts it" "1" "$(grep -c '1 of 2 billings moved' <<< "$out")"
check "a call naming no country is refused" "2" "$(trace "Obsession (18)" > /dev/null; echo $?)"
check "two raw titles are refused" "2" "$(trace Obsession Lalka --country pl > /dev/null; echo $?)"

spec_summary
