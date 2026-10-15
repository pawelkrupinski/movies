#!/usr/bin/env bash
# convergence-bisect.sh (which commit turned a hermetic leg red), run for real: `last-green` against a stub `gh`, and
# `bisect` over a scratch repository whose pipeline commits a stub step command judges by their content -- every
# verdict the script can write, the caps, and the reads whose failure must fail rather than read as "nothing".
# ConvergenceBisectSpec holds the same script's verdict prose from the Scala side.
# Run: bash .github/scripts/convergence-bisect-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
# shellcheck source=scripts/shell-spec.sh
. "$REPO_ROOT/scripts/shell-spec.sh"
# the scratch repository is built only through the isolating helper: run from a hook, a bare git would act on the
# repository being pushed
# shellcheck source=scripts/scratch-git.sh
. "$REPO_ROOT/scripts/scratch-git.sh"
SCRIPT="$REPO_ROOT/.github/scripts/convergence-bisect.sh"

printf '\033[36m▸\033[0m convergence-bisect.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin"
printf 'pipeline/**\n# a comment\n\n' > "$work/paths.txt"

# ── last-green ──────────────────────────────────────────────────────────────────────────────────
# `run list` prints "$STUB/runs" (or fails when it is absent); `run view <id>` prints the jobs file of that run as
# the --jq filter would (the names of the matching jobs), failing when "$STUB/view-<id>.fail" exists.
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
echo "gh $*" >> "$STUB/calls"
case "$1 $2" in
  "run list") [ -f "$STUB/runs" ] || { echo "HTTP 502" >&2; exit 1; }; cat "$STUB/runs" ;;
  "run view") run=$3
              [ -f "$STUB/view-$run.fail" ] && { echo "HTTP 500" >&2; exit 1; }
              # the jobs that passed, filtered by the job name the caller's --jq asks for
              jq=""; while [ "$#" -gt 0 ]; do [ "$1" = --jq ] && jq="$2"; shift; done
              want=$(printf '%s' "$jq" | sed -n 's/.*\.name == "\([^"]*\)".*/\1/p')
              grep -x "$want" "$STUB/green-$run" 2>/dev/null || true ;;
esac
STUB
chmod +x "$work/bin/gh"

last_green() {
  STUB="$work" PATH="$work/bin:$PATH" COUNTRY=poland WORKFLOW="Identity model convergence" RUN_ID=900 \
    bash "$SCRIPT" last-green 2> "$work/err"
}
printf '900 sha-this-run\n901 sha-red\n902 sha-sample-only\n903 sha-green\n904 sha-older-green\n' > "$work/runs"
echo "poland / convergence" > "$work/green-900"
echo "poland / sample" > "$work/green-902"
printf 'germany / convergence\npoland / convergence\n' > "$work/green-903"
echo "poland / convergence" > "$work/green-904"
check "the newest run whose FULL leg passed, this run and a sample-only pass aside" "sha-green|0" "$(out=$(last_green); echo "$out|$?")"
check "...read no further than it" "0" "$(grep -c 'run view 904' "$work/calls")"
check "...and never this run itself" "0" "$(grep -c 'run view 900' "$work/calls")"
rm -f "$work/green-903" "$work/green-904"
check "no run's full leg passed: exit 1, nothing printed" "|1" "$(out=$(last_green); echo "$out|$?")"
echo "poland / convergence" > "$work/green-904"
touch "$work/view-903.fail"
check "a run that cannot be read is no red run: exit 2, not an older base" "|2" "$(out=$(last_green); echo "$out|$?")"
rm -f "$work/view-903.fail" "$work/runs"
check "the runs cannot be listed: exit 2" "|2" "$(out=$(last_green); echo "$out|$?")"
: > "$work/runs"
check "no runs at all: exit 1" "|1" "$(out=$(last_green); echo "$out|$?")"
check "a word that is no command: usage, exit 2" "2" "$(bash "$SCRIPT" nonsense 2> /dev/null; echo $?)"

# ── bisect ──────────────────────────────────────────────────────────────────────────────────────
# History: base (good) -> docs-only -> pipeline 1..6, where pipeline 4 breaks the leg; the step command is red while
# pipeline/state says broken, untestable at a commit listed in "$work/untestable", and logs every replay.
repo="$work/repo"
scratch_repo "$repo"
commit() { # <file> <content> <message> -> the new commit's sha
  mkdir -p "$repo/$(dirname "$1")"; printf '%s\n' "$2" > "$repo/$1"
  scratch_git "$repo" add -A && scratch_git "$repo" commit -qm "$3" && scratch_git "$repo" rev-parse HEAD
}
good=$(commit pipeline/state ok "base")
docs=$(commit docs/readme "words" "docs only")
commit pipeline/a 1 "pipeline change 1" > /dev/null
commit pipeline/a 2 "pipeline change 2" > /dev/null
p3=$(commit pipeline/a 3 "pipeline change 3")
p4=$(commit pipeline/state broken "pipeline change 4")
commit pipeline/a 5 "pipeline change 5" > /dev/null
bad=$(commit pipeline/a 6 "pipeline change 6")

cat > "$work/step" <<STEP
#!/usr/bin/env bash
sha=\$(cat .git/HEAD) # the bisect checks each commit out detached: HEAD holds its sha, no git asked
echo "\$sha" >> "$work/replays"
grep -qx "\$sha" "$work/untestable" 2>/dev/null && exit 125
grep -qx broken pipeline/state && exit 1
exit 0
STEP
chmod +x "$work/step"

bisect() { # <good> <bad> [env...] -> the verdict file's contents; replays and first-bad beside it
  local g=$1 b=$2; shift 2
  rm -f "$work/verdict" "$work/replays" "$work/first-bad"
  (cd "$repo" && env CONVERGENCE_PATHS_FILE="$work/paths.txt" COUNTRY=poland STEP_COMMAND="$work/step" \
    STEP_MINUTES=1 VERDICT_FILE="$work/verdict" FIRST_BAD_FILE="$work/first-bad" MAX_STEPS=10 "$@" \
    bash "$SCRIPT" bisect "$g" "$b" > /dev/null 2>&1)
  echo "status $?"
  cat "$work/verdict"
}
replays() { grep -c . "$work/replays" 2>/dev/null || echo 0; }
has() { case "$2" in *"$1"*) echo yes ;; *) echo "no: $2" ;; esac; }

v=$(bisect "$good" "$bad")
check "the range counts only pipeline commits, the docs one aside" yes "$(has "6 pipeline commit(s)" "$v")"
check "the first bad commit is pinned exactly" yes "$(has "First bad commit: \`${p4:0:7}" "$v")"
check "...and written to FIRST_BAD_FILE" "$p4" "$(cat "$work/first-bad")"
check "...the run ends 0" yes "$(has "status 0" "$v")"
check "<bad> and <good> are replayed first, before any halving" "$bad $good" "$(head -2 "$work/replays" | tr '\n' ' ' | sed 's/ $//')"
check "the bisect leaves the repository off bisect" "no" "$( [ -e "$repo/.git/BISECT_START" ] && echo yes || echo no)"

v=$(bisect "$good" "$bad" SAMPLE_FAILED=true)
check "a sample already red at <bad> is not replayed there" "$good" "$(head -1 "$work/replays")"
check "...and still pins the commit" yes "$(has "First bad commit: \`${p4:0:7}" "$v")"

v=$(bisect "$good" "$bad" MAX_STEPS=1)
check "MAX_STEPS caps the halvings: a range, never a guess" yes "$(has "Stopped after 1 replay(s)" "$v")"
check "...which lists the first bad commit among its candidates" yes "$(has "${p4:0:7}" "$v")"
check "...and FIRST_BAD_FILE stays unwritten" "no" "$( [ -e "$work/first-bad" ] && echo yes || echo no)"
check "...two replays to confirm, one halving" "3" "$(replays)"

v=$(bisect "$good" "$bad" MAX_STEPS=0)
check "no halving allowed: the whole range, after 0 replays" yes "$(has "Stopped after 0 replay(s)" "$v")"
check "...every pipeline commit listed" "6" "$(printf '%s\n' "$v" | grep -c '^- ')"
check "...the docs commit not among them" yes "$(printf '%s\n' "$v" | grep -q 'docs only' && echo "no: listed" || echo yes)"

v=$(bisect "$good" "$bad" BUDGET_MINUTES=0)
check "a budget that cannot fit one more step starts none" yes "$(has "Stopped after 0 replay(s)" "$v")"

printf '%s\n%s\n' "$p3" "$p4" > "$work/untestable"
v=$(bisect "$good" "$bad")
check "a commit the step cannot test stays a candidate, marked untested" yes "$(has "untested (the sample could not run here)" "$v")"
check "...the pin falls back to a range around the untested commits" yes "$(has "The first bad commit is one of:" "$v")"
rm -f "$work/untestable"

v=$(bisect "$bad" "$bad")
check "<good> = <bad>: a flake or recording change, not bisected" yes "$(has "was GREEN in an earlier run" "$v")"
check "...nothing replayed" "0" "$(replays)"

v=$(bisect "$bad" "$good")
check "<good> not an ancestor of <bad>: no range" yes "$(has "is not an ancestor of" "$v")"

v=$(bisect "$good" "$docs")
check "no pipeline commit in the range: data, not code" yes "$(has "touches the pipeline" "$v")"
check "...nothing replayed" "0" "$(replays)"

v=$(bisect "$good" "$p3")
check "a range red only in the full leg: the sample does not reproduce it at <bad>" yes "$(has "does not reproduce the failure" "$v")"

v=$(bisect "$p4" "$bad" SAMPLE_FAILED=true)
check "the sample red at <good> too: the recorded pair, not a commit" yes "$(has "red at the last green commit" "$v")"
check "...no first bad commit named" "no" "$( [ -e "$work/first-bad" ] && echo yes || echo no)"

echo "$good" > "$work/untestable"
v=$(bisect "$good" "$bad" SAMPLE_FAILED=true)
check "<good> untestable under this recording: nothing can be blamed" yes "$(has "could not confirm" "$v")"
rm -f "$work/untestable"

v=$(bisect "$p3" "$p4")
check "one pipeline commit in the range: it, after confirming both ends" yes "$(has "the only pipeline commit in the range" "$v")"
check "...pinned exactly" "$p4" "$(cat "$work/first-bad")"
check "...two replays, no halving" "2" "$(replays)"

spec_summary
