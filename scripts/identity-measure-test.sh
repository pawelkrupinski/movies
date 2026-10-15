#!/usr/bin/env bash
# scripts/identity-measure against a stub `gh` and scratch repositories: the commit goes out on a temporary
# measure/ branch that is deleted whatever happens, the dispatch names the base and the takes-only rows, an old
# commit is measured under main's CI, and the downloaded takes artifacts become a LANDABLE / NOT LANDABLE verdict.
# Run: bash scripts/identity-measure-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"

printf '\033[36m▸\033[0m identity-measure\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
origin="$work/origin.git" repo="$work/repo"
scratch_repo "$origin" --bare -b main
scratch_repo "$repo" -b main
scratch_git "$repo" remote add origin "$origin"

tab=$'\t'
mkdir -p "$repo/.github/workflows" "$repo/scripts/ci" "$repo/test/resources/fixtures"
printf 'country\tvenue\trawTitle\tfilm\tverdict\tstate\tscope\tfilmTitle\treason\tjudged\tevidence\n' \
  > "$repo/test/resources/fixtures/identity-judgements.tsv"
printf 'pl\t*\tLalka\ttmdb:1\tright\topen\t\t\t\t2026-10-14\t\n' >> "$repo/test/resources/fixtures/identity-judgements.tsv"
echo "phase: convergence" > "$repo/.github/workflows/country-convergence-leg.yml"
# What can change a leg's verdict, in the matches.sh dialect: the resolver (rule.txt, engine/**) and the judgements.
printf '# the paths\nrule.txt\nengine/**\n.github/scripts/convergence-setup.sh\ntest/resources/fixtures/identity-judgements.tsv\n' > "$repo/.github/convergence-paths.txt"
echo "old" > "$repo/scripts/ci/identity-takes-check.sh"
scratch_git "$repo" add -A && scratch_git "$repo" commit -qm "old main, before takes-only"
old=$(scratch_git "$repo" rev-parse HEAD)
# Large, as the real one is: a reader that stops at the first match must not read it as lacking takes-only.
{ echo "takes-only: {}"; for _ in $(seq 1 4000); do echo "# a line of the leg workflow, padding it past a pipe buffer"; done; } \
  > "$repo/.github/workflows/country-convergence-leg.yml"
echo "new" > "$repo/scripts/ci/identity-takes-check.sh"
scratch_git "$repo" commit -qam "main learns takes-only"
parent=$(scratch_git "$repo" rev-parse HEAD)
echo "rule" > "$repo/rule.txt" && scratch_git "$repo" add rule.txt && scratch_git "$repo" commit -qm "a resolver change"
tip=$(scratch_git "$repo" rev-parse HEAD)
scratch_git "$repo" push -q origin main

# One artifact: identity-takes-pl, with a right take ("Lalka") lost when $LOSE is set.
artifact() { # <dir> <lose?>
  local d="$1/identity-takes-pl"
  mkdir -p "$d/base" "$d/head"
  echo "identity takes (pl): lost ${2:-0}" > "$d/verdict.txt"
  echo "$parent" > "$d/base.txt"
  echo '{"key":"Helios|Lalka","venue":"Helios","rawTitle":"Lalka","film":1,"filmTitle":"Lalka (2026)","basis":"OwnMatch","explanation":[],"blocker":null,"suspect":[]}' > "$d/base/decisions-pl.jsonl"
  if [ -n "${2:-}" ]; then
    echo '{"key":"Helios|Lalka","venue":"Helios","rawTitle":"Lalka","film":null,"filmTitle":null,"basis":"none","explanation":[],"blocker":"no film","suspect":[]}' > "$d/head/decisions-pl.jsonl"
  else
    cp "$d/base/decisions-pl.jsonl" "$d/head/decisions-pl.jsonl"
  fi
  printf 'phase\tseconds\thow\nhead-resolve\t546\tresolved\nbase-resolve\t3\treused\n' > "$d/timings.tsv"
  for side in base head; do echo "status${tab}country${tab}venue${tab}rawTitle${tab}film${tab}title${tab}basis${tab}ids${tab}was${tab}explanation" > "$d/$side/ratchet-takes.tsv"; done
}
artifact "$work/landable"
artifact "$work/lost" 1

mkdir -p "$work/bin"
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
echo "$*" >> "$STUB/calls"
json=""; jq_filter=""; prev=""
for a in "$@"; do [ "$prev" = --json ] && json=$a; [ "$prev" = --jq ] && jq_filter=$a; prev=$a; done
case "$1 $2" in
  "workflow run") echo "$*" > "$STUB/dispatch"; [ -z "${DISPATCH_FAILS:-}" ] || exit 1 ;;
  "run list")
    if [[ " $* " == *" --branch "* ]]; then
      # the run exists only once the commit is on origin's measure/ branch: record what was pushed
      for f in "$ORIGIN"/refs/heads/measure/*; do [ -f "$f" ] && { basename "$f" > "$STUB/branch"; cat "$f" > "$STUB/pushed"; }; done
      [ -f "$STUB/pushed" ] && echo 777
    else echo "${LANE_BUSY:-0}"; fi ;;
  "run view")
    case "$json" in
      url) echo "https://example.test/runs/777" ;;
      status,conclusion) [ -n "${VIEW_FAILS:-}" ] && exit 1
                         [ -n "${VIEW_BLIPS:-}" ] && [ ! -f "$STUB/blipped" ] && { touch "$STUB/blipped"; exit 1; }
                         [ -n "${IN_PROGRESS:-}" ] && { touch "$STUB/watched"; printf 'in_progress\t\n'; exit 0; }
                         # CANCELS=n: the first n runs end cancelled, replaced in the lane by a newer dispatch
                         reads=$(( $(cat "$STUB/reads" 2>/dev/null || echo 0) + 1 )); echo "$reads" > "$STUB/reads"
                         [ "$reads" -le "${CANCELS:-0}" ] && { printf 'completed\tcancelled\n'; exit 0; }
                         printf 'completed\tsuccess\n' ;;
      createdAt,startedAt,updatedAt,jobs)
        # gh applies --jq itself: so does the stub, to what GitHub answers
        jq -r "$jq_filter" <<'JSON'
{"createdAt":"2026-10-15T06:42:54Z","startedAt":"2026-10-15T06:42:54Z","updatedAt":"2026-10-15T07:23:38Z",
 "jobs":[{"name":"preflight","startedAt":"2026-10-15T07:02:42Z","completedAt":"2026-10-15T07:02:47Z","conclusion":"success"},
         {"name":"poland / takes","startedAt":"2026-10-15T07:02:49Z","completedAt":"2026-10-15T07:12:31Z","conclusion":"failure"}]}
JSON
        ;;
      # RUN_PENDING=n: the run is still in progress for --run's first n reads of it
      status,headBranch,headSha) pending=$(( $(cat "$STUB/pending" 2>/dev/null || echo 0) + 1 )); echo "$pending" > "$STUB/pending"
                                 [ "$pending" -le "${RUN_PENDING:-0}" ] && { printf 'in_progress\tmeasure/abc-1\t%s\n' "$HEAD_SHA"; exit 0; }
                                 printf 'completed\tmeasure/abc-1\t%s\n' "$HEAD_SHA" ;;
      jobs) printf 'poland / takes\tfailure\n' ;;
    esac ;;
  "run download") while [ "$#" -gt 0 ]; do [ "$1" = --dir ] && dir=$2; shift; done; cp -R "$ARTIFACT/." "$dir/" ;;
  "run cancel") echo cancelled > "$STUB/cancelled"
                # A second signal while the script cleans up (a second Ctrl-C): gh's parent is `timeout`, its parent the script.
                [ -z "${SIGNAL_ON_CANCEL:-}" ] || kill -TERM "$(ps -o ppid= -p "$PPID" | tr -d ' ')" ;;
  *) exit 1 ;;
esac
STUB
chmod +x "$work/bin/gh"
# The real git, but a measure/ push that reports failure AFTER origin took it (a push timed out on its answer).
real_git=$(command -v git)
cat > "$work/bin/git" <<STUB
#!/usr/bin/env bash
if [ "\$1" = push ] && [ -n "\${PUSH_FAILS_AFTER:-}" ] && [[ " \$* " == *refs/heads/measure/* ]] && [[ " \$* " != *--delete* ]]; then
  "$real_git" "\$@"; exit 1
fi
exec "$real_git" "\$@"
STUB
chmod +x "$work/bin/git"

measure() { # <artifact> <args...>: exit code; output in $work/out
  local art=$1; shift
  rm -rf "$work/stub"; mkdir -p "$work/stub"
  [ -n "${KEEP_CACHE:-}" ] || rm -rf "$work/cache"
  ( cd "$repo" && STUB="$work/stub" ORIGIN="$origin" ARTIFACT="$work/$art" HEAD_SHA="${HEAD_SHA:-}" \
      KINOWO_IDENTITY_CACHE="$work/cache" IDENTITY_MEASURE_POLL=0 PATH="$work/bin:$PATH" \
      "$REPO_ROOT/scripts/identity-measure" "$@" > "$work/out" 2>&1 )
  echo $?
}
measure_branches() { ls "$origin/refs/heads/measure" 2>/dev/null | wc -l | tr -d ' '; }

check "a commit that moved nothing is LANDABLE" "0" "$(measure landable --countries pl)"
check "...pushed as itself on a measure/<short sha>-<random> branch" \
  "$tip" "$(cat "$work/stub/pushed")"
check "...named after the commit" "true" "$([[ "$(cat "$work/stub/branch")" == "${tip:0:9}-"* ]] && echo true || echo false)"
check "...dispatching only the takes rows against the commit's parent (it is on main already)" \
  "workflow run identity-model-convergence.yml --ref measure/$(cat "$work/stub/branch") -f takes-base=$parent -f takes-only=true" \
  "$(cat "$work/stub/dispatch")"
check "...and the branch is deleted afterwards" "0" "$(measure_branches)"
check "...printing the verdict" "true" "$(grep -q '^LANDABLE' "$work/out" && echo true || echo false)"
check "...and the takes row's own conclusion" "true" "$(grep -Eq '^pl +model .*failure' "$work/out" && echo true || echo false)"
check "...and where its time went: the wait for runners, from the first job's start, not the run's" "true" \
  "$(grep -q 'waited for runners 1188s, then ran 1256s' "$work/out" && echo true || echo false)"
check "...each takes row's job" "true" "$(grep -q 'poland / takes: 582s failure' "$work/out" && echo true || echo false)"
check "...and each row's phases, saying which a stored result spared" "true" \
  "$(grep -q 'pl phases: head-resolve 546s (resolved), base-resolve 3s (reused)' "$work/out" && echo true || echo false)"

check "a run cancelled more than twice is dispatched again until it runs" "0" "$(CANCELS=3 measure landable --countries pl)"
check "...four dispatches in all" "true" "$(grep -q 'dispatching (attempt 4)' "$work/out" && echo true || echo false)"
check "a measurement does not wait behind another run of the workflow: it has a lane of its own" "0" \
  "$(LANE_BUSY=1 measure landable --timeout 0 --countries pl)"

check "a right take lost is NOT LANDABLE" "1" "$(measure lost --base "$parent" --countries pl)"
check "...naming the bucket" "true" "$(grep -q 'pl model: 1 right-lost' "$work/out" && echo true || echo false)"
check "...with the listing, before -> after" "true" \
  "$(grep -q 'Helios | Lalka: tmdb:1 Lalka (2026) -> none' "$work/out" && echo true || echo false)"
check "...and the branch is deleted" "0" "$(measure_branches)"

check "a commit from before the takes-only dispatch is measured" "0" "$(measure landable "$old" --base "$old" --countries pl)"
pushed=$(cat "$work/stub/pushed")
check "...under a commit laying main's CI over it, whose parent is the commit" \
  "$old" "$(scratch_git "$repo" rev-parse "$pushed^")"
check "...with main's workflows" "takes-only: {}" "$(scratch_git "$repo" show "$pushed:.github/workflows/country-convergence-leg.yml" | sed -n 1p)"
check "...and main's scripts/ci" "new" "$(scratch_git "$repo" show "$pushed:scripts/ci/identity-takes-check.sh")"
check "...and nothing else of main's" "false" \
  "$(scratch_git "$repo" cat-file -e "$pushed:rule.txt" 2>/dev/null && echo true || echo false)"

check "a run read that fails once is retried, not the end of the measurement" "0" "$(VIEW_BLIPS=1 measure landable --countries pl)"
check "--country, the other tools' spelling, is the same flag" "0" "$(VIEW_BLIPS=1 measure landable --country pl)"
check "a country no tool knows is a usage error" "2" "$(measure landable --countries pl,xx)"
check "a run that cannot be read is no measurement" "2" "$(VIEW_FAILS=1 measure landable --countries pl)"
check "...and its branch is still deleted" "0" "$(measure_branches)"

check "--no-wait dispatches and keeps the branch for --run" "0" "$(measure landable --no-wait --countries pl)"
check "...which stays on origin" "1" "$(measure_branches)"
check "...saying how to collect it" "true" "$(grep -q 'scripts/identity-measure --run 777' "$work/out" && echo true || echo false)"
scratch_git "$repo" push -q origin --delete "refs/heads/measure/$(cat "$work/stub/branch")"

scratch_git "$repo" push -q origin "$tip:refs/heads/measure/abc-1"
check "--run collects a finished run" "1" "$(HEAD_SHA=$tip measure lost --run 777 --countries pl)"
check "...and deletes its measure/ branch" "0" "$(measure_branches)"
check "...dispatching nothing" "false" "$([ -f "$work/stub/dispatch" ] && echo true || echo false)"
# --run of a run still in progress waits for it (bounded by --timeout), then collects: no poll loop of the caller's
rm -f "$work/stub/pending"; scratch_git "$repo" push -q origin "$tip:refs/heads/measure/abc-1"
check "--run waits for a run still in progress, then collects it" "1" "$(RUN_PENDING=1 HEAD_SHA=$tip measure lost --run 777 --countries pl)"
check "...saying it waits" "true" "$(grep -q 'waiting for run 777' "$work/out" && echo true || echo false)"
rm -f "$work/stub/pending"; scratch_git "$repo" push -q origin "$tip:refs/heads/measure/abc-1"
check "--run --no-wait leaves a run in progress for later" "2" "$(RUN_PENDING=1 HEAD_SHA=$tip measure lost --run 777 --no-wait --countries pl)"
scratch_git "$repo" push -q origin --delete refs/heads/measure/abc-1
# --run re-classifies with the checkout's judgements, made since the run and not yet committed: Lalka judged wrong
# since, its loss is a cure.
cp "$repo/test/resources/fixtures/identity-judgements.tsv" "$work/judgements.orig"
sed -i.bak 's/\tright\topen\t/\twrong\topen\t/' "$repo/test/resources/fixtures/identity-judgements.tsv"
scratch_git "$repo" push -q origin "$tip:refs/heads/measure/abc-1"
check "--run classifies with the checkout's newer judgements" "0" "$(HEAD_SHA=$tip measure lost --run 777 --countries pl)"
cp "$work/judgements.orig" "$repo/test/resources/fixtures/identity-judgements.tsv"; rm -f "$repo/test/resources/fixtures/identity-judgements.tsv.bak"
check "...classifying in a scratch directory of its own: another agent's --run of the run classifies with its own" "0" \
  "$(find "$work/cache/measure/777" -maxdepth 1 \( -name rows.json -o -name identity-judgements.tsv \) | wc -l | tr -d ' ')"
# The shared cache's download of a run is made under a lock: another agent's live download of it is waited for.
sleep 30 & holder=$!
mkdir -p "$work/cache/locks/measure-777"; echo "$holder" > "$work/cache/locks/measure-777/pid"; rm -f "$work/cache/measure/777/downloaded"
scratch_git "$repo" push -q origin "$tip:refs/heads/measure/abc-1"
check "a run another agent is downloading into the cache is waited for, not downloaded beside it" "2 0" \
  "$(HEAD_SHA=$tip KEEP_CACHE=1 KINOWO_IDENTITY_LOCK_WAIT=2 measure lost --run 777 --countries pl) $(grep -c '^run download' "$work/stub/calls")"
kill "$holder" 2> /dev/null; wait "$holder" 2> /dev/null

# THE BRANCH ON EVERY EXIT PATH: whatever ends a measurement after the push, origin keeps no measure/ branch.
check "a dispatch refused after the push is no measurement" "2" "$(DISPATCH_FAILS=1 measure landable --countries pl)"
check "...and its branch is deleted" "0" "$(measure_branches)"
check "a run past --timeout is no measurement" "2" "$(IN_PROGRESS=1 measure landable --timeout 0 --countries pl)"
check "...its run is cancelled" "cancelled" "$(cat "$work/stub/cancelled" 2>/dev/null)"
check "...and its branch deleted" "0" "$(measure_branches)"
check "a second signal while cleaning up does not stop the clean-up" "0" \
  "$(IN_PROGRESS=1 SIGNAL_ON_CANCEL=1 measure landable --timeout 0 --countries pl > /dev/null; measure_branches)"
check "a push that failed after origin took the branch still deletes it" "2" \
  "$(PUSH_FAILS_AFTER=1 measure landable --countries pl)"
check "...leaving no branch" "0" "$(measure_branches)"
for f in "$origin"/refs/heads/measure/*; do
  [ -f "$f" ] && scratch_git "$repo" push -q origin --delete "refs/heads/measure/$(basename "$f")"
done

# REUSE: a branch measured once, then rebased onto a main that moved, is collected again -- not dispatched -- when the
# change is the same and main moved outside the convergence paths.
dispatched() { [ -f "$work/stub/dispatch" ] && echo true || echo false; }
on_main() { # <file> <content>: a commit on origin's main
  scratch_git "$repo" checkout -q main && mkdir -p "$(dirname "$repo/$1")" && echo "$2" > "$repo/$1" && scratch_git "$repo" add "$1" \
    && scratch_git "$repo" commit -qm "main: $1" && scratch_git "$repo" push -q origin main
}
branch_onto_main() { # <rule content>: the one-commit feature branch, (re)made on main's tip
  scratch_git "$repo" checkout -q -B feature main && echo "$1" > "$repo/rule.txt" \
    && scratch_git "$repo" commit -qam "feature: a resolver change"
}
rm -rf "$work/cache"
branch_onto_main "rule v2"
check "a first measurement of a change dispatches" "0" "$(KEEP_CACHE=1 measure landable --countries pl)"
check "...a run" "true" "$(dispatched)"
on_main docs.txt "a doc"; branch_onto_main "rule v2"
check "the change rebased onto a main moved outside the convergence paths is answered" "0" \
  "$(KEEP_CACHE=1 measure landable --countries pl)"
check "...without a dispatch" "false" "$(dispatched)"
check "...or a measure/ branch" "0" "$(measure_branches)"
check "...saying which run it reused, and why" "true" \
  "$(grep -q 'REUSED measurement of run 777 (base [0-9a-f]*, same change, main moved only outside the paths that move a take)' "$work/out" && echo true || echo false)"
check "...with the same verdict" "true" "$(grep -q '^LANDABLE' "$work/out" && echo true || echo false)"
check "--fresh dispatches all the same" "0" "$(KEEP_CACHE=1 measure landable --fresh --countries pl)"
check "...a run" "true" "$(dispatched)"
# Main judges Lalka wrong since: the judgements move no take, so the run is reused (collecting classifies with the head's).
sed -i.bak 's/\tright\topen\t/\twrong\topen\t/' "$repo/test/resources/fixtures/identity-judgements.tsv"
rm -f "$repo/test/resources/fixtures/identity-judgements.tsv.bak"
scratch_git "$repo" checkout -q main && scratch_git "$repo" commit -qam "main: Lalka judged wrong" && scratch_git "$repo" push -q origin main
branch_onto_main "rule v2"
check "main moved only its judgements: reused" "false" "$(KEEP_CACHE=1 measure landable --countries pl > /dev/null; dispatched)"
on_main .github/scripts/convergence-setup.sh "a leg's plumbing"; branch_onto_main "rule v2"
check "main moved only a leg's CI plumbing (a convergence path that moves no take): reused" "false" \
  "$(KEEP_CACHE=1 measure landable --countries pl > /dev/null; dispatched)"
on_main engine/match.txt "a resolver change on main"; branch_onto_main "rule v2"
check "main moved inside the convergence paths: dispatched" "true" "$(KEEP_CACHE=1 measure landable --countries pl > /dev/null; dispatched)"
branch_onto_main "rule v3"
check "the change itself changed: dispatched" "true" "$(KEEP_CACHE=1 measure landable --countries pl > /dev/null; dispatched)"
scratch_git "$repo" checkout -q main
# A run collected with --run (after --no-wait) is stored too: its base from the artifact, its head the run's commit.
rm -rf "$work/cache"
scratch_git "$repo" push -q origin "$tip:refs/heads/measure/abc-1"
KEEP_CACHE=1 HEAD_SHA=$tip measure lost --run 777 --countries pl > /dev/null
check "a run collected with --run answers its change later" "1" "$(KEEP_CACHE=1 measure landable "$tip" --base "$parent" --countries pl)"
check "...without a dispatch" "false" "$(dispatched)"
check "...the same base" "true" "$(grep -q 'REUSED measurement of run 777 (base [0-9a-f]*, same change, the same base)' "$work/out" && echo true || echo false)"

# SIGTERM (Ctrl-C alike) while waiting on the run: the run is cancelled and the branch deleted.
rm -rf "$work/stub" "$work/cache"; mkdir -p "$work/stub"
( cd "$repo" && STUB="$work/stub" ORIGIN="$origin" ARTIFACT="$work/landable" IN_PROGRESS=1 \
    KINOWO_IDENTITY_CACHE="$work/cache" IDENTITY_MEASURE_POLL=1 PATH="$work/bin:$PATH" \
    exec "$REPO_ROOT/scripts/identity-measure" --countries pl > "$work/out" 2>&1 ) &
pid=$!
for _ in $(seq 1 100); do [ -f "$work/stub/watched" ] && break; sleep 0.1; done
# A watchdog: a script that defers the signal (a wait inside a $(...) subshell does) must fail this, not hang it.
( sleep 20; kill -KILL "$pid" 2> /dev/null ) & watchdog=$!
kill -TERM "$pid"; wait "$pid"; status=$?
kill "$watchdog" 2> /dev/null
check "SIGTERM while waiting on the run exits 130" "130" "$status"
check "...cancelling the run" "cancelled" "$(cat "$work/stub/cancelled" 2>/dev/null)"
check "...and deleting the branch" "0" "$(measure_branches)"

spec_summary
