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
printf '# the paths\nrule.txt\nengine/**\ncommon/**\n.github/scripts/convergence-setup.sh\ntest/resources/fixtures/identity-judgements.tsv\n' > "$repo/.github/convergence-paths.txt"
echo "old" > "$repo/scripts/ci/identity-takes-check.sh"
scratch_git "$repo" add -A && scratch_git "$repo" commit -qm "old main, before takes-only"
old=$(scratch_git "$repo" rev-parse HEAD)
# Large, as the real one is: a reader that stops at the first match must not read it as lacking takes-only.
{ echo "takes-only: {} takes-countries: {}"; for _ in $(seq 1 4000); do echo "# a line of the leg workflow, padding it past a pipe buffer"; done; } \
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
    if [[ " $* " == *" status,createdAt "* ]]; then
      # the sweep's read of a branch's newest run: "<status> <createdAt>" from $SWEEP_DIR/<branch>, none for a run-less branch
      b=""; prev=""; for a in "$@"; do [ "$prev" = --branch ] && b=$a; prev=$a; done
      [ -f "$SWEEP_DIR/${b//\//_}" ] && cat "$SWEEP_DIR/${b//\//_}"; exit 0
    elif [[ " $* " == *" status,conclusion,createdAt,updatedAt,headBranch "* ]]; then
      # --wait's ETA: the workflow's recent runs, $ETA_RUNS (none by default), through the caller's --jq
      jq -r "$jq_filter" <<< "${ETA_RUNS:-[]}"
    elif [[ " $* " == *" --branch "* ]]; then
      # the run exists only once the commit is on origin's measure/ branch: record what was pushed
      while IFS= read -r f; do { echo "${f#"$ORIGIN"/refs/heads/measure/}" > "$STUB/branch"; cat "$f" > "$STUB/pushed"; }; done \
        < <(find "$ORIGIN/refs/heads/measure" -type f 2>/dev/null)
      [ -f "$STUB/pushed" ] && echo 777
    else echo "${LANE_BUSY:-0}"; fi ;;
  "run view")
    case "$json" in
      url) echo "https://example.test/runs/777" ;;
      # scripts/ci-wait's read: the whole run object, as GitHub answers it
      *status,conclusion*) [ -n "${VIEW_FAILS:-}" ] && exit 1
                         [ -n "${VIEW_BLIPS:-}" ] && [ ! -f "$STUB/blipped" ] && { touch "$STUB/blipped"; exit 1; }
                         view() { printf '{"databaseId":777,"headSha":"%s","headBranch":"measure/abc-1","workflowName":"w","status":"%s","conclusion":"%s"}\n' \
                                    "${HEAD_SHA:-abc1230000000000000000000000000000000000}" "$1" "${2:-}"; }
                         [ -n "${IN_PROGRESS:-}" ] && { touch "$STUB/watched"; view in_progress; exit 0; }
                         # VIEW_PENDING=n: the run is in progress for the first n of these reads
                         vp=$(( $(cat "$STUB/view-pending" 2>/dev/null || echo 0) + 1 )); echo "$vp" > "$STUB/view-pending"
                         [ "$vp" -le "${VIEW_PENDING:-0}" ] && { view in_progress; exit 0; }
                         # CANCELS=n: the first n runs end cancelled, replaced in the lane by a newer dispatch
                         reads=$(( $(cat "$STUB/reads" 2>/dev/null || echo 0) + 1 )); echo "$reads" > "$STUB/reads"
                         [ "$reads" -le "${CANCELS:-0}" ] && { view completed cancelled; exit 0; }
                         view completed success ;;
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
      # the takes rows' state (wait_for_takes asks .status), else their conclusions (collect)
      jobs) if [[ "$jq_filter" == *.status* ]]; then printf 'poland / takes\t%s\n' "${TAKES_STATUS:-in_progress}"
            else printf 'poland / takes\tfailure\n'; fi ;;
      createdAt) echo "${RUN_CREATED:-$(date -u +%Y-%m-%dT%H:%M:%SZ)}" ;;
    esac ;;
  "run download") [ -n "${DOWNLOAD_FAILS:-}" ] && { echo "HTTP 502: Bad Gateway" >&2; exit 1; }
                  [ -n "${DOWNLOAD_BLIPS:-}" ] && [ ! -f "$STUB/download-blipped" ] && { touch "$STUB/download-blipped"; echo "HTTP 500" >&2; exit 1; }
                  while [ "$#" -gt 0 ]; do [ "$1" = --dir ] && dir=$2; shift; done; cp -R "$ARTIFACT/." "$dir/" ;;
  # the footprint measure's traces: the release's assets as its --jq prints them ("<name> <size>"), and one downloaded
  "release view") [ -n "${FOOTPRINT_ASSETS:-}" ] || exit 1; printf '%s\n' "$FOOTPRINT_ASSETS" ;;
  # THE RESULT CACHE's assets: kept in $RELEASE_DIR when a case names one, else no release takes or holds them
  "release upload") [ -n "${RELEASE_DIR:-}" ] || exit 1; cp "$4" "$RELEASE_DIR/" ;;
  "release download") o=""; p=""; prev=""; for a in "$@"; do [ "$prev" = -O ] && o=$a; [ "$prev" = -p ] && p=$a; prev=$a; done
                      if [[ "$p" == measure-result-* ]]; then [ -n "${RELEASE_DIR:-}" ] && [ -f "$RELEASE_DIR/$p" ] || exit 1
                        cp "$RELEASE_DIR/$p" "$o"; exit 0; fi
                      echo '{"m":"sanitize"}' | gzip | gzip > "$o" ;;
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
      KINOWO_IDENTITY_CACHE="$work/cache" IDENTITY_MEASURE_POLL=0 GH_DOWNLOAD_BACKOFF=0 PATH="$work/bin:$PATH" \
      "$REPO_ROOT/scripts/identity-measure" "$@" > "$work/out" 2>&1 )
  echo $?
}
measure_branches() { find "$origin/refs/heads/measure" -type f 2>/dev/null | wc -l | tr -d ' '; }

check "a commit that moved nothing is LANDABLE" "0" "$(measure landable --countries pl)"
check "...pushed as itself on a measure/<user>/<short sha>-<random> branch" \
  "$tip" "$(cat "$work/stub/pushed")"
check "...named after the commit" "true" "$([[ "$(cat "$work/stub/branch")" == "$(id -un)/${tip:0:9}-"* ]] && echo true || echo false)"
check "...dispatching only the takes rows against the commit's parent (it is on main already)" \
  "workflow run identity-model-convergence.yml --ref measure/$(cat "$work/stub/branch") -f takes-base=$parent -f takes-only=true -f takes-countries=pl" \
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
check "...with main's workflows" "takes-only: {} takes-countries: {}" "$(scratch_git "$repo" show "$pushed:.github/workflows/country-convergence-leg.yml" | sed -n 1p)"
check "...and main's scripts/ci" "new" "$(scratch_git "$repo" show "$pushed:scripts/ci/identity-takes-check.sh")"
check "...and nothing else of main's" "false" \
  "$(scratch_git "$repo" cat-file -e "$pushed:rule.txt" 2>/dev/null && echo true || echo false)"

check "a run read that fails once is retried, not the end of the measurement" "0" "$(VIEW_BLIPS=1 measure landable --countries pl)"
check "--country, the other tools' spelling, is the same flag" "0" "$(VIEW_BLIPS=1 measure landable --country pl)"
check "--head names the commit, --recording the run every leg replays" "0" \
  "$(measure landable --head "$tip" --recording 528 --countries pl)"
check "...dispatched as the workflow's recording input" "true" \
  "$(grep -q -- '-f takes-only=true -f recording=528 -f takes-countries=pl' "$work/stub/dispatch" && echo true || echo false)"
check "...and never answered by an earlier measurement over the pinned pair" "0|true" \
  "$(KEEP_CACHE=1 measure landable --countries pl >/dev/null; KEEP_CACHE=1 measure landable --recording 528 --countries pl)|$([ -f "$work/stub/dispatch" ] && echo true || echo false)"
check "--tier names the run's ci-runner tier (identity-landing-check's check tier)" "0|true" \
  "$(measure landable --countries pl --fresh --tier ubuntu-24.04)|$(grep -q -- '-f takes-countries=pl -f tier=ubuntu-24.04$' "$work/stub/dispatch" && echo true || echo false)"
check "a recording is a run id" "2" "$(measure landable --recording latest)"
check "a country no tool knows is a usage error" "2" "$(measure landable --countries pl,xx)"
check "a takes download that fails once is asked again, not the end of the measurement" "0" "$(DOWNLOAD_BLIPS=1 measure landable --countries pl)"
check "a takes download that keeps failing is no measurement, not a run without takes" "2 1" \
  "$(DOWNLOAD_FAILS=1 measure landable --countries pl) $(grep -c "could not download run 777's identity-takes" "$work/out")"
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
rm -f "$work/stub/pending"
check "--run gives up on a run still in progress at --timeout" "2" "$(RUN_PENDING=1 IN_PROGRESS=1 HEAD_SHA=$tip measure lost --run 777 --timeout 0 --countries pl)"
check "...saying so" "true" "$(grep -q 'gave no conclusion within --timeout 0 minutes' "$work/out" && echo true || echo false)"
check "...keeping the branch of the run still going" "1" "$(measure_branches)"
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
mkdir -p "$work/cache/locks"; spec_lock_holder "$work/cache/locks/measure-777.flock"; holder=$SPEC_LOCK_HOLDER
rm -f "$work/cache/measure/777/downloaded"
scratch_git "$repo" push -q origin "$tip:refs/heads/measure/abc-1"
check "a run another agent is downloading into the cache is waited for, not downloaded beside it" "2 0" \
  "$(HEAD_SHA=$tip KEEP_CACHE=1 KINOWO_IDENTITY_LOCK_WAIT=2 measure lost --run 777 --countries pl) $(grep -c '^run download' "$work/stub/calls")"
kill "$holder" 2> /dev/null; wait "$holder" 2> /dev/null

# THE BRANCH ON EVERY EXIT PATH: whatever ends a measurement after the push, origin keeps no measure/ branch.
check "a dispatch refused after the push is no measurement" "2" "$(DISPATCH_FAILS=1 measure landable --countries pl)"
check "...and its branch is deleted" "0" "$(measure_branches)"
check "a run past --timeout is no measurement" "2" "$(IN_PROGRESS=1 measure landable --timeout 0 --countries pl)"
check "...even collecting early, while its takes rows still run" "2" \
  "$(IN_PROGRESS=1 IDENTITY_MEASURE_EARLY_COLLECT=1 measure landable --timeout 0 --countries pl)"
check "a run whose takes rows have ended is collected without waiting for the run's tail" "1" \
  "$(IN_PROGRESS=1 TAKES_STATUS=completed IDENTITY_MEASURE_EARLY_COLLECT=1 measure lost --timeout 0 --countries pl)"
check "...and says so" "1" "$(grep -c "every takes row has ended" "$work/out")"
check "...but not where this script, not the run, deletes the branch (github.com)" "2" \
  "$(IN_PROGRESS=1 TAKES_STATUS=completed IDENTITY_MEASURE_EARLY_COLLECT=0 measure lost --timeout 0 --countries pl)"
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
# The patch-id ignores whitespace: a change differing only in it has the same id, but not the same head.
branch_onto_main "rule  v3"
check "a change differing only in whitespace (the same patch-id): dispatched" "true" \
  "$(KEEP_CACHE=1 measure landable --countries pl > /dev/null; dispatched)"
# What the takes check reads that no convergence path names still moves a take.
on_main test/resources/fixtures/identity-unmatched/pl.inputs "a re-captured family"; branch_onto_main "rule  v3"
check "main moved the captures the final takes replay: dispatched" "true" \
  "$(KEEP_CACHE=1 measure landable --countries pl > /dev/null; dispatched)"
on_main worker/src/it/scala/IdentityResolveDump.scala "the resolve dump"; branch_onto_main "rule  v3"
check "main moved the takes check's resolve dump: dispatched" "true" \
  "$(KEEP_CACHE=1 measure landable --countries pl > /dev/null; dispatched)"
on_main test/resources/fixtures/identity-unmatched/expected-matches.tsv "the ratchet's output"; branch_onto_main "rule  v3"
check "main moved only the ratchet's expected-matches.tsv: reused" "false" \
  "$(KEEP_CACHE=1 measure landable --countries pl > /dev/null; dispatched)"
scratch_git "$repo" checkout -q main
# A run collected with --run (after --no-wait) is stored too: its base from the artifact, its head the run's commit.
rm -rf "$work/cache"
scratch_git "$repo" push -q origin "$tip:refs/heads/measure/abc-1"
KEEP_CACHE=1 HEAD_SHA=$tip measure lost --run 777 --countries pl > /dev/null
check "a run collected with --run answers its change later" "1" "$(KEEP_CACHE=1 measure landable "$tip" --base "$parent" --countries pl)"
check "...without a dispatch" "false" "$(dispatched)"
check "...the same base" "true" "$(grep -q 'REUSED measurement of run 777 (base [0-9a-f]*, same change, the same base)' "$work/out" && echo true || echo false)"
# ...for 24 hours from when the run was made, not from when it was collected.
rm -rf "$work/cache"
scratch_git "$repo" push -q origin "$tip:refs/heads/measure/abc-1"
KEEP_CACHE=1 HEAD_SHA=$tip RUN_CREATED=2026-01-01T00:00:00Z measure lost --run 777 --countries pl > /dev/null
check "a run made days before it was collected with --run is not reused" "true" \
  "$(KEEP_CACHE=1 measure landable "$tip" --base "$parent" --countries pl > /dev/null; dispatched)"

# The take-moving paths cover everything the real takes check reads: its identity paths, the specs it runs, the captures.
take_patterns="$work/take-paths"
# (reads the checkout's own HEAD: a hook's GIT_DIR is not inherited)
( cd "$REPO_ROOT" && env -u GIT_DIR -u GIT_WORK_TREE -u GIT_INDEX_FILE "$REPO_ROOT/scripts/identity-measure" --take-paths ) > "$take_patterns"
takes_check="$REPO_ROOT/scripts/ci/identity-takes-check.sh"
read_paths=$(
  sed -n '/^identity_paths=(/,/)/p' "$takes_check" | sed -e 's/^identity_paths=(//' -e 's/)$//' | tr ' ' '\n' | grep -v '^$'
  grep -oE 'testOnly [a-z.]*[A-Z][A-Za-z]*' "$takes_check" | sed 's/.* //; s/.*\.//' | while read -r class; do
    ( cd "$REPO_ROOT" && find ./*/src -name "$class.scala" | sed 's|^\./||' ); done
  echo test/resources/fixtures/identity-unmatched/pl.json.gz)
uncovered=""
for p in $read_paths; do
  [ -d "$REPO_ROOT/$p" ] && p="$p/x.scala"
  [ "$(printf '%s\n' "$p" | "$REPO_ROOT/.github/actions/changed-paths/matches.sh" "$take_patterns")" = true ] || uncovered="$uncovered $p"
done
check "every path the takes check reads is one a reuse treats as moving a take" "" "$uncovered"
check "...read from the takes check (identity_paths and the specs it runs)" "true" \
  "$(grep -q UnmatchedClustersRatchetSpec <<<"$read_paths" && grep -q '^identity/src/main$' <<<"$read_paths" && echo true || echo false)"

# THE FOOTPRINT: without --countries only the countries a change can move are measured; THE RECORDINGS: one by default.
fp_commit() { # <file> <content> -> a commit on top of main changing only it
  scratch_git "$repo" checkout -q --detach main
  mkdir -p "$repo/$(dirname "$1")"; printf '%b' "$2" > "$repo/$1"
  scratch_git "$repo" add -A && scratch_git "$repo" commit -qm "footprint: $1" && scratch_git "$repo" rev-parse HEAD
}
plan() { ( cd "$repo" && KINOWO_IDENTITY_CACHE="$work/cache" PATH="$work/bin:$PATH" IDENTITY_MEASURE_RECORDINGS="528:pl,de 999" \
  "$REPO_ROOT/scripts/identity-measure" --plan "$@" 2>/dev/null ); }
main_tip=$(scratch_git "$repo" rev-parse main)
de_only=$(fp_commit engine/services/cinemas/de/Kinoheld.scala "x")
check "a change under services/cinemas/de/ measures Germany alone" "footprint de" "$(plan --head "$de_only" --base "$main_tip" | head -1)"
check "...over one recording, the rest only for a cross-country move" \
  "measure pinned de|then, when LANDABLE and 2+ countries moved: 528 pl,de;999 pl,de,uk,us,es" \
  "$(plan --head "$de_only" --base "$main_tip" | tail -2 | tr '\n' '|' | sed 's/|$//')"
check "...every recording at once with --all-recordings, each over the countries it holds" \
  "measure pinned de|measure 528 de|measure 999 de" "$(plan --head "$de_only" --base "$main_tip" --all-recordings | tail -3 | tr '\n' '|' | sed 's/|$//')"
shared=$(fp_commit engine/core.scala "x")
check "a shared resolver change measures every country" "footprint pl,de,uk,us,es" "$(plan --head "$shared" --base "$main_tip" | head -1)"
capture=$(fp_commit test/resources/fixtures/identity-unmatched/uk.inputs "x")
check "a country's capture reaches that country" "footprint uk" "$(plan --head "$capture" --base "$main_tip" | head -1)"
words=$(fp_commit common/src/main/resources/title-words.tsv "# header\\nanniversary\\ten\\tanniversary\\nanniversary\\tes\\taniversario\\n")
check "title-words rows reach their languages' countries (en: uk and us)" "footprint uk,us,es" "$(plan --head "$words" --base "$main_tip" | head -1)"
check "--countries wins over the footprint" "footprint pl" "$(plan --head "$de_only" --base "$main_tip" --countries pl | head -1)"
check "--all-countries ignores it" "footprint pl,de,uk,us,es" "$(plan --head "$de_only" --base "$main_tip" --all-countries | head -1)"
docs=$(fp_commit docs/notes.md "x")
check "a change no take-moving path holds measures every country, as before" "footprint pl,de,uk,us,es" "$(plan --head "$docs" --base "$main_tip" | head -1)"
scratch_git "$repo" checkout -q main

# THE FOOTPRINT MEASURE: a title-rule change whose every traced reading reads alike is answered on the laptop, no dispatch;
# anything else says why and measures in full.
mkdir -p "$repo/common/src/main/resources"
echo "rule a" > "$repo/common/src/main/resources/title-rules.tsv"
scratch_git "$repo" add -A && scratch_git "$repo" commit -qm "rules before"
rules_base=$(scratch_git "$repo" rev-parse HEAD)
echo "rule b" > "$repo/common/src/main/resources/title-rules.tsv"
scratch_git "$repo" commit -qam "a title-rule change"
rules_head=$(scratch_git "$repo" rev-parse HEAD)
read -ra id_paths <<< "$(sed -n '/^identity_paths=(/,/)/p' "$takes_check" | sed -e 's/^identity_paths=(//' -e 's/)$//' | tr '\n' ' ')"
code=$(scratch_git "$repo" ls-tree -r "$rules_base" -- "${id_paths[@]}" | { if command -v sha256sum > /dev/null; then sha256sum; else shasum -a 256; fi; } | cut -c1-24)
cat > "$work/bin/trace-check" <<'CHECK'
#!/usr/bin/env bash
echo "[normalizer-trace] 12 reading(s) made again: ${TRACE_DIFFER:-0} read otherwise, 0 could not be made again"
[ "${TRACE_DIFFER:-0}" = 0 ] && exit 0
printf 'pl\tsanitize\tFestive Fun with Peppa: Cinema Experience\tfestivefunwithpeppacinemaexperience\tfestivefunwithpeppa\n' > "$2"; exit 1
CHECK
chmod +x "$work/bin/trace-check"
# shellcheck disable=SC2120 # flags of a case, when it needs any
footprint() { FOOTPRINT_ASSETS=${ASSETS-"takes-trace-pl-$code-p573.gz 99"} IDENTITY_MEASURE_TRACE_CHECK="$work/bin/trace-check" \
  measure landable --head "$rules_head" --base "$rules_base" --countries pl --footprint "$@"; }
check "--footprint: a title-rule change reading every traced title alike is LANDABLE without a dispatch" "0" "$(footprint)"
check "...measured as a footprint" "true" "$(grep -q '^measure: footprint' "$work/out" && echo true || echo false)"
check "...naming the trace it replayed" "true" "$(grep -q "footprint: pl's base readings: takes-trace-pl-$code-p573.gz" "$work/out" && echo true || echo false)"
check "...and saying the full measure is still the gate it lands on" "true" \
  "$(grep -q '^LANDABLE (footprint): no take can move -- before landing, the full measure' "$work/out" && echo true || echo false)"
check "...dispatching nothing" "false" "$([ -f "$work/stub/dispatch" ] && echo true || echo false)"
check "--footprint: a title read otherwise is listed, and the full measure decides" "0" "$(TRACE_DIFFER=1 footprint)"
check "...listing the reading" "true" "$(grep -q 'Festive Fun with Peppa: Cinema Experience' "$work/out" && echo true || echo false)"
check "...then measuring in full" "true|true" \
  "$(grep -q '^measure: full' "$work/out" && echo true || echo false)|$([ -f "$work/stub/dispatch" ] && echo true || echo false)"
check "--footprint: no trace of the base's identity code over the recording measures in full" "true|true" \
  "$(ASSETS="takes-trace-pl-000000000000000000000000-p573.gz 99" footprint > /dev/null; grep -q 'no trace of pl at the base' "$work/out" && echo true || echo false)|$(
     grep -q '^measure: full' "$work/out" && echo true || echo false)"
check "--footprint: a change outside the title rules says so and measures in full" "true|true" \
  "$(FOOTPRINT_ASSETS="takes-trace-pl-$code-p573.gz 99" IDENTITY_MEASURE_TRACE_CHECK="$work/bin/trace-check" measure landable --head "$tip" --countries pl --footprint > /dev/null
     grep -q 'footprint: rule.txt can move a take outside the title rules' "$work/out" && echo true || echo false)|$(
     grep -q '^measure: full' "$work/out" && echo true || echo false)"

# THE SWEEP: orphaned measure/ branches (a SIGKILLed measurement leaves one) are deleted by --sweep alone; a measurement never sweeps.
rm -rf "$work/stub"; mkdir -p "$work/stub" "$work/sweep"; export SWEEP_DIR="$work/sweep"
old_run="2020-01-01T00:00:00Z" new_run=$(date -u +%Y-%m-%dT%H:%M:%SZ)
for b in dead-finished live-running fresh-finished runless; do
  scratch_git "$repo" push -q origin "$tip:refs/heads/measure/$b"
done
echo "completed $old_run" > "$work/sweep/measure_dead-finished"
echo "in_progress $old_run" > "$work/sweep/measure_live-running"
echo "completed $new_run" > "$work/sweep/measure_fresh-finished"
sweep_remaining() { scratch_git "$origin" for-each-ref --format='%(refname:short)' refs/heads/measure | sort | tr '\n' ' '; }
( cd "$repo" && STUB="$work/stub" ORIGIN="$origin" KINOWO_IDENTITY_CACHE="$work/cache" PATH="$work/bin:$PATH" \
    "$REPO_ROOT/scripts/identity-measure" --sweep > "$work/out" 2>&1 ); status=$?
check "--sweep exits 0" "0" "$status"
check "...deleting only the branch whose run finished long ago" \
  "measure/fresh-finished measure/live-running measure/runless " "$(sweep_remaining)"
check "...and saying so" "true" "$(grep -q 'sweep: deleted orphaned measure/dead-finished' "$work/out" && echo true || echo false)"
echo "completed $old_run" > "$work/sweep/measure_fresh-finished"
echo "completed $old_run" > "$work/sweep/measure_live-running"
rm -f "$work/stub/dispatch" "$work/stub/pushed"
check "a measurement never sweeps: it deletes remote branches, so only --sweep does" "0" "$(measure landable --countries pl)"
check "...leaving every branch" "measure/fresh-finished measure/live-running measure/runless " "$(sweep_remaining)"
for b in runless fresh-finished live-running; do scratch_git "$origin" update-ref -d "refs/heads/measure/$b"; done
rm -rf "$work/sweep"

# THE RESULT CACHE: a verdict stored on the release answers the same request again, from any clone, in seconds.
export RELEASE_DIR="$work/release"; mkdir -p "$RELEASE_DIR"
dispatched() { [ -f "$work/stub/dispatch" ] && echo true || echo false; }
first=$(measure lost --base "$parent" --countries pl); grep -E '^(LANDABLE|NOT LANDABLE)' "$work/out" > "$work/first-verdict"
check "a measurement stores its verdict in the result cache" "1" "$(find "$RELEASE_DIR" -name 'measure-result-*.json' | wc -l | tr -d ' ')"
check "...and the same request again answers from it: the same verdict, no dispatch" "$first false" \
  "$(measure lost --base "$parent" --countries pl) $(dispatched)"
check "...saying so, with the stored table" "true" \
  "$(grep -q 'CACHED verdict of run 777' "$work/out" && grep -qxF "$(cat "$work/first-verdict")" "$work/out" && echo true || echo false)"
check "--no-cache measures again" "$first true" "$(measure lost --base "$parent" --countries pl --no-cache) $(dispatched)"
check "another set of countries is another key" "true" "$(measure lost --base "$parent" --countries pl,de > /dev/null; dispatched)"
check "a pinned-pair verdict past IDENTITY_MEASURE_CACHE_HOURS is measured again" "true" \
  "$(IDENTITY_MEASURE_CACHE_HOURS=-1 measure lost --base "$parent" --countries pl > /dev/null; dispatched)"
check "--json neither reads nor stores it" "true" "$(measure lost --base "$parent" --countries pl --json > /dev/null; dispatched)"
unset RELEASE_DIR

# SIGTERM (Ctrl-C alike) while waiting on the run: the run is cancelled and the branch deleted.
rm -rf "$work/stub" "$work/cache"; mkdir -p "$work/stub"
( cd "$repo" && STUB="$work/stub" ORIGIN="$origin" ARTIFACT="$work/landable" IN_PROGRESS=1 \
    KINOWO_IDENTITY_CACHE="$work/cache" IDENTITY_MEASURE_POLL=1 PATH="$work/bin:$PATH" \
    exec "$REPO_ROOT/scripts/identity-measure" --countries pl > "$work/out" 2>&1 ) &
pid=$!
wait_until "the tool is watching the run" '[ -f "$work/stub/watched" ]'
# A watchdog: a script that defers the signal (a wait inside a $(...) subshell does) must fail this, not hang it.
( sleep 20; kill -KILL "$pid" 2> /dev/null ) & watchdog=$!
kill -TERM "$pid"; wait "$pid"; status=$?
kill "$watchdog" 2> /dev/null
check "SIGTERM while waiting on the run exits 130" "130" "$status"
check "...cancelling the run" "cancelled" "$(cat "$work/stub/cancelled" 2>/dev/null)"
check "...and deleting the branch" "0" "$(measure_branches)"

# --wait: the run's id and an ETA said before the wait, then agentctl wait's backoff (60 s, x1.5, at most 5 min) between
# reads instead of a read every DISPATCH_POLL_SECONDS. `sleep` is stubbed to log what it was asked.
mkdir -p "$work/sleepbin"; printf '#!/bin/sh\necho "$1" >> "$SLEEP_LOG"\n' > "$work/sleepbin/sleep"; chmod +x "$work/sleepbin/sleep"
eta_run() { printf '{"status":"completed","conclusion":"%s","headBranch":"%s","createdAt":"2026-10-23T10:00:00Z","updatedAt":"2026-10-23T%s:00Z"}' "$@"; }
slept() { paste -sd' ' "$work/slept" 2>/dev/null; }
rm -f "$work/slept"
check "--wait says the run and its ETA from the workflow's recent measure runs" "0|true" \
  "$(PATH="$work/sleepbin:$PATH" SLEEP_LOG="$work/slept" ETA_RUNS="[$(eta_run success measure/a 10:30),$(eta_run failure measure/b 10:50),$(eta_run success main 13:00)]" \
     measure landable --countries pl --wait)|$(grep -q 'run 777: ETA ~50 min (median of 2 recent runs)' "$work/out" && echo true || echo false)"
rm -f "$work/slept"
check "...then waits with the backoff: 60 s, 90, 135" "0|60 90 135" \
  "$(PATH="$work/sleepbin:$PATH" SLEEP_LOG="$work/slept" VIEW_PENDING=3 measure landable --countries pl --wait)|$(slept)"
rm -f "$work/slept"
check "...where without --wait it reads every DISPATCH_POLL_SECONDS, and says no ETA" "0|0 0 0|false" \
  "$(PATH="$work/sleepbin:$PATH" SLEEP_LOG="$work/slept" VIEW_PENDING=3 measure landable --countries pl)|$(slept)|$(grep -q ETA "$work/out" && echo true || echo false)"
rm -f "$work/slept"
check "no recent measure run to time: said, and the wait goes on" "0|true" \
  "$(PATH="$work/sleepbin:$PATH" SLEEP_LOG="$work/slept" measure landable --countries pl --wait)|$(grep -q 'run 777: no ETA' "$work/out" && echo true || echo false)"
check "--wait and --no-wait together are refused" "2" "$(measure landable --countries pl --wait --no-wait)"

spec_summary
