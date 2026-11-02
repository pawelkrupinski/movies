#!/usr/bin/env bash
# identity-takes-check.sh's choice of BASE, against a stub `gh` and a throwaway repository: the newest
# earlier commit whose own check passed (so a red check stays red until fixed or accepted), however far
# back, across pages of history; a check that has only ever failed diffs from the base its first run
# took, never the head's parent; the parent only when the check never ran; a failed history read stops
# the check (exit 2) rather than falling back to the parent; and a range touching no identity code
# passes without resolving anything.
set -euo pipefail

script="$(cd "$(dirname "$0")" && pwd)/identity-takes-check.sh"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
REPO_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

# origin holds c1 <- c2 <- c3, none touching identity code; the job checks out c3 shallowly, as actions/checkout does.
. "$REPO_ROOT/scripts/scratch-git.sh"
scratch_repo "$work/origin.git" --bare
scratch_repo "$work/src"
for n in 1 2 3; do
  echo "$n" > "$work/src/README"
  scratch_git "$work/src" add README
  scratch_git "$work/src" commit -q -m "c$n"
done
scratch_git "$work/src" push -q "$work/origin.git" HEAD:refs/heads/main
c1=$(scratch_git "$work/src" rev-parse HEAD~2) c2=$(scratch_git "$work/src" rev-parse HEAD~1) c3=$(scratch_git "$work/src" rev-parse HEAD)
mkdir -p "$work/job"
scratch_git "$work/job" clone -q --depth=1 --branch main "file://$work/origin.git" .

# gh: GraphQL history pages of the commits before c3 -- c2, then (PAGED: on a second page) c1, each with its parent and
# its `identity takes (pl)` state from $work/status-<sha> (absent: none); HISTORY_FAILS fails the read.
c0=0000000000000000000000000000000000000000
mkdir -p "$work/bin"
cat > "$work/bin/gh" <<EOF
#!/usr/bin/env bash
[ -n "\${HISTORY_FAILS:-}" ] && { echo "HTTP 502" >&2; exit 1; }
[ -n "\${HISTORY_HANGS:-}" ] && { echo \$\$ >> "\$HISTORY_HANGS"; exec sleep 86400; } # never answers: blocks until killed
state() { cat "$work/status-\$1" 2>/dev/null || echo -; }
case "\$*" in
  "api repos/o/r/commits/"*) short="\${2##*/}"; for c in $c1 $c2 $c3; do [ "\${c#\$short}" != "\$c" ] && { echo "\$c"; exit 0; }; done
                            echo "No commit found for SHA: \$short" >&2; exit 1 ;;
  *"cursor=$c2 0"*) echo "$c1 \$(state $c1) $c0" ;;
  *cursor=*) echo "not a valid cursor" >&2; exit 1 ;; # GitHub's history cursor is "<sha> <offset>", space and all
  *) echo "$c2 \$(state $c2) $c1"
     if [ -n "\${PAGED:-}" ]; then echo "NEXT $c2 0"; else echo "$c1 \$(state $c1) $c0"; fi ;;
esac
EOF
chmod +x "$work/bin/gh"

run() { rm -rf "$work/out"; (cd "$work/job" && PATH="$work/bin:$PATH" GITHUB_REPOSITORY=o/r GITHUB_SHA=$c3 RUNNER_TEMP=$work \
  spec_bounded "$script" pl "$work/out" > "$work/log" 2>&1); }

echo SUCCESS > "$work/status-$c1"
run || true; check "the newest green commit is the base, past a red one" "$c1" "$(cat "$work/out/base.txt")"
check "a range with no identity code passes unresolved" 1 "$(grep -c 'no identity code changed' "$work/out/verdict.txt")"

echo FAILURE > "$work/status-$c2"
PAGED=1 run || true; check "a green commit on a later page of history, past a red one" "$c1" "$(cat "$work/out/base.txt")"

rm "$work/status-$c1"
run || true; check "a check that only ever failed diffs from its first run's base, never the head's parent" "$c1" "$(cat "$work/out/base.txt")"

rm "$work/status-$c2"
run || true; check "a check that never ran: the parent" "$c2" "$(cat "$work/out/base.txt")"

echo SUCCESS > "$work/status-$c2"
(TAKES_BASE=$c1; export TAKES_BASE; run || true)
check "a base named by hand overrides the newest green commit" "$c1" "$(cat "$work/out/base.txt")"
rm "$work/status-$c2"

code=0; HISTORY_FAILS=1 run || code=$?
check "a failed history read stops the check" 2 "$code"
# The stub never answers: the check stops only by its bound cutting the read off (shell-spec.sh's HUNG STUBS).
: > "$work/hung"
code=0; HISTORY_HANGS="$work/hung" IDENTITY_TAKES_GH_TIMEOUT=1 run || code=$?
check "a history read that hangs is cut off by its bound, and stops the check, leaving no gh running" "2 0" \
  "$code $(spec_still_running "$work/hung")"

# github.com's history floor (.github/github-takes-history-floor): the history query keeps only statuses at or after it;
# a floor that is no ISO time stops the check.
cat > "$work/bin/gh-recording" <<EOF2
#!/usr/bin/env bash
printf '%s\\n' "\$*" >> "$work/gh-args"
exec "$work/bin/gh-history-stub" "\$@"
EOF2
cp "$work/bin/gh" "$work/bin/gh-history-stub"; mv "$work/bin/gh-recording" "$work/bin/gh"; chmod +x "$work/bin/gh"
: > "$work/gh-args"; (GITHUB_SERVER_URL=https://github.com TAKES_HISTORY_FLOOR=2026-10-31T15:00:00Z run || true)
check "github.com asks only for statuses since its floor" 1 "$(grep -c '>= "2026-10-31T15:00:00Z")' "$work/gh-args")"
: > "$work/gh-args"; (GITHUB_SERVER_URL=https://forgejo.example TAKES_HISTORY_FLOOR=2026-10-31T15:00:00Z run || true)
check "...and Forgejo reads no floor" 1 "$(grep -c '>= "")' "$work/gh-args")"
code=0; (GITHUB_SERVER_URL=https://github.com TAKES_HISTORY_FLOOR=yesterday; export GITHUB_SERVER_URL TAKES_HISTORY_FLOOR; run) || code=$?
check "a floor that is no ISO time stops the check" 2 "$code"
mv "$work/bin/gh-history-stub" "$work/bin/gh"

# Run 38533191835 was dispatched with takes-base=c2974e3aa: a fetch asks for a FULL object name, so every leg failed to
# fetch it -- and passed. A short base is expanded through the API; one naming no commit, or one the fetch cannot reach,
# stops the check (exit 2, which the leg posts as `error` and fails on), saying why in verdict.txt.
echo SUCCESS > "$work/status-$c2"
(TAKES_BASE=${c1:0:9}; export TAKES_BASE; run || true)
check "a base named by its abbreviation is expanded to the commit" "$c1" "$(cat "$work/out/base.txt")"
code=0; (TAKES_BASE=deadbeef1; export TAKES_BASE; run) || code=$?
check "a short base naming no commit stops the check" 2 "$code"
check "...saying so in the verdict the leg posts" 1 "$(grep -c 'no commit deadbeef1' "$work/out/verdict.txt")"
code=0; (TAKES_BASE=1111111111111111111111111111111111111111; export TAKES_BASE; run) || code=$?
check "a base the fetch cannot reach stops the check, never passes it" 2 "$code"
check "...saying so in the verdict the leg posts" 1 "$(grep -c 'could not fetch base' "$work/out/verdict.txt")"
rm "$work/status-$c2"

# THE STORE. c4 and c5 each touch identity code, so c5's check resolves; its base c4 ran the check before it, on main,
# over the same pinned pair, and stored what it resolved. A stub `sbt` writes one take and logs which checkout ran it, and
# how; a stub release (a directory) holds what `upload` stored and answers `download` from it. c5 brings the resolve and
# the ratchet as tools: c4, from before them, runs them as the specs they were.
for n in 4 5; do
  mkdir -p "$work/src/common/src/main" && echo "$n" > "$work/src/common/src/main/Identity.scala"
  if [ "$n" = 5 ]; then
    mkdir -p "$work/src/worker/src/it/scala" "$work/src/worker/src/test/scala/scripts"
    # (tools that take --trace: the takes check asks them for the normalizer trace)
    echo 'object IdentityResolveDump // "trace"' > "$work/src/worker/src/it/scala/IdentityResolveDump.scala"
    echo 'object UnmatchedClustersRatchet // "trace"' > "$work/src/worker/src/test/scala/scripts/UnmatchedClustersRatchet.scala"
    scratch_git "$work/src" add worker
  fi
  scratch_git "$work/src" add common && scratch_git "$work/src" commit -q -m "c$n"
done
scratch_git "$work/src" push -q "$work/origin.git" HEAD:refs/heads/main
c4=$(scratch_git "$work/src" rev-parse HEAD~1) c5=$(scratch_git "$work/src" rev-parse HEAD)
rm -rf "$work/job" && mkdir -p "$work/job" && scratch_git "$work/job" clone -q --depth=1 --branch main "file://$work/origin.git" .
mkdir -p "$work/job/test/resources/fixtures" "$work/job/scripts/ci"
printf 'country\tvenue\trawTitle\tfilm\tverdict\tstate\tscope\tfilmTitle\treason\tjudged\tevidence\n' > "$work/job/test/resources/fixtures/identity-judgements.tsv"
cp "$(dirname "$script")"/identity_*.py "$(dirname "$script")/report_fields.py" "$(dirname "$script")/gh-release.sh" "$work/job/scripts/ci/"
cat > "$work/bin/sbt" <<'SBT'
#!/usr/bin/env bash
# Every argument not a flag is an sbt command, run in order as sbt runs them: "[success]" after each, and the first that
# fails (a tool's exit, which takes sbt's JVM with it) ends the run. The tools' flags are read from the command, the
# specs' variables from the environment.
flag() { local i; for i in "${!words[@]}"; do [ "${words[$i]}" = "$1" ] && { echo "${words[$((i + 1))]:-yes}"; return; }; done; }
called() { echo "$(pwd -P) $1" >> "$SBT_CALLS"; }
decisions() { # <dir> <cc>
  mkdir -p "$1"
  # STUB_HEAD_FILM: the head's take of the one listing, where the base's is film 1 -- a take the code moved
  local film=1; case "$(pwd -P)" in *identity-takes-base*) ;; *) film=${STUB_HEAD_FILM:-1} ;; esac
  echo '{"key":"k","venue":"v","rawTitle":"t","film":'"$film"',"filmTitle":"F","basis":"b","explanation":[],"blocker":null}' > "$1/decisions-$2.jsonl"
}
takes() { # <gaps read as unknown: 1 or empty>
  mkdir -p target/identity-unmatched
  # STUB_GAPS: the capture lacks a lookup this code asks -- the ratchet writes takes only reading it as unknown
  if [ -n "${STUB_GAPS:-}" ]; then
    [ "$1" = 1 ] || { echo "the fixture lacks answers this resolve asks"; exit 1; }
    printf 'country\tkind\tlookup\npl\tfamily\t(RottenTomatoes, record|x)\nuk\ttmdb\tsearch y\n' > target/identity-unmatched/capture-gaps.tsv
  fi
  printf 'country\tvenue\trawTitle\tstatus\tfilm\ttitle\tbasis\npl\tv\tt\tRIGHT\ttmdb:1\tF\tb\n' > target/identity-unmatched/ratchet-takes.tsv
}
traced() { local t; t=$(flag --trace); [ -z "$t" ] || echo "{\"tool\":\"$1\"}" | gzip > "$t"; }
run_one() {
case "$command" in
  *"runMain integration.IdentityResolveDump "*)
    called dump-tool; traced dump; decisions "$(flag --out)" "$(flag --full)"
    [ -z "$(flag --population)" ] || echo "kind=identity.population_flips country=$(flag --full) chains=$(flag --population) families=1"
    [ -z "${STUB_DUMP_EXIT:-}" ] || exit "$STUB_DUMP_EXIT" ;;
  *IdentityResolveDumpIntegrationSpec*)
    called dump-spec; decisions "$KINOWO_IDENTITY_DUMP" "$KINOWO_IDENTITY_FULL" ;;
  *"runMain scripts.UnmatchedClustersRatchet "*)
    called ratchet-tool; traced ratchet; [ "$(flag --out)" = target/identity-unmatched ] || exit 2
    takes "$([ -n "$(flag --measure)" ] && echo 1)"; [ -z "${STUB_RATCHET_EXIT:-}" ] || exit "$STUB_RATCHET_EXIT" ;;
  *UnmatchedClustersRatchetSpec*)
    called ratchet-spec; takes "${KINOWO_RATCHET_CAPTURE_GAPS_UNKNOWN:-}" ;;
esac
}
pwd -P >> "$SBT_CALLS.runs"
for command in "$@"; do
  case "$command" in -*) continue ;; esac
  read -ra words <<< "$command"
  run_one
  echo "[success] Total time: 1 s"
done
SBT
chmod +x "$work/bin/sbt"
# gh: the history stub above, plus a release kept in $work/release.
mv "$work/bin/gh" "$work/bin/gh-history"
cat > "$work/bin/gh" <<GH
#!/usr/bin/env bash
case "\$1 \$2" in
  "release download") [ -f "$work/release/\$5" ] && cp "$work/release/\$5" "\$7" ;;
  "release upload")   [ -n "\${UPLOAD_RACES:-}" ] && { mkdir -p "$work/release" && cp "\$4" "$work/release/"; exit 1; }
                      mkdir -p "$work/release" && cp "\$4" "$work/release/" ;;
  "release create")   echo "\$3" >> "$work/creates"; mkdir -p "$work/release" ;;
  "release view")     case "\$*" in
                        *--json*) ;;
                        *) [ -d "$work/release" ] || { echo "release not found" >&2; exit 1; } ;;
                      esac
                      case "\$*" in
                        *takes-full-*) f=\$(ls "$work"/release/takes-full-*.txt 2> /dev/null | head -1)
                                       [ -n "\$f" ] && python3 -c 'import os, sys, datetime as d; print(d.datetime.fromtimestamp(os.path.getmtime(sys.argv[1]), d.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"))' "\$f" ;;
                        *) for f in "$work"/release/*; do [ -f "\$f" ] && basename "\$f"; done ;;
                      esac ;;
  "api repos/o/r/actions/runs/7/jobs"*)
    [ -d "$work/pending" ] && mkdir -p "$work/release" && mv "$work"/pending/* "$work/release/" 2> /dev/null && rmdir "$work/pending"
    cat "$work/base-row" 2> /dev/null || true ;;
  *) exec "$work/bin/gh-history" "\$@" ;;
esac
GH
chmod +x "$work/bin/gh"
store_run() { # <head>: the check of <head>, on main, storing on the release
  rm -rf "$work/out" "$work/identity-takes-base"; : > "$work/sbt-calls"; : > "$work/sbt-calls.runs"
  (cd "$work/job" && export RUNNER_TEMP="$work" && { [ -z "${NO_RUNNER_TEMP:-}" ] || unset RUNNER_TEMP; } &&
   PATH="$work/bin:$PATH" GITHUB_REPOSITORY=o/r GITHUB_SHA=$1 GITHUB_REF=${REF-refs/heads/main} \
    SBT_CALLS="$work/sbt-calls" TAKES_STORE=fixtures KINOWO_CONVERGENCE_PIN_CORPUS_RUN=${PIN-42} "$script" pl "$work/out" > "$work/log" 2>&1)
}
base_calls() { grep -c "identity-takes-base" "$work/sbt-calls" || true; }
# an earlier run that ended before removing its base checkout left it registered, its directory since removed
scratch_git "$work/job" worktree add -q --detach "$work/identity-takes-base" HEAD
code=0; TAKES_BASE=$c4 store_run "$c5" || code=$?
check "a base checkout an earlier run left registered is checked out afresh" "0 0" \
  "$code $(grep -c 'already registered' "$work/log")"
rm -rf "$work/release" "$work/creates"
code=0; TAKES_BASE=$c4 store_run "$c5" || code=$?
check "with nothing stored, the base is checked out and resolved" 2 "$(base_calls)"
check "...and the check passes on the same takes" 0 "$code"
check "...and the head's decisions, final takes and normalizer trace (by its code, and by its exact inputs), the base's it resolved, and the daily full row's marker, are stored" 7 "$(ls "$work/release" | wc -l | tr -d ' ')"
# The store is a release of its own (run 41351315090: beside the fixtures it took their release to the 1000-asset cap).
check "...on a store release the first result made, once" "fixtures" "$(cat "$work/creates" 2> /dev/null)"
check "...the exact-input trace among them" 1 "$(find "$work/release" -name 'takes-tracex-pl-*.gz' | wc -l | tr -d ' ')"
check "...the trace under the head's identity code and the pinned recording, both tools' readings in one" \
  "1 dump ratchet" "$(find "$work/release" -name 'takes-trace-pl-*-p42.gz' | grep -cE 'takes-trace-pl-[0-9a-f]{24}-p42\.gz$') $(
    gunzip -c "$work"/release/takes-trace-pl-*-p42.gz | gunzip -c | sed 's/.*"tool":"\([a-z]*\)".*/\1/' | sort | xargs)"
check "...each phase timed, saying which it resolved" "phase:how head-final:replayed head-resolve:resolved base-final:replayed base-resolve:resolved " \
  "$(cut -f1,3 "$work/out/timings.tsv" | tr '\t\n' ': ')"
check "...and why the base was not stored" 1 "$(grep -c 'the base resolve not stored -- takes-decisions-pl-.* not on release fixtures: resolving it' "$work/log")"
check "...the head resolving and replaying with the tools" "dump-tool ratchet-tool" "$(grep -v identity-takes-base "$work/sbt-calls" | cut -d' ' -f2 | sort | xargs)"
check "...the resolve in the checkout, the final takes beside it in one of their own (two sbt runs at once)" \
  "identity-takes-head-final job" "$(for c in ratchet dump; do grep -v identity-takes-base "$work/sbt-calls" | grep " $c-" | cut -d' ' -f1 | xargs basename; done | xargs)"
check "...its base, from before them, with the specs they were" "dump-spec ratchet-spec" "$(grep identity-takes-base "$work/sbt-calls" | cut -d' ' -f2 | sort | xargs)"
# outside Actions: no RUNNER_TEMP (the same run, nothing stored)
rm -rf "$work/release"; mkdir -p "$work/tmp"
code=0; NO_RUNNER_TEMP=1 TMPDIR="$work/tmp" TAKES_BASE=$c4 store_run "$c5" || code=$?
check "outside Actions (no RUNNER_TEMP) the check runs, the base in a scratch directory of its own" "0 2" "$code $(base_calls)"
check "...which is removed at exit" "0" "$(find "$work/tmp" -mindepth 1 -maxdepth 1 | wc -l | tr -d ' ')"
# (each with nothing stored: a head whose results are stored runs no resolve or ratchet to fail -- THE HEAD'S OWN RESULTS)
rm -rf "$work/release"; code=0; STUB_DUMP_EXIT=1 TAKES_BASE=$c4 store_run "$c5" || code=$?
check "a resolve that fails, whatever it wrote, is no verdict" "2 1" "$code $(grep -c 'the resolve exited 1' "$work/log")"
rm -rf "$work/release"; code=0; STUB_RATCHET_EXIT=1 TAKES_BASE=$c4 store_run "$c5" || code=$?
check "a ratchet that fails measuring, whatever it wrote, is no verdict" "2 1" "$code $(grep -c 'the ratchet exited 1' "$work/log")"
rm -rf "$work/release"
# (a mktemp that fails: macOS's ignores TMPDIR)
mkdir -p "$work/no-tmp" && printf '#!/usr/bin/env bash\necho "mktemp: no temporary directory" >&2; exit 1\n' > "$work/no-tmp/mktemp"
chmod +x "$work/no-tmp/mktemp"
code=0; PATH="$work/no-tmp:$PATH" TAKES_BASE=$c4 store_run "$c5" || code=$?
check "the check writes nothing outside its out dir: no temporary directory loses no verdict" \
  "0 1" "$code $(grep -c 'identity final takes (pl): 1 listings' "$work/out/verdict.txt")"
check "the head's resolve walks its families' arrival chains, the population line in the verdict (report only)" \
  1 "$(grep -c '^kind=identity.population_flips country=pl chains=4 ' "$work/out/verdict.txt")"

# A measure/ branch (scripts/identity-measure) stores the BASE it resolved -- main's commit, under a key naming its code
# and inputs, never the ref -- and never its own head: the next measurement against the same base reuses it.
rm -rf "$work/release"
code=0; REF=refs/heads/measure/c5-1 TAKES_BASE=$c4 store_run "$c5" || code=$?
check "a measurement stores the base it resolved, not its head" 2 "$(ls "$work/release" | wc -l | tr -d ' ')"
REF=refs/heads/measure/c5-2 TAKES_BASE=$c4 store_run "$c5" || true
check "...so the next measurement against that base resolves only its head" 0 "$(base_calls)"
check "...timing the base as reused" 1 "$(grep -c 'base-resolve.*reused' "$work/out/timings.tsv")"
# Two measurements of one base at once: the one whose upload is refused because the other's landed first says so, and
# warns of nothing (a warning there read as "the store is broken").
rm -rf "$work/release"
UPLOAD_RACES=1 REF=refs/heads/measure/c5-3 TAKES_BASE=$c4 store_run "$c5" || true
check "an upload refused because another run stored the asset meanwhile is no warning" \
  "0 1" "$(grep -c 'could not store' "$work/log") $(grep -c 'takes-decisions-pl-.* was stored by another run meanwhile' "$work/log")"

# THE BASE ROW. A measurement's `takes-base` row (TAKES_SIDE=base) stores the base's side on a runner of its own; its
# `takes` row (TAKES_SIDE=head) runs the head's and waits for them while that row runs (the stub jobs API answers its
# state from $work/base-row, and hands over what $work/pending holds -- the base row storing it meanwhile).
side_run() { # <side> <head>
  REF=refs/heads/measure/c5-side TAKES_SIDE=$1 TAKES_BASE_JOB="takes-base (poland)" GITHUB_RUN_ID=7 TAKES_BASE_POLL=0 \
    TAKES_BASE=$c4 store_run "$2"
}
head_calls() { grep -vc identity-takes-base "$work/sbt-calls" || true; }
rm -rf "$work/release" "$work/pending"
code=0; side_run base "$c5" || code=$?
check "the base row resolves the base's side" "0 2" "$code $(base_calls)"
check "...and nothing of the head's" 0 "$(head_calls)"
check "...its final takes replayed in a checkout of their own, beside the resolve (one target, two sbt runs)" \
  "identity-takes-base identity-takes-base-final" \
  "$(for c in dump ratchet; do grep " $c-" "$work/sbt-calls" | cut -d' ' -f1 | xargs basename; done | xargs)"
check "...storing the base's decisions and final takes" 2 "$(ls "$work/release" | wc -l | tr -d ' ')"
side_run base "$c5" || true
check "...or, stored already, nothing at all" 0 "$(base_calls)"
mkdir -p "$work/pending" && mv "$work/release"/* "$work/pending/"
echo in_progress > "$work/base-row"
code=0; side_run head "$c5" || code=$?
check "the takes row beside it resolves the head alone" "0 2" "$(base_calls) $(head_calls)"
check "...waiting for what the base row stores meanwhile" "base-final:awaited base-resolve:reused " \
  "$(grep base- "$work/out/timings.tsv" | cut -f1,3 | tr '\t\n' ': ')"
check "...and the check passes on the same takes" 0 "$code"
rm -rf "$work/release"
echo completed > "$work/base-row"
side_run head "$c5" || true
check "a base row done without storing them leaves the base to the takes row" 2 "$(base_calls)"
rm -rf "$work/release"
echo queued > "$work/base-row"
side_run head "$c5" || true
check "...as does one still waiting for a runner" 2 "$(base_calls)"
rm -f "$work/base-row"

# A capture lacking a lookup the code asks: the final takes are still replayed, the gap read as unknown and named --
# only this row's country's (run 38694072183 lost every country's final takes to one UK Rotten Tomatoes record).
rm -rf "$work/release"
code=0; STUB_GAPS=1 TAKES_BASE=$c4 store_run "$c5" || code=$?
check "a capture lacking a lookup the code asks still has its final takes replayed, and the check passes" 0 "$code"
check "...naming the row's own gap as a blind spot" \
  1 "$(grep -c "the head's final takes read 1 lookup(s) the capture lacks as UNKNOWN.*family (RottenTomatoes, record|x)" "$work/out/verdict.txt")"
check "...and not another country's" 0 "$(grep -c 'search y' "$work/out/verdict.txt")"
rm -rf "$work/release"
code=0; TAKES_BASE=$c4 store_run "$c5" || code=$?

# c4's own check, then c5's again: its base c4 is now stored.
TAKES_BASE=$c2 store_run "$c4" || true
code=0; TAKES_BASE=$c4 store_run "$c5" || code=$?
check "a base its own run stored is reused, not compiled or resolved" 0 "$(base_calls)"
check "...nor the head, which its own earlier run stored" 0 "$(grep -vc identity-takes-base "$work/sbt-calls")"
check "...and the check still diffs and passes" 0 "$code"
check "...saying it reused the base" 1 "$(grep -c 'base resolve reused' "$work/log")"

PIN=43 TAKES_BASE=$c4 store_run "$c5" || true
check "another pinned recording is another key: the base's decisions resolve again" 1 "$(base_calls)"
check "...its final takes, which read only the capture, still reused" 1 "$(grep -c 'base final takes reused' "$work/log")"

echo unpublished > "$work/laid-fill"
TAKES_BASE=$c4 store_run "$c5" || true
check "a fill laid but never published names no key: nothing is reused" 2 "$(base_calls)"
rm -f "$work/laid-fill"

# No pinned recording names the decisions' inputs: their key would match another unpinned run's over other data.
rm -rf "$work/release"; PIN="" TAKES_BASE=$c2 store_run "$c4" || true
PIN="" TAKES_BASE=$c4 store_run "$c5" || true
check "with no recording pinned the base's decisions are resolved, never reused from a key naming no recording" 1 "$(base_calls)"
check "...nor stored under one" 0 "$(find "$work/release" -name "*decisions*" | wc -l | tr -d " ")"

# c6 touches no identity code, so its own check passed unresolved and stored nothing -- and it is the base of c7's. Its
# identity code is c5's, which c5's run stored: the key is the identity code, not the commit, so c7 reuses c5's results
# (run 38599662543: base 263b1c341 had exited early, and its US row compiled and resolved the base again, 270s).
echo 6 > "$work/src/README" && scratch_git "$work/src" add README && scratch_git "$work/src" commit -q -m c6
echo 7 > "$work/src/common/src/main/Identity.scala" && scratch_git "$work/src" add common && scratch_git "$work/src" commit -q -m c7
scratch_git "$work/src" push -q "$work/origin.git" HEAD:refs/heads/main
c6=$(scratch_git "$work/src" rev-parse HEAD~1) c7=$(scratch_git "$work/src" rev-parse HEAD)
at() { scratch_git "$work/job" fetch -q --depth=1 origin "$1" && scratch_git "$work/job" checkout -q "$1"; } # the head, as CI checks it out
TAKES_BASE=$c4 store_run "$c5" || true
at "$c6"; TAKES_BASE=$c5 store_run "$c6" || true
check "c6's check, touching no identity code, resolves nothing" 1 "$(grep -c 'no identity code changed' "$work/out/verdict.txt")"
at "$c7"; code=0; TAKES_BASE=$c6 store_run "$c7" || code=$?
check "a base that stored nothing, its identity code another's that did, reuses that one's decisions" \
  1 "$(grep -c 'base resolve reused' "$work/log")"
check "...and its final takes" 1 "$(grep -c 'base final takes reused' "$work/log")"
check "...compiling and resolving nothing for the base" 0 "$(base_calls)"
check "...and the check still passes" 0 "$code"

# THE HEAD'S OWN RESULTS. A measurement's base row stores main's newest commit while main's own run for it waits; main's
# takes row then fetches its head's results under the same keys and runs no sbt for them. A key that differs in any
# input -- another recording -- is made again, never a nearer one reused.
rm -rf "$work/release"
REF=refs/heads/measure/c7-1 TAKES_BASE=$c5 store_run "$c7" || true
check "a measurement against c5 stores c5's side" 1 "$(grep -c 'the base resolve not stored' "$work/log")"
code=0; TAKES_BASE=$c4 store_run "$c5" || code=$?
check "main's check of c5 then runs no sbt for its head" "0 1" \
  "$(head_calls) $(grep -c 'every head result was stored -- no head session' "$work/log")"
check "...timing the head's results as reused" "head-final:reused head-resolve:reused " \
  "$(grep head- "$work/out/timings.tsv" | cut -f1,3 | tr '\t\n' ': ')"
check "...and the check still diffs and passes" "0 1" "$code $(grep -c 'identity final takes (pl): 1 listings' "$work/out/verdict.txt")"
code=0; PIN=43 TAKES_BASE=$c4 store_run "$c5" || code=$?
check "another recording is another key: the head resolves again, its final takes (the capture's alone) reused" \
  "dump-tool" "$(grep -v identity-takes-base "$work/sbt-calls" | cut -d' ' -f2 | xargs)"
check "...and passes" 0 "$code"
rm -f "$work"/release/takes-final-*
code=0; TAKES_BASE=$c4 store_run "$c5" || code=$?
check "a head whose decisions alone are stored replays only its final takes" \
  "ratchet-tool" "$(grep -v identity-takes-base "$work/sbt-calls" | cut -d' ' -f2 | xargs)"
check "...in a checkout of its own, and no head session" "identity-takes-head-final 0" \
  "$(grep -v identity-takes-base "$work/sbt-calls" | grep ' ratchet-' | cut -d' ' -f1 | xargs basename) $(grep -c "the head's session ran" "$work/log")"
# the capture's gaps travel with the final takes: a reused head still names its blind spots
rm -rf "$work/release"
STUB_GAPS=1 TAKES_BASE=$c4 store_run "$c5" || true
code=0; STUB_GAPS=1 TAKES_BASE=$c4 store_run "$c5" || code=$?
check "a head whose final takes are reused still names the gaps its capture has" "0 0 1" "$code $(head_calls) $(
  grep -c "the head's final takes read 1 lookup(s) the capture lacks as UNKNOWN" "$work/out/verdict.txt")"
check "...as does a base whose are" 1 "$(grep -c "the base's final takes read 1 lookup(s) the capture lacks" "$work/out/verdict.txt")"
rm -rf "$work/release"

# THE SKIP GATE (takes-skip-gate.sh, here a stand-in answering STUB_GATE: SKIP or RUN) and THE SHADOW. Off: nothing asked.
cat > "$work/bin/gate" <<'GATE'
#!/usr/bin/env bash
echo "$TAKES_SKIP_MODE $*" >> "$GATE_LOG"
if [ "${STUB_GATE:-RUN}" = SKIP ]; then echo SKIP; echo "measure: footprint, 0 readings differ (2 readings)"; echo "takes-skip-gate: SKIP -- stub" >&2; exit 0; fi
echo "takes-skip-gate: RUN -- stub" >&2; echo RUN; exit 1
GATE
chmod +x "$work/bin/gate"
export GATE_LOG="$work/gate.log"
gate_run() { # <mode> [head]: the check of c5 (or <head>) against c4, the gate the stand-in; MARKER fresh or old seeds the
  # daily full row's marker (none by default)
  : > "$GATE_LOG"; rm -rf "$work/release"
  if [ -n "${MARKER:-}" ]; then
    mkdir -p "$work/release" && echo marker > "$work/release/takes-full-pl.txt"
    [ "$MARKER" = old ] && touch -t 202001010000 "$work/release/takes-full-pl.txt"
  fi
  TAKES_SKIP_MODE=$1 TAKES_SKIP_GATE_CMD="$work/bin/gate" TAKES_BASE=$c4 store_run "${2:-$c5}"
}
code=0; gate_run off || code=$?
check "skip gate off: never asked, no shadow" "0 0 0" "$code $(wc -l < "$GATE_LOG" | tr -d ' ') $(grep -c takes-skip-shadow "$work/log")"
code=0; STUB_GATE=RUN gate_run shadow || code=$?
check "shadow, the gate says RUN: the takes run, and agree" "0 2 1" \
  "$code $(head_calls) $(grep -c 'takes-skip-shadow (pl): mode=shadow gate=RUN actual=unmoved agree=yes' "$work/log")"
check "...the gate asked about this base and head, with the base's exact-input trace and the proof" 1 \
  "$(grep -cE "^shadow $c4 $c5 .*/gate/base-trace.jsonl.gz -- .*/scripts/ci/takes-skip-proof.sh$" "$GATE_LOG")"
code=0; STUB_GATE=SKIP gate_run shadow || code=$?
check "shadow, the gate says SKIP and no take moved: the takes still run, and agree" "0 2 1" \
  "$code $(head_calls) $(grep -c 'gate=SKIP actual=unmoved agree=yes' "$work/log")"
code=0; STUB_HEAD_FILM=2 STUB_GATE=SKIP gate_run shadow || code=$?
check "shadow, the gate says SKIP but a take moved: the row fails, the gate named unsound" "1 1 1" \
  "$code $(grep -c 'gate=SKIP actual=moved agree=no' "$work/log") $(grep -c 'the gate said SKIP but the takes were moved -- the gate is unsound' "$work/log")"
code=0; STUB_HEAD_FILM=2 STUB_GATE=RUN gate_run shadow || code=$?
check "shadow, the gate says RUN and a take moved: agree (the takes verdict is the takes check's own)" 1 \
  "$(grep -c 'gate=RUN actual=moved agree=yes' "$work/log")"
# enforce: a SKIP leaves the takes diff out -- unless the commit is sampled (TAKES_SKIP_SAMPLE_EVERY=1: every one is)
check "a full takes row on main refreshes the daily marker" 1 "$([ -f "$work/release/takes-full-pl.txt" ] && echo 1 || echo 0)"
code=0; MARKER=fresh TAKES_SKIP_SAMPLE_EVERY=1000000000 STUB_GATE=SKIP gate_run enforce || code=$?
check "enforce, the gate says SKIP: no resolve or ratchet on either side, and the check passes" "0 0 0" "$code $(head_calls) $(base_calls)"
check "...saying why" 1 "$(grep -c "the takes diff skipped -- measure: footprint, 0 readings differ (2 readings) under the head's rules" "$work/out/verdict.txt")"
check "...and not that no identity code changed" 0 "$(grep -c 'no identity code changed' "$work/out/verdict.txt")"
code=0; TAKES_SKIP_SAMPLE_EVERY=1000000000 STUB_GATE=SKIP gate_run enforce || code=$?
check "enforce, SKIP, but no full row's marker: the takes run in full, held against the gate" "0 2 1 1" \
  "$code $(head_calls) $(grep -c 'no full takes row of pl on main in the last 24 hours (no marker)' "$work/log") $(grep -c 'mode=enforce gate=SKIP actual=unmoved agree=yes' "$work/log")"
code=0; MARKER=old TAKES_SKIP_SAMPLE_EVERY=1000000000 STUB_GATE=SKIP gate_run enforce || code=$?
check "enforce, SKIP, the marker older than a day: the takes run in full" "0 2 1" \
  "$code $(head_calls) $(grep -c 'no full takes row of pl on main in the last 24 hours ([0-9]* s ago)' "$work/log")"
code=0; MARKER=fresh TAKES_SKIP_SAMPLE_EVERY=1 STUB_GATE=SKIP gate_run enforce || code=$?
check "enforce, a sampled commit: the takes run in full, held against the gate" "0 2 1" \
  "$code $(head_calls) $(grep -c 'mode=enforce gate=SKIP actual=unmoved agree=yes' "$work/log")"
code=0; STUB_GATE=RUN gate_run enforce || code=$?
check "enforce, the gate says RUN: the takes run" "0 2" "$code $(head_calls)"
code=0; MARKER=fresh TAKES_SKIP_SAMPLE_EVERY=x STUB_GATE=SKIP gate_run enforce || code=$?
check "enforce, a sample rate that is no number: the takes run (sampled)" "0 2" "$code $(head_calls)"
rm -rf "$work/release"

# THE CORPUS SHAPE. c8 brings the specs that write their findings, c9 edits only them (an allowlist): no take can move,
# so nothing resolves -- but the corpus-shape specs run at the head and the base over the one recording, and only a
# finding the head made fails. The stub sbt writes the findings $SHAPE_HEAD / $SHAPE_BASE name (the base's checkout is
# the worktree beside the head).
mkdir -p "$work/src/e2e/src/test/scala/services/movies"
echo 'ConvergenceDiagnostics.write(s"corpus-shape-...")' > "$work/src/e2e/src/test/scala/services/movies/CorpusShapeSpec.scala"
scratch_git "$work/src" add e2e && scratch_git "$work/src" commit -q -m c8
echo '// allowlist edited' >> "$work/src/e2e/src/test/scala/services/movies/CorpusShapeSpec.scala"
scratch_git "$work/src" add e2e && scratch_git "$work/src" commit -q -m c9
scratch_git "$work/src" push -q "$work/origin.git" HEAD:refs/heads/main
c8=$(scratch_git "$work/src" rev-parse HEAD~1) c9=$(scratch_git "$work/src" rev-parse HEAD)
cp "$(dirname "$script")/corpus_shape_diff.py" "$work/job/scripts/ci/"
mv "$work/bin/sbt" "$work/bin/sbt-takes"
cat > "$work/bin/sbt" <<'SBT'
#!/usr/bin/env bash
# the corpus-shape command here, every other one by the takes stub -- in order, each in one sbt
flags=(); for a in "$@"; do case "$a" in -*) flags+=("$a") ;; esac; done
for command in "$@"; do
  case "$command" in
    -*) ;;
    *corpusShapePoland*)
      pwd -P >> "$SBT_CALLS"
      mkdir -p target/convergence-diagnostics
      case "$(pwd -P)" in *identity-takes-base*) findings=$SHAPE_BASE ;; *) findings=$SHAPE_HEAD ;; esac
      printf '%b' "$findings" > target/convergence-diagnostics/corpus-shape-recorded-pl-SearchQueryMarkers.txt
      exit 1 ;; # a finding fails the specs themselves: their exit is never the verdict
    *) "$(dirname "$0")/sbt-takes" "${flags[@]}" "$command" || exit ;;
  esac
done
SBT
chmod +x "$work/bin/sbt"
arrietty='finding\tCineworld Barnsley\tArrietty (4K Restoration) Experience It In IMAX\tsearches it (4k)\n'
preview='finding\tFilmRaum\tLa bola negra , Preview am Mi.\tsearches it (preview)\n'
at "$c9"
code=0; SHAPE_COMMAND=corpusShapePoland SHAPE_HEAD=$arrietty SHAPE_BASE=$arrietty TAKES_BASE=$c8 store_run "$c9" || code=$?
check "an allowlist edit resolves no take" 1 "$(grep -c 'no identity code changed' "$work/out/verdict.txt")"
check "...runs the corpus-shape specs at the head and the base" 2 "$(wc -l < "$work/sbt-calls" | tr -d ' ')"
check "...and passes on a finding the recording brought, standing at both" 0 "$code"
check "...reporting it" 1 "$(grep -c '0 new (the code.s: fail), 1 standing' "$work/out/verdict.txt")"
# (the stub's findings change under one key, as no real head's can: each case a store of its own)
rm -rf "$work/release"
code=0; SHAPE_COMMAND=corpusShapePoland SHAPE_HEAD="$arrietty$preview" SHAPE_BASE=$arrietty TAKES_BASE=$c8 store_run "$c9" || code=$?
check "a finding only the head has fails the check" 1 "$code"
check "...naming it" 1 "$(grep -c 'FAIL SearchQueryMarkers finding: FilmRaum' "$work/out/verdict.txt")"
# c8's own check: its base c6 predates the findings files, and it stores the head's findings for c9's run to reuse.
rm -rf "$work/release"
at "$c8"; code=0; SHAPE_COMMAND=corpusShapePoland SHAPE_HEAD=$arrietty SHAPE_BASE=$preview TAKES_BASE=$c6 store_run "$c8" || code=$?
check "a base from before the specs wrote findings: the head's are reported, not judged" \
  1 "$(grep -c 'the base predates the corpus-shape findings' "$work/out/verdict.txt")"
check "...and pass" 0 "$code"
at "$c9"; code=0; SHAPE_COMMAND=corpusShapePoland SHAPE_HEAD="$arrietty$preview" SHAPE_BASE=$arrietty TAKES_BASE=$c8 store_run "$c9" || code=$?
check "a base whose findings its own run stored is reused, not run" 0 "$(grep -c identity-takes-base "$work/sbt-calls")"
check "...and the head's new finding still fails" 1 "$code"
rm -rf "$work/out"  # the cases above left their findings there: this one's head must write none of its own
code=0; (cd "$work/job" && PATH="$work/bin:$PATH" GITHUB_REPOSITORY=o/r GITHUB_SHA=$c9 RUNNER_TEMP=$work SBT_CALLS="$work/sbt-calls" \
  SHAPE_COMMAND=corpusShapeNowhere TAKES_BASE=$c8 "$script" pl "$work/out" > "$work/log" 2>&1) || code=$?
check "specs that wrote no findings are no verdict, never a pass" 2 "$code"
check "...said as such, the run reaching its verdict" "1 0" \
  "$(grep -c 'the corpus-shape specs wrote no findings' "$work/log") $(grep -c 'unbound variable' "$work/log")"

spec_summary
