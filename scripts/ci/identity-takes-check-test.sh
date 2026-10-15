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
fails=0
check() { if [ "$2" = "$3" ]; then echo "ok   $1"; else echo "FAIL $1: expected '$3', got '$2'"; fails=$((fails + 1)); fi; }

# origin holds c1 <- c2 <- c3, none touching identity code; the job checks out c3 shallowly, as actions/checkout does.
. "$(cd "$(dirname "$0")/../.." && pwd)/scripts/scratch-git.sh"
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
[ -n "\${HISTORY_HANGS:-}" ] && exec sleep 30
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
  "$script" pl "$work/out" > "$work/log" 2>&1); }

echo SUCCESS > "$work/status-$c1"
run || true; check "the newest green commit is the base, past a red one" "$(cat "$work/out/base.txt")" "$c1"
check "a range with no identity code passes unresolved" "$(grep -c 'no identity code changed' "$work/out/verdict.txt")" 1

echo FAILURE > "$work/status-$c2"
PAGED=1 run || true; check "a green commit on a later page of history, past a red one" "$(cat "$work/out/base.txt")" "$c1"

rm "$work/status-$c1"
run || true; check "a check that only ever failed diffs from its first run's base, never the head's parent" "$(cat "$work/out/base.txt")" "$c1"

rm "$work/status-$c2"
run || true; check "a check that never ran: the parent" "$(cat "$work/out/base.txt")" "$c2"

echo SUCCESS > "$work/status-$c2"
(TAKES_BASE=$c1; export TAKES_BASE; run || true)
check "a base named by hand overrides the newest green commit" "$(cat "$work/out/base.txt")" "$c1"
rm "$work/status-$c2"

code=0; HISTORY_FAILS=1 run || code=$?
check "a failed history read stops the check" "$code" 2
started=$SECONDS
code=0; HISTORY_HANGS=1 IDENTITY_TAKES_GH_TIMEOUT=1 run || code=$?
check "a history read that hangs is cut off, and stops the check" "$code" 2
check "...within its bound" "$([ $((SECONDS - started)) -lt 15 ] && echo yes || echo "no: $((SECONDS - started))s")" yes

# Run 38533191835 was dispatched with takes-base=c2974e3aa: a fetch asks for a FULL object name, so every leg failed to
# fetch it -- and passed. A short base is expanded through the API; one naming no commit, or one the fetch cannot reach,
# stops the check (exit 2, which the leg posts as `error` and fails on), saying why in verdict.txt.
echo SUCCESS > "$work/status-$c2"
(TAKES_BASE=${c1:0:9}; export TAKES_BASE; run || true)
check "a base named by its abbreviation is expanded to the commit" "$(cat "$work/out/base.txt")" "$c1"
code=0; (TAKES_BASE=deadbeef1; export TAKES_BASE; run) || code=$?
check "a short base naming no commit stops the check" "$code" 2
check "...saying so in the verdict the leg posts" "$(grep -c 'no commit deadbeef1' "$work/out/verdict.txt")" 1
code=0; (TAKES_BASE=1111111111111111111111111111111111111111; export TAKES_BASE; run) || code=$?
check "a base the fetch cannot reach stops the check, never passes it" "$code" 2
check "...saying so in the verdict the leg posts" "$(grep -c 'could not fetch base' "$work/out/verdict.txt")" 1
rm "$work/status-$c2"

# THE STORE. c4 and c5 each touch identity code, so c5's check resolves; its base c4 ran the check before it, on main,
# over the same pinned pair, and stored what it resolved. A stub `sbt` writes one take and logs which checkout ran it; a
# stub release (a directory) holds what `upload` stored and answers `download` from it.
for n in 4 5; do
  mkdir -p "$work/src/common/src/main" && echo "$n" > "$work/src/common/src/main/Identity.scala"
  scratch_git "$work/src" add common && scratch_git "$work/src" commit -q -m "c$n"
done
scratch_git "$work/src" push -q "$work/origin.git" HEAD:refs/heads/main
c4=$(scratch_git "$work/src" rev-parse HEAD~1) c5=$(scratch_git "$work/src" rev-parse HEAD)
rm -rf "$work/job" && mkdir -p "$work/job" && scratch_git "$work/job" clone -q --depth=1 --branch main "file://$work/origin.git" .
mkdir -p "$work/job/test/resources/fixtures" "$work/job/scripts/ci"
printf 'country\tvenue\trawTitle\tfilm\tverdict\tstate\tscope\tfilmTitle\treason\tjudged\tevidence\n' > "$work/job/test/resources/fixtures/identity-judgements.tsv"
cp "$(dirname "$script")"/identity_*.py "$(dirname "$script")/gh-release.sh" "$work/job/scripts/ci/"
cat > "$work/bin/sbt" <<'SBT'
#!/usr/bin/env bash
pwd -P >> "$SBT_CALLS"
if [ -n "${KINOWO_IDENTITY_DUMP:-}" ]; then
  mkdir -p "$KINOWO_IDENTITY_DUMP"
  echo '{"key":"k","venue":"v","rawTitle":"t","film":1,"filmTitle":"F","basis":"b","explanation":[],"blocker":null}' \
    > "$KINOWO_IDENTITY_DUMP/decisions-$KINOWO_IDENTITY_FULL.jsonl"
else
  mkdir -p target/identity-unmatched
  # STUB_GAPS: the capture lacks a lookup this code asks -- the ratchet writes takes only reading it as unknown
  if [ -n "${STUB_GAPS:-}" ]; then
    [ "${KINOWO_RATCHET_CAPTURE_GAPS_UNKNOWN:-}" = 1 ] || { echo "the fixture lacks answers this resolve asks"; exit 1; }
    printf 'country\tkind\tlookup\npl\tfamily\t(RottenTomatoes, record|x)\nuk\ttmdb\tsearch y\n' > target/identity-unmatched/capture-gaps.tsv
  fi
  printf 'country\tvenue\trawTitle\tstatus\tfilm\ttitle\tbasis\npl\tv\tt\tRIGHT\ttmdb:1\tF\tb\n' > target/identity-unmatched/ratchet-takes.tsv
fi
echo "[success] Total time: 1 s"
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
  "release view")     for f in "$work"/release/*; do [ -f "\$f" ] && basename "\$f"; done ;;
  *) exec "$work/bin/gh-history" "\$@" ;;
esac
GH
chmod +x "$work/bin/gh"
store_run() { # <head>: the check of <head>, on main, storing on the release
  rm -rf "$work/out" "$work/identity-takes-base"; : > "$work/sbt-calls"
  (cd "$work/job" && PATH="$work/bin:$PATH" GITHUB_REPOSITORY=o/r GITHUB_SHA=$1 GITHUB_REF=${REF-refs/heads/main} RUNNER_TEMP=$work \
    SBT_CALLS="$work/sbt-calls" TAKES_STORE=fixtures KINOWO_CONVERGENCE_PIN_CORPUS_RUN=${PIN-42} "$script" pl "$work/out" > "$work/log" 2>&1)
}
base_calls() { grep -c "identity-takes-base" "$work/sbt-calls" || true; }
code=0; TAKES_BASE=$c4 store_run "$c5" || code=$?
check "with nothing stored, the base is checked out and resolved" "$(base_calls)" 2
check "...and the check passes on the same takes" "$code" 0
check "...and the head's decisions and final takes, and the base's it resolved, are stored" "$(ls "$work/release" | wc -l | tr -d ' ')" 4
check "...each phase timed, saying which it resolved" "$(cut -f1,3 "$work/out/timings.tsv" | tr '\t\n' ': ')" \
  "phase:how head-resolve:resolved base-resolve:resolved head-final:replayed base-final:replayed "
check "...and why the base was not stored" "$(grep -c 'the base resolve not stored -- takes-decisions-pl-.* not on release fixtures: resolving it' "$work/log")" 1
# (a mktemp that fails: macOS's ignores TMPDIR)
mkdir -p "$work/no-tmp" && printf '#!/usr/bin/env bash\necho "mktemp: no temporary directory" >&2; exit 1\n' > "$work/no-tmp/mktemp"
chmod +x "$work/no-tmp/mktemp"
code=0; PATH="$work/no-tmp:$PATH" TAKES_BASE=$c4 store_run "$c5" || code=$?
check "the check writes nothing outside its out dir: no temporary directory loses no verdict" \
  "$code $(grep -c 'identity final takes (pl): 1 listings' "$work/out/verdict.txt")" "0 1"

# A measure/ branch (scripts/identity-measure) stores the BASE it resolved -- main's commit, under a key naming its code
# and inputs, never the ref -- and never its own head: the next measurement against the same base reuses it.
rm -rf "$work/release"
code=0; REF=refs/heads/measure/c5-1 TAKES_BASE=$c4 store_run "$c5" || code=$?
check "a measurement stores the base it resolved, not its head" "$(ls "$work/release" | wc -l | tr -d ' ')" 2
REF=refs/heads/measure/c5-2 TAKES_BASE=$c4 store_run "$c5" || true
check "...so the next measurement against that base resolves only its head" "$(base_calls)" 0
check "...timing the base as reused" "$(grep -c 'base-resolve.*reused' "$work/out/timings.tsv")" 1
# Two measurements of one base at once: the one whose upload is refused because the other's landed first says so, and
# warns of nothing (a warning there read as "the store is broken").
rm -rf "$work/release"
UPLOAD_RACES=1 REF=refs/heads/measure/c5-3 TAKES_BASE=$c4 store_run "$c5" || true
check "an upload refused because another run stored the asset meanwhile is no warning" \
  "0 1" "$(grep -c 'could not store' "$work/log") $(grep -c 'takes-decisions-pl-.* was stored by another run meanwhile' "$work/log")"

# A capture lacking a lookup the code asks: the final takes are still replayed, the gap read as unknown and named --
# only this row's country's (run 38694072183 lost every country's final takes to one UK Rotten Tomatoes record).
rm -rf "$work/release"
code=0; STUB_GAPS=1 TAKES_BASE=$c4 store_run "$c5" || code=$?
check "a capture lacking a lookup the code asks still has its final takes replayed, and the check passes" "$code" 0
check "...naming the row's own gap as a blind spot" \
  "$(grep -c "the head's final takes read 1 lookup(s) the capture lacks as UNKNOWN.*family (RottenTomatoes, record|x)" "$work/out/verdict.txt")" 1
check "...and not another country's" "$(grep -c 'search y' "$work/out/verdict.txt")" 0
rm -rf "$work/release"
code=0; TAKES_BASE=$c4 store_run "$c5" || code=$?

# c4's own check, then c5's again: its base c4 is now stored.
TAKES_BASE=$c2 store_run "$c4" || true
code=0; TAKES_BASE=$c4 store_run "$c5" || code=$?
check "a base its own run stored is reused, not compiled or resolved" "$(base_calls)" 0
check "...and the head still resolves" "$(grep -vc identity-takes-base "$work/sbt-calls")" 2
check "...and the check still diffs and passes" "$code" 0
check "...saying it reused the base" "$(grep -c 'base resolve reused' "$work/log")" 1

PIN=43 TAKES_BASE=$c4 store_run "$c5" || true
check "another pinned recording is another key: the base's decisions resolve again" "$(base_calls)" 1
check "...its final takes, which read only the capture, still reused" "$(grep -c 'base final takes reused' "$work/log")" 1

echo unpublished > "$work/laid-fill"
TAKES_BASE=$c4 store_run "$c5" || true
check "a fill laid but never published names no key: nothing is reused" "$(base_calls)" 2
rm -f "$work/laid-fill"

# No pinned recording names the decisions' inputs: their key would match another unpinned run's over other data.
rm -rf "$work/release"; PIN="" TAKES_BASE=$c2 store_run "$c4" || true
PIN="" TAKES_BASE=$c4 store_run "$c5" || true
check "with no recording pinned the base's decisions are resolved, never reused from a key naming no recording" "$(base_calls)" 1
check "...nor stored under one" "$(find "$work/release" -name "*decisions*" | wc -l | tr -d " ")" 0

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
check "c6's check, touching no identity code, resolves nothing" "$(grep -c 'no identity code changed' "$work/out/verdict.txt")" 1
at "$c7"; code=0; TAKES_BASE=$c6 store_run "$c7" || code=$?
check "a base that stored nothing, its identity code another's that did, reuses that one's decisions" \
  "$(grep -c 'base resolve reused' "$work/log")" 1
check "...and its final takes" "$(grep -c 'base final takes reused' "$work/log")" 1
check "...compiling and resolving nothing for the base" "$(base_calls)" 0
check "...and the check still passes" "$code" 0

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
case "$*" in
  *corpusShapePoland*)
    pwd -P >> "$SBT_CALLS"
    mkdir -p target/convergence-diagnostics
    case "$(pwd -P)" in *identity-takes-base*) findings=$SHAPE_BASE ;; *) findings=$SHAPE_HEAD ;; esac
    printf '%b' "$findings" > target/convergence-diagnostics/corpus-shape-recorded-pl-SearchQueryMarkers.txt
    exit 1 ;; # a finding fails the specs themselves: their exit is never the verdict
  *) exec "$(dirname "$0")/sbt-takes" "$@" ;;
esac
SBT
chmod +x "$work/bin/sbt"
arrietty='finding\tCineworld Barnsley\tArrietty (4K Restoration) Experience It In IMAX\tsearches it (4k)\n'
preview='finding\tFilmRaum\tLa bola negra , Preview am Mi.\tsearches it (preview)\n'
at "$c9"
code=0; SHAPE_COMMAND=corpusShapePoland SHAPE_HEAD=$arrietty SHAPE_BASE=$arrietty TAKES_BASE=$c8 store_run "$c9" || code=$?
check "an allowlist edit resolves no take" "$(grep -c 'no identity code changed' "$work/out/verdict.txt")" 1
check "...runs the corpus-shape specs at the head and the base" "$(wc -l < "$work/sbt-calls" | tr -d ' ')" 2
check "...and passes on a finding the recording brought, standing at both" "$code" 0
check "...reporting it" "$(grep -c '0 new (the code.s: fail), 1 standing' "$work/out/verdict.txt")" 1
code=0; SHAPE_COMMAND=corpusShapePoland SHAPE_HEAD="$arrietty$preview" SHAPE_BASE=$arrietty TAKES_BASE=$c8 store_run "$c9" || code=$?
check "a finding only the head has fails the check" "$code" 1
check "...naming it" "$(grep -c 'FAIL SearchQueryMarkers finding: FilmRaum' "$work/out/verdict.txt")" 1
# c8's own check: its base c6 predates the findings files, and it stores the head's findings for c9's run to reuse.
at "$c8"; code=0; SHAPE_COMMAND=corpusShapePoland SHAPE_HEAD=$arrietty SHAPE_BASE=$preview TAKES_BASE=$c6 store_run "$c8" || code=$?
check "a base from before the specs wrote findings: the head's are reported, not judged" \
  "$(grep -c 'the base predates the corpus-shape findings' "$work/out/verdict.txt")" 1
check "...and pass" "$code" 0
at "$c9"; code=0; SHAPE_COMMAND=corpusShapePoland SHAPE_HEAD="$arrietty$preview" SHAPE_BASE=$arrietty TAKES_BASE=$c8 store_run "$c9" || code=$?
check "a base whose findings its own run stored is reused, not run" "$(grep -c identity-takes-base "$work/sbt-calls")" 0
check "...and the head's new finding still fails" "$code" 1
code=0; (cd "$work/job" && PATH="$work/bin:$PATH" GITHUB_REPOSITORY=o/r GITHUB_SHA=$c9 RUNNER_TEMP=$work SBT_CALLS="$work/sbt-calls" \
  SHAPE_COMMAND=corpusShapeNowhere TAKES_BASE=$c8 "$script" pl "$work/out" > "$work/log" 2>&1) || code=$?
check "specs that wrote no findings are no verdict, never a pass" "$code" 2

[ "$fails" = 0 ] || { echo "$fails failure(s)"; exit 1; }
