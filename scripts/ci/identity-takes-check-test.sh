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
mkdir -p "$work/job/test/resources/fixtures/identity-unmatched" "$work/job/scripts/ci"
: > "$work/job/test/resources/fixtures/identity-unmatched/labels.tsv"
cp "$(dirname "$script")/identity_takes_diff.py" "$(dirname "$script")/gh-release.sh" "$work/job/scripts/ci/"
cat > "$work/bin/sbt" <<'SBT'
#!/usr/bin/env bash
pwd -P >> "$SBT_CALLS"
if [ -n "${KINOWO_IDENTITY_DUMP:-}" ]; then
  mkdir -p "$KINOWO_IDENTITY_DUMP"
  echo '{"key":"k","venue":"v","rawTitle":"t","film":1,"filmTitle":"F","basis":"b","explanation":[],"blocker":null}' \
    > "$KINOWO_IDENTITY_DUMP/decisions-$KINOWO_IDENTITY_FULL.jsonl"
else
  mkdir -p target/identity-unmatched
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
  "release upload")   mkdir -p "$work/release" && cp "\$4" "$work/release/" ;;
  "release view")     echo ;;
  *) exec "$work/bin/gh-history" "\$@" ;;
esac
GH
chmod +x "$work/bin/gh"
store_run() { # <head>: the check of <head>, on main, storing on the release
  rm -rf "$work/out" "$work/identity-takes-base"; : > "$work/sbt-calls"
  (cd "$work/job" && PATH="$work/bin:$PATH" GITHUB_REPOSITORY=o/r GITHUB_SHA=$1 GITHUB_REF=refs/heads/main RUNNER_TEMP=$work \
    SBT_CALLS="$work/sbt-calls" TAKES_STORE=fixtures KINOWO_CONVERGENCE_PIN_CORPUS_RUN=${PIN:-42} "$script" pl "$work/out" > "$work/log" 2>&1)
}
base_calls() { grep -c "identity-takes-base" "$work/sbt-calls" || true; }
code=0; TAKES_BASE=$c4 store_run "$c5" || code=$?
check "with nothing stored, the base is checked out and resolved" "$(base_calls)" 2
check "...and the check passes on the same takes" "$code" 0
check "...and the head's decisions and final takes are stored for the run whose base it becomes" "$(ls "$work/release" | wc -l | tr -d ' ')" 2

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

[ "$fails" = 0 ] || { echo "$fails failure(s)"; exit 1; }
