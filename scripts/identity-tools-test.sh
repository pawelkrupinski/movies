#!/usr/bin/env bash
# identity-tools.sh (what the local identity tools share): a capture fetched once into the shared cache from a run's
# artifacts or a git ref, a tree's classpath compiled only when a source is newer, the newest recording holding a
# country, a recording or a ref's tree laid out once under a kernel lock a killed holder cannot strand, and no gh unbounded or
# failure read as an answer.
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
[ -f "$STUB/blip" ] && { rm -f "$STUB/blip"; echo "HTTP 500: Internal Server Error" >&2; exit 1; }
[ -d "$STUB/artifacts/$name" ] || { echo "no artifact matches any of the names or patterns provided" >&2; exit 1; }
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
echo build > "$repo/build.sbt"
scratch_repo "$repo" && scratch_git "$repo" add -A && scratch_git "$repo" commit -qm "capture, judged by labels.tsv"
legacy=$(scratch_git "$repo" rev-parse HEAD)
scratch_git "$repo" rm -q test/resources/fixtures/identity-unmatched/labels.tsv
echo "labels-at-ref" > "$repo/test/resources/fixtures/identity-judgements.tsv"
scratch_git "$repo" add -A && scratch_git "$repo" commit -qm "the judgements, one file"
echo "labels-here" > "$repo/test/resources/fixtures/identity-judgements.tsv"

tools() {
  KINOWO_JVM_GATE_FOLLOW_MAIN=0 STUB="$work" PATH="$work/bin:$PATH" KINOWO_IDENTITY_CACHE="$work/cache" GH_DOWNLOAD_BACKOFF=0 spec_bounded bash -c \
    '. "$1"; IDENTITY_REPO="$2"; shift 2; "$@"' _ "$REPO_ROOT/scripts/identity-tools.sh" "$repo" "$@" 2> "$work/err"
}

check "no source is this checkout's capture" "$repo/test/resources/fixtures/identity-unmatched" "$(tools identity_capture "" pl)"
check "a run id downloads that country's capture into the cache" "de-capture" "$(cat "$(tools identity_capture 987654 de)/de.json.gz")"
check "...judged by this checkout's judgements" "labels-here" "$(cat "$work/cache/captures/run-987654/identity-judgements.tsv")"
rm -f "$work/calls"
tools identity_capture 987654 de > /dev/null
check "...and is not downloaded again" "0" "$(grep -c 'run download' "$work/calls" 2>/dev/null || echo 0)"
touch "$work/blip"
check "a capture download that fails once (a GitHub 5xx) is asked again" "de-capture" \
  "$(cat "$(tools identity_capture 987655 de)/de.json.gz" 2>/dev/null)"
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
rm -f "$work/calls"
touch -t 200001010000 "$tree/worker/src/A.scala"
mkdir -p "$tree/common/src/main/resources"
echo 'row' > "$tree/common/src/main/resources/title-rules.tsv"
touch -t 209901020000 "$tree/common/src/main/resources/title-rules.tsv"
tools identity_classpath "$tree" > /dev/null
check "...and again once a resource (title-rules.tsv, no .s* extension) changes" "1" "$(grep -c '^sbt' "$work/calls" 2>/dev/null || echo 0)"

# The default recording is the newest run holding the country's corpus: the newest run has no UK artifact, an older does.
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
case "$1" in
  # listed out of creation order, as GitHub does: the newest is the latest CREATED
  run) printf '100\t2026-10-01T00:00:00Z\n300\t2026-10-03T00:00:00Z\n200\t2026-10-02T00:00:00Z\n' ;;
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

# A run whose artifacts cannot be read is not skipped for an older one: the answer would be a stale recording.
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
case "$1" in
  run) printf '300\n100\n' ;;
  api) case "$2" in */runs/300/*) echo "HTTP 502" >&2; exit 1 ;; *) echo scrape-fixtures-uk ;; esac ;;
esac
STUB
check "a run whose artifacts cannot be read fails, never falling back to an older one" "1 1" \
  "$(tools identity_newest_recording uk > /dev/null; echo $?) $(grep -c 'could not read recording 300' "$work/err")"
# (the stub never answers: the read fails only by its bound, leaving no gh running -- shell-spec.sh's HUNG STUBS)
printf '#!/usr/bin/env bash\necho $$ >> "%s"\nexec sleep 86400\n' "$work/gh-hung" > "$work/bin/gh"; : > "$work/gh-hung"
check "a gh that hangs is cut off, and fails, saying so" "1 1" \
  "$(KINOWO_IDENTITY_GH_TIMEOUT=1 tools identity_newest_recording uk > /dev/null; echo $?) $(grep -c 'could not list .*(gh failed or timed out)' "$work/err")"
check "...leaving no gh running" "0" "$(spec_still_running "$work/gh-hung")"

# identity_recording: the fetch (a stub of scripts/identity-fetch-recording.sh) lays the corpus out last, as the real one does
mkdir -p "$repo/scripts"
cat > "$repo/scripts/identity-fetch-recording.sh" <<'STUB'
#!/usr/bin/env bash
echo "fetch $*" >> "$STUB/calls"
[ -z "${FETCH_FAILS:-}" ] || exit 1
while :; do case "$1" in --corpus-only) shift ;; --fills) shift 2 ;; *) break ;; esac; done
mkdir -p "$2/pair/test/resources/fixtures/enrichment-$3" "$2/pair/test/resources/fixtures/corpus"
echo corpus > "$2/pair/test/resources/fixtures/corpus/cinema-scrapes-$3.json.gz"
STUB
chmod +x "$repo/scripts/identity-fetch-recording.sh"
fixtures="$work/cache/recordings/500/pair/test/resources/fixtures"
mkdir -p "$fixtures/enrichment-pl"
rm -f "$work/calls"
check "a recording whose tree is there but whose corpus is not (a fetch cut short) is fetched again" "$fixtures corpus" \
  "$(tools identity_recording 500 pl) $(cat "$fixtures/corpus/cinema-scrapes-pl.json.gz" 2>/dev/null)"
rm -f "$work/calls"
tools identity_recording 500 pl > /dev/null
check "...then read from the cache" "0" "$(grep -c '^fetch' "$work/calls" 2>/dev/null || echo 0)"
check "--corpus-only fetches the scrape corpus alone" "fetch --corpus-only 501 $work/cache/recordings/501 pl" \
  "$(tools identity_recording 501 pl corpus > /dev/null; grep '^fetch' "$work/calls")"
# a pair with a hermetic leg's fills laid over it is cached apart from the bare pair: the two answer differently
rm -f "$work/calls"
filled=$(IDENTITY_FILLS="fill-pl-505-1-1-convergence.tar.zst" tools identity_recording 505 pl)
check "IDENTITY_FILLS fetches the recording with those fills" "1" \
  "$(grep -c '^fetch --fills fill-pl-505-1-1-convergence.tar.zst 505 '"$work"'/cache/recordings/505-fills-' "$work/calls")"
case "$filled" in "$work/cache/recordings/505-fills-"*/pair/test/resources/fixtures) apart="filled-apart" ;; *) apart=$filled ;; esac
check "...into a directory of its own, apart from the bare pair" "filled-apart" "$apart"
check "...and the bare pair stays where it was" "$work/cache/recordings/505/pair/test/resources/fixtures" "$(tools identity_recording 505 pl)"
check "a fetch that fails fails, handing back no directory" "1 " "$(out=$(FETCH_FAILS=1 tools identity_recording 502 pl); echo "$? $out")"
check "...and leaves its lock free" "free" \
  "$(spec_lock_state "$work/cache/locks/recording-502-pl.flock")"
# a lock whose holder was killed (kill -9: no trap ran) is free at once: nothing to take over, nothing waited on
spec_lock_holder "$work/cache/locks/recording-503-pl.flock"; dead=$SPEC_LOCK_HOLDER
kill -9 "$dead"; wait "$dead" 2>/dev/null
check "a lock whose holder was killed is free at once" "0 0" \
  "$(KINOWO_IDENTITY_LOCK_WAIT=0 tools identity_recording 503 pl > /dev/null; echo $?) $(grep -c 'waiting' "$work/err")"
# agents racing for one lock: one runs under it at a time -- never two inside
# shellcheck disable=SC2016 # the command's own $1
printf '%s\n' 'echo in >> "$1"; sleep 0.3; echo out >> "$1"' > "$work/inside"
for i in $(seq 1 12); do
  ( touch "$work/ready$i"; while [ ! -e "$work/go" ]; do :; done; tools identity_locked race sh "$work/inside" "$work/inside.log" ) &
done
wait_until "every racer is ready" '[ "$(ls "$work"/ready* 2>/dev/null | wc -l)" -eq 12 ]'
touch "$work/go"; wait
check "12 agents racing for one lock run under it one at a time" "12 in,out" \
  "$(grep -c in "$work/inside.log") $(paste -sd, - < "$work/inside.log" | sed 's/\(in,out,\)*in,out$/in,out/')"
# a live holder is waited for, but not forever
spec_lock_holder "$work/cache/locks/recording-504-pl.flock"; live=$SPEC_LOCK_HOLDER
check "a live holder is waited for until the deadline, then this fails naming the lock and its holder" "1 1" \
  "$(KINOWO_IDENTITY_LOCK_WAIT=2 tools identity_recording 504 pl > /dev/null; echo $?) $(grep -c "gave up after .*held by pid $live; .*recording-504-pl" "$work/err")"
kill "$live" 2>/dev/null; wait "$live" 2>/dev/null

# The lock names its holder: the subshell running the command, a live pid -- also under macOS's /bin/bash 3.2 and set -u
# (no $BASHPID there).
cat > "$work/holder" <<'HOLDER'
holder=$(cat "$KINOWO_IDENTITY_CACHE/locks/held.flock")
if [ "$holder" = "$PPID" ] && kill -0 "$holder"; then echo same; else echo "holder <$holder>, runner $PPID"; fi
HOLDER
check "a held lock names the holder running the command, under set -u" "same 0" \
  "$(out=$(KINOWO_IDENTITY_CACHE="$work/cache" bash -c 'set -u; . "$1"; identity_locked held sh "$2"' _ "$REPO_ROOT/scripts/identity-tools.sh" "$work/holder" 2>&1); echo "$out $?")"
check "...and lets it go, its pid cleared" "free " \
  "$(spec_lock_state "$work/cache/locks/held.flock") $(cat "$work/cache/locks/held.flock")"

# identity_tree: a layout cut short leaves no tree another call takes for whole
head=$(scratch_git "$repo" rev-parse HEAD)
mkdir -p "$work/cache/refs/$head/stray"
check "a ref's tree left half laid out is laid out again, whole" "build" "$(cat "$(tools identity_tree HEAD)/build.sbt" 2>/dev/null)"
check "...not nested inside the half" "0" "$(find "$work/cache/refs/$head" -maxdepth 1 -name '*.part' | wc -l | tr -d ' ')"
rm -rf "$work/cache/refs"
( tools identity_tree HEAD > "$work/t1" ) & ( tools identity_tree HEAD > "$work/t2" ) & wait
check "two agents laying out one ref at once get one whole tree" "build build 1" \
  "$(cat "$(cat "$work/t1")/build.sbt") $(cat "$(cat "$work/t2")/build.sbt") $(find "$work/cache/refs" -mindepth 1 -maxdepth 1 | wc -l | tr -d ' ')"

# two checkouts whose tool files differ never share a tree: the first claims refs/<sha>, the other gets its own
rm -rf "$work/cache/refs"
echo v1 > "$repo/tool.txt"
base=$(tools identity_tree HEAD tool.txt)
echo v2 > "$repo/tool.txt"
variant=$(tools identity_tree HEAD tool.txt)
echo v1 > "$repo/tool.txt"
check "a checkout with other tool files gets a tree of its own, the first one's untouched" "v1 v2 yes" \
  "$(cat "$base/tool.txt") $(cat "$variant/tool.txt") $([ "$base" != "$variant" ] && echo yes || echo no)"
check "...and the same tool files again reach the first tree" "$base" "$(tools identity_tree HEAD tool.txt)"
check "...whose tool file stayed the first's" "v1" "$(cat "$base/tool.txt")"
rm -f "$repo/tool.txt"
rm -rf "$work/cache/refs"

# identity_capture: an artifact lacking the country's capture is a failure, not an empty directory
mkdir -p "$work/artifacts/identity-capture-uk"
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
while [ "$#" -gt 0 ]; do case "$1" in --dir) dir="$2" ;; --name) name="$2" ;; esac; shift; done
[ -d "$STUB/artifacts/$name" ] || { echo "no artifact matches any of the names or patterns provided" >&2; exit 1; }
mkdir -p "$dir"; cp -R "$STUB/artifacts/$name/." "$dir/"
STUB
check "a run's artifact holding no capture of the country fails" "1" "$(tools identity_capture 987655 uk > /dev/null; echo $?)"
check "a run's capture that cannot be downloaded fails" "1" "$(tools identity_capture 987656 pl > /dev/null; echo $?)"

# scripts/tool-conventions.sh, which identity-tools.sh sources: what every Bash tool shares (tested here, beside its
# first user, rather than in a self-test of its own).
printf '\033[36m▸\033[0m tool-conventions.sh\n'
conventions="$REPO_ROOT/scripts/tool-conventions.sh"
# a command that never ends on its own, so bounded returns only by its bound -- reported from inside the outer bound,
# whose own 124 would otherwise look the same (shell-spec.sh's HUNG STUBS)
check "bounded ends a command that outlasts it, 124" "inner 124" \
  "$(spec_bounded bash -c '. "$1"; bounded 1 sleep 86400; echo "inner $?"' _ "$conventions")"
# stock macOS: no timeout(1) on the PATH, only perl
mkdir -p "$work/bare"
for c in perl sleep bash; do ln -sf "$(command -v "$c")" "$work/bare/$c"; done
bare() { PATH="$work/bare" "$work/bare/bash" -c '. "$1"; shift; "$@"' _ "$conventions" "$@"; }
check "without timeout(1) (stock macOS) bounded still ends it, 124" "124" "$(bare bounded 1 sleep 20; echo $?)"
check "...and hands back a command's own status" "3" "$(bare bounded 5 bash -c 'exit 3'; echo $?)"
bare bounded 1 bash -c 'sleep 30 & echo $! > "$1"; wait' _ "$work/grandchild" > /dev/null
check "...ending its whole process group: what it started too" "gone" \
  "$(if kill -0 "$(cat "$work/grandchild")" 2>/dev/null; then echo alive; kill "$(cat "$work/grandchild")"; else echo gone; fi)"
# Ctrl-C: the terminal signals bounded itself (GNU timeout leaves the terminal's group, and a Ctrl-C never reached
# what it ran): it ends the command's group and is ended by the signal, as the caller expects.
rm -f "$work/grandchild" "$work/command"
bash -c '. "$1"; bounded 30 bash -c "sleep 30 & echo \$! > $2; echo \$\$ > $3; wait"' _ "$conventions" "$work/grandchild" "$work/command" &
caller=$!
wait_until "the command and its grandchild recorded their pids" '[ -s "$work/command" ] && [ -s "$work/grandchild" ]'
kill -INT "$(ps -o ppid= -p "$(cat "$work/command")" | tr -d ' ')"
wait "$caller"; status=$?
check "a Ctrl-C to bounded ends the command's whole group, the caller seeing the interrupt" "gone 130" \
  "$(if kill -0 "$(cat "$work/grandchild")" 2>/dev/null; then echo alive; kill "$(cat "$work/grandchild")"; else echo gone; fi) $status"
printf '#!/usr/bin/env bash\n#\n# Do one thing.\n#   -x  extra\nset -euo pipefail\necho body\n' > "$work/tool"
check "usage_from_header prints the header comment, unprefixed" "Do one thing.|  -x  extra" \
  "$(bash -c '. "$1"; usage_from_header "$2"' _ "$conventions" "$work/tool" | paste -sd'|' -)"
printf '#!/usr/bin/env bash\n# Starts on line two.\nset -uo pipefail\n' > "$work/tool2"
check "...from line two when the header starts there" "Starts on line two." \
  "$(bash -c '. "$1"; usage_from_header "$2"' _ "$conventions" "$work/tool2")"
check "the countries, in one order" "pl,de,uk,us,es" "$(bash -c '. "$1"; kinowo_countries_csv' _ "$conventions")"
check "the identity cache defaults under the home, exported to what a tool starts" "/h/.cache/kinowo-identity" \
  "$(env -u KINOWO_IDENTITY_CACHE HOME=/h bash -c '. "$1"; bash -c "echo \$KINOWO_IDENTITY_CACHE"' _ "$conventions")"
check "identity-tools.sh's cache is that one" "/h/.cache/kinowo-identity" \
  "$(env -u KINOWO_IDENTITY_CACHE HOME=/h bash -c '. "$1"; echo "$IDENTITY_CACHE"' _ "$REPO_ROOT/scripts/identity-tools.sh")"

check "identity_country_code and identity_country_name are inverses over the five" "pl de uk us es|poland germany united-kingdom united-states spain" \
  "$(bash -c '. "$1"; for n in poland germany united-kingdom united-states spain; do identity_country_code $n; done | paste -sd" " -; for c in pl de uk us es; do identity_country_name $c; done | paste -sd" " -' _ "$REPO_ROOT/scripts/identity-tools.sh" | paste -sd'|' -)"
ov="$work/overlay"
scratch_repo "$ov" -b main
ov_commit() { scratch_git "$ov" add -A && scratch_git "$ov" -c user.email=t@t -c user.name=t commit -qm "$1"; }
mkdir -p "$ov/.github" "$ov/scripts/ci" && echo old > "$ov/.github/w.yml" && echo old > "$ov/scripts/ci/x.sh" && echo own > "$ov/code.txt"
ov_commit old && scratch_git "$ov" branch old
echo new > "$ov/.github/w.yml" && echo new > "$ov/scripts/ci/y.sh" && rm "$ov/scripts/ci/x.sh" && echo main > "$ov/code.txt"
ov_commit new && scratch_git "$ov" update-ref refs/remotes/r/main HEAD
over=$(cd "$ov" && env -u GIT_DIR -u GIT_INDEX_FILE -u GIT_WORK_TREE GIT_AUTHOR_NAME=t GIT_AUTHOR_EMAIL=t@t GIT_COMMITTER_NAME=t \
  GIT_COMMITTER_EMAIL=t@t bash -c '. "$1"; identity_main_ci_over r old drift' _ "$REPO_ROOT/scripts/identity-tools.sh")
check "identity_main_ci_over: the commit's own code under main's .github/ and scripts/ci/, a child of the commit" \
  "own new new|drift: r/main's CI over" \
  "$(scratch_git "$ov" show "$over:code.txt") $(scratch_git "$ov" show "$over:.github/w.yml") $(scratch_git "$ov" show "$over:scripts/ci/y.sh")|$(scratch_git "$ov" log -1 --format=%s "$over" | cut -c1-23)"
check "...main's scripts/ci/ whole: a file main dropped is gone" "absent" \
  "$(scratch_git "$ov" cat-file -e "$over:scripts/ci/x.sh" 2> /dev/null && echo present || echo absent)"
check "...its parent the commit" "true" "$([ "$(scratch_git "$ov" rev-parse "$over^")" = "$(scratch_git "$ov" rev-parse old)" ] && echo true || echo false)"

check "identity_measure_branch: measure/<user>/<short sha>-<tag>-<random>" "true" \
  "$(bash -c '. "$1"; identity_measure_branch 0123456789abcdef drift' _ "$REPO_ROOT/scripts/identity-tools.sh" | grep -qE '^measure/[A-Za-z0-9._-]+/012345678-drift-[0-9a-f]{6}$' && echo true || echo false)"
mkdir -p "$work/home/bin"
printf '#!/usr/bin/env bash\necho "$*" > "%s/recorded"\n' "$work" > "$work/home/bin/agentctl"; chmod +x "$work/home/bin/agentctl"
dispatch_case() { # <dispatch rc> <find rc>: runs identity_dispatch_measure over stub dispatch functions
  HOME="$work/home" bash -c '. "$1"; d_rc=$2 f_rc=$3 w=$4
    dispatch_workflow() { echo "dispatch $*" >> "$w/dispatched"; return "$d_rc"; }
    find_dispatched_run() { DISPATCHED_RUN=901; return "$f_rc"; }
    identity_dispatch_measure identity-model-convergence.yml measure/x/b abc123 "a purpose" -f drift-only=true \
      && echo "run $DISPATCHED_RUN" || echo "failed: $IDENTITY_DISPATCH_ERROR"' _ "$REPO_ROOT/scripts/identity-tools.sh" "$1" "$2" "$work" 2> /dev/null
}
rm -f "$work/recorded" "$work/dispatched"
check "identity_dispatch_measure: dispatched with its inputs, the run found, and recorded as this machine's" \
  "run 901|dispatch identity-model-convergence.yml measure/x/b -f drift-only=true|dispatch record --branch measure/x/b --run 901 --workflow Identity model convergence --purpose a purpose" \
  "$(dispatch_case 0 0)|$(cat "$work/dispatched")|$(cat "$work/recorded")"
rm -f "$work/recorded"
check "...a dispatch refused says so, and records nothing" "failed: could not dispatch identity-model-convergence.yml on measure/x/b|none" \
  "$(dispatch_case 1 0)|$([ -f "$work/recorded" ] && echo recorded || echo none)"
check "...a run not found is told apart: the branch is the run's to delete" "failed: dispatched" "$(dispatch_case 0 1 | cut -c1-18)"

# kept_jvm_call: one request to a kept JVM (tools.KeptJvm's framing), its answer streamed, its exit code returned
check "kept_jvm_call: nothing listening is 111" "111" "$(bash -c '. "$1"; kept_jvm_call "$2/none.sock" a; echo $?' _ "$REPO_ROOT/scripts/identity-tools.sh" "$work" 2> /dev/null)"
sock="$work/kept.sock"
python3 -I - "$sock" <<'PY' &
import os, socket, struct, sys
s = socket.socket(socket.AF_UNIX); s.bind(sys.argv[1]); s.listen(1)
c, _ = s.accept()
n = struct.unpack(">i", c.recv(4))[0]
data = b""
while len(data) < n:
    data += c.recv(n - len(data))
args = data.decode().split("\0")
c.sendall(("got " + " ".join(args) + "\n").encode() + b"\0EXIT 7\n"); c.close()
PY
server=$!
for _ in $(seq 1 50); do [ -S "$sock" ] && break; sleep 0.1; done
check "...a request's args reach the JVM, its answer streams back and its exit code is returned" "got de --out /x two words|7" \
  "$(bash -c '. "$1"; kept_jvm_call "$2" de --out /x "two words"; echo "|$?"' _ "$REPO_ROOT/scripts/identity-tools.sh" "$sock" 2> /dev/null | tr -d '\n')"
wait "$server" 2> /dev/null

spec_summary
