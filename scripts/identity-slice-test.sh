#!/usr/bin/env bash
# scripts/identity-slice: a log line read into its listing and build, each ref laid out once with the tool's files
# dropped in and run on ONE recording, the two refs' takes diffed and tallied against the labels, a ref the clone does
# not hold refused by name. java, sbt and gh are stubs; the repository is a scratch one.
# Run: bash scripts/identity-slice-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
# the scratch repository below is built only through the isolating helper: run from a hook, a bare git would act on
# the repository being pushed
. "$REPO_ROOT/scripts/scratch-git.sh"

printf '\033[36m▸\033[0m identity-slice\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin"
# xargs as GNU's, CI's: with no input it still runs its command once -- macOS's runs nothing -- so a key hashing "no
# untracked file" through `xargs shasum` hashed shasum's empty stdin on Linux alone (main 38707274817, green on a Mac).
real_xargs=$(command -v xargs)
cat > "$work/bin/xargs" <<XARGS
#!/usr/bin/env bash
input=\$(mktemp); cat > "\$input"
if [ ! -s "\$input" ]; then
  while [ "\${1:-}" != "\${1#-}" ]; do shift; done
  rm -f "\$input"; exec "\$@" < /dev/null
fi
exec "$real_xargs" "\$@" < "\$input"
XARGS
chmod +x "$work/bin/xargs"

# A repository holding the tool (its scripts and the files it drops into a ref's tree) at two commits.
repo="$work/repo"
mkdir -p "$repo/scripts" "$repo/worker/src/test/scala/scripts" "$repo/worker/src/test/scala/services/identity" \
  "$repo/worker/src/fixtures/scala/tools" "$repo/common/src" "$repo/identity/src" "$repo/testkit/src" "$repo/test/resources/fixtures/identity-unmatched"
cp "$REPO_ROOT/scripts/identity-slice" "$REPO_ROOT/scripts/identity-explain" "$REPO_ROOT/scripts/identity-tools.sh" "$REPO_ROOT/scripts/tool-conventions.sh" "$REPO_ROOT/scripts/jvm-gate.sh" "$REPO_ROOT/scripts/identity_slice_diff.py" "$repo/scripts/"
mkdir -p "$repo/scripts/ci" && cp "$REPO_ROOT"/scripts/ci/identity_*.py "$repo/scripts/ci/"  # the shared modules the diff imports
for f in worker/src/test/scala/scripts/IdentitySlice.scala worker/src/test/scala/scripts/IdentityExplain.scala \
  worker/src/test/scala/services/identity/IdentitySlicing.scala worker/src/fixtures/scala/tools/RecordedCorpus.scala \
  worker/src/fixtures/scala/tools/SliceHttp.scala worker/src/fixtures/scala/tools/MemoLookups.scala worker/src/fixtures/scala/tools/CatalogueFill.scala \
  worker/src/fixtures/scala/tools/FiledAnswers.scala worker/src/fixtures/scala/tools/ProdMirror.scala \
  worker/src/fixtures/scala/tools/IdentityJudgements.scala; do
  echo "// $f" > "$repo/$f"
done
echo "build" > "$repo/build.sbt"
# the mirror's freshness check: records how it was asked, answers as $FRESHNESS_EXIT says
mkdir -p "$repo/scripts/local-mirror"
cat > "$repo/scripts/local-mirror/freshness" <<'STUB'
#!/usr/bin/env bash
echo "freshness $*" >> "$STUB/calls"
echo "de  kinowo_de_prod_mirror: synced …"
exit "${FRESHNESS_EXIT:-0}"
STUB
chmod +x "$repo/scripts/local-mirror/freshness"
printf 'country\tvenue\trawTitle\tfilm\tverdict\tstate\tscope\tfilmTitle\treason\tjudged\tevidence\n' > "$repo/test/resources/fixtures/identity-judgements.tsv"
scratch_repo "$repo" && scratch_git "$repo" add -A && scratch_git "$repo" commit -qm base
echo "// head" >> "$repo/build.sbt"
scratch_git "$repo" commit -qam head
base=$(scratch_git "$repo" rev-parse HEAD~1) head=$(scratch_git "$repo" rev-parse HEAD)

# A recording already in the cache: nothing is downloaded.
cache="$work/cache"
mkdir -p "$cache/recordings/777/pair/test/resources/fixtures/corpus" "$cache/recordings/777/pair/test/resources/fixtures/enrichment-de"
echo corpus > "$cache/recordings/777/pair/test/resources/fixtures/corpus/cinema-scrapes-de.json.gz"
echo '{"recordedAt":"2026-10-14T05:00:00Z"}' > "$cache/recordings/777/pair/test/resources/fixtures/corpus/prod-coverage-de.json"

# sbt prints a classpath, as `export` does; java records its arguments and writes the takes the base and head refs take
cat > "$work/bin/sbt" <<'STUB'
#!/usr/bin/env bash
echo "sbt $PWD" >> "$STUB/calls"
echo "$PWD/worker/target/scala-3.9.0/test-classes:/lib/a.jar"
STUB
cat > "$work/bin/java" <<STUB
#!/usr/bin/env bash
echo "java \$PWD \$*" >> "$work/calls"
out="" ref="" serve=""
while [ "\$#" -gt 0 ]; do case "\$1" in --out) out=\$2 ;; --ref) ref=\$2 ;; --serve) serve=\$2 ;; esac; shift; done
# --serve: a kept JVM, answering each request on its socket (the batch's own arguments) until it is stopped
[ -z "\$serve" ] || [ -z "\${STUB_SERVE_HANG:-}" ] || exec sleep 60   # alive, never listening
[ -z "\$serve" ] || exec python3 -I "$work/serve.py" "\$serve" "$work/calls" scripts.IdentitySlice   # a kept JVM's command line names its main class
[ -n "\$out" ] || { echo "explained"; exit 0; }
h='selector\tkey\tvenue\trawTitle\tfilm\ttitle\tbasis\tconfidence\tmodel\trules\tblocker\trunnerUp\tverdict\texplanation'
{ printf "\$h\n"
  if [ "\$ref" = "$base" ]; then
    printf 'k\tKino|Lalka\tKino\tLalka\ttmdb:1\tLalka (1968)\tOwnMatch\t0.9\ttmdb:1\t\t\t\tWRONG\twhy\n'
    printf 'k\tKino|Emil\tKino\tEmil\ttmdb:2\tEmil (2001)\tOwnMatch\t0.9\ttmdb:2\t\t\t\tRIGHT\twhy\n'
    printf 'k\tKino|Sinn\tKino\tSinn\tnone\t\tNoCandidate\t0\tnone\t\t\t\t-\twhy\n'
  else
    printf 'k\tKino|Lalka\tKino\tLalka\tnone\t\tBelowThreshold\t0.1\tnone\t\t\t\t-\twhy not\n'
    printf 'k\tKino|Emil\tKino\tEmil\tnone\t\tBelowThreshold\t0.1\tnone\t\t\t\t-\twhy not\n'
    printf 'k\tKino|Sinn\tKino\tSinn\ttmdb:9\tSinn (1990)\tOwnMatch\t0.8\ttmdb:9\t\t\t\tWRONG\twhy\n'
  fi; } > "\$out"
echo "decided"
[ -z "\${STUB_INCOMPLETE:-}" ] || { echo "SLICE INCOMPLETE" >&2; exit 3; }
# the script edited while it runs (a rebase, a pull, an agent's edit): its text shifted under bash's read offset
[ -z "\${STUB_REWRITE:-}" ] || { { printf '%09000d\n' 0 | tr 0 '#'; cat "\$STUB_REWRITE"; } > "\$STUB_REWRITE.new"; cat "\$STUB_REWRITE.new" > "\$STUB_REWRITE"; }
STUB
cat > "$work/serve.py" <<'PY'
import os, socket, struct, sys
path, calls = sys.argv[1], sys.argv[2]
s = socket.socket(socket.AF_UNIX); s.bind(path); s.listen(1)
while True:
    c, _ = s.accept()
    n = struct.unpack(">i", c.recv(4))[0]
    data = b""
    while len(data) < n:
        data += c.recv(n - len(data))
    args = data.decode().split("\0")
    open(calls, "a").write("request " + " ".join(args) + "\n")
    out = args[args.index("--out") + 1]
    open(out, "w").write("selector\tkey\tvenue\trawTitle\tfilm\ttitle\tbasis\tconfidence\tmodel\trules\tblocker\trunnerUp\tverdict\texplanation\n"
                         "k\tKino|Lalka\tKino\tLalka\ttmdb:1\tLalka (1968)\tOwnMatch\t0.9\ttmdb:1\t\t\t\tWRONG\twhy\n")
    c.sendall(b"decided by the kept JVM\n\0EXIT 0\n"); c.close()
PY
chmod +x "$work/bin/sbt" "$work/bin/java"

slice() { STUB="$work" PATH="$work/bin:$PATH" KINOWO_IDENTITY_CACHE="$cache" "$repo/scripts/identity-slice" "$@" 2> "$work/err"; }

out=$(slice de --recording 777 --key 'Kino|Lalka' --ref HEAD~1 --ref HEAD)
check "both refs are laid out once in the cache" "2" "$(find "$cache/refs" -mindepth 1 -maxdepth 1 -type d | wc -l | tr -d ' ')"
check "...with the tool's files dropped in" "// worker/src/test/scala/scripts/IdentitySlice.scala" "$(cat "$cache/refs/$base/worker/src/test/scala/scripts/IdentitySlice.scala")"
check "each ref decides on the SAME recording" "2" "$(grep -c -- "--recording $cache/recordings/777/pair/test/resources/fixtures" "$work/calls")"
check "...each reading prod's filed answers from this checkout's capture" "2" "$(grep -c -- "--families $repo/test/resources/fixtures/identity-unmatched/de.json.gz" "$work/calls")"
check "...each under its own commit" "1" "$(grep -c -- "--ref $head" "$work/calls")"
check "the moves are tallied against the labels" "TALLY cured 1, lost 1, new-wrong 1, won 0, moved (unjudged) 0" "$(grep '^TALLY' <<< "$out")"
check "...each move shown with its verdict at both refs" "1" "$(grep -c 'Kino | Emil: tmdb:2 .Emil (2001). via OwnMatch \[RIGHT\] -> none via BelowThreshold \[-\]' <<< "$out")"

rm -f "$work/calls"
slice --log '{"country":"de","venue":"Kino","listing":"Lalka","build":"'"$base"'","projection_mode":"changed","kind":"lost"}' --recording 777 > /dev/null
check "a log line names the listing exactly" "1" "$(grep -c -- '--key Kino|Lalka' "$work/calls")"
check "...and decides it at the build that logged it" "1" "$(grep -c -- "--ref $base" "$work/calls")"

check "a log line naming no listing is refused" "2" "$(slice --log '{"country":"de"}' > /dev/null; echo $?)"
check "a ref the clone does not hold is refused by name" "1" "$(slice de --recording 777 --key 'Kino|Lalka' --ref 0123456789abcdef > /dev/null; grep -c 'no commit 0123456789abcdef' "$work/err")"
check "a batch file is handed to the JVM by its absolute path" "1" "$(cd "$work" && printf 'Kino\tLalka\n' > b.tsv && slice de --recording 777 --batch b.tsv --ref HEAD > /dev/null; grep -c -- "--batch $work/b.tsv" "$work/calls")"
out=$(cd "$work" && STUB_INCOMPLETE=1 slice de --recording 777 --batch b.tsv --ref HEAD~1 --ref HEAD)
check "a batch whose slices met a request they lack still diffs the two refs, the incompleteness said at each ref" "1 2" \
  "$(grep -c '^TALLY' <<< "$out") $(grep -c 'incomplete' "$work/err")"

check "--out keeps the takes of the (last) ref's batch, for another tool to read" "1" \
  "$(cd "$work" && slice de --recording 777 --batch b.tsv --out "$work/takes.tsv" > /dev/null; grep -c $'^k\tKino|Sinn\tKino\tSinn\ttmdb:9' "$work/takes.tsv")"
check "an --out in a directory that is not there is a usage error, never a file written at / instead" "2 1" \
  "$(cd "$work" && slice de --recording 777 --batch b.tsv --out "$work/nope/takes.tsv" > /dev/null; echo $?) $(grep -c "no directory $work/nope" "$work/err")"

cp "$repo/scripts/identity-slice" "$work/identity-slice.orig"
out=$(STUB_REWRITE="$repo/scripts/identity-slice" slice de --recording 777 --key 'Kino|Lalka' --ref HEAD~1 --ref HEAD); code=$?
cp "$work/identity-slice.orig" "$repo/scripts/identity-slice"
check "the script edited while it runs finishes as it started" "0 1" "$code $(grep -c '^TALLY' <<< "$out")"

# this checkout's code (no --ref) is keyed by WHICH checkout, its commit and its uncommitted code: the whole-corpus index
# cached under that key is never another worktree's, an older commit's or an earlier edit's
refOf() { grep -o -- '--ref worktree[^ ]*' "$work/calls" | tail -1; }
rm -f "$work/calls"; slice de --recording 777 --key 'Kino|Lalka' > /dev/null; here=$(refOf)
scratch_git "$repo" worktree add -q "$work/wt2" HEAD~1
rm -f "$work/calls"; STUB="$work" PATH="$work/bin:$PATH" KINOWO_IDENTITY_CACHE="$cache" "$work/wt2/scripts/identity-slice" de --recording 777 --key 'Kino|Lalka' > /dev/null 2>&1; there=$(refOf)
check "two worktrees at different commits never share a key" "yes" "$([ -n "$here" ] && [ -n "$there" ] && [ "$here" != "$there" ] && echo yes || echo "no: '$here' '$there'")"
echo "// edited" >> "$repo/worker/src/test/scala/services/identity/IdentitySlicing.scala"
rm -f "$work/calls"; slice de --recording 777 --key 'Kino|Lalka' > /dev/null; edited=$(refOf)
check "...nor one checkout before and after an uncommitted edit of its code" "yes" "$([ "$here" != "$edited" ] && echo yes || echo "no: '$here'")"
scratch_git "$repo" checkout -q -- worker/src/test/scala/services/identity/IdentitySlicing.scala
rm -f "$work/calls"; slice de --recording 777 --key 'Kino|Lalka' > /dev/null
check "...and the same checkout at the same code keeps its key" "$here" "$(refOf)"

rm -f "$work/calls" "$work/sbt-calls"
check "--compiles lays a ref out with the tool files and compiles it, deciding nothing" "0 0" \
  "$(slice --compiles HEAD~1 > /dev/null; echo $?) $(grep -c . "$work/calls" 2>/dev/null || echo 0)"

# each slice's result is kept for every checkout asking it again: per recording, CODE (by content) and filed answers
rm -f "$work/calls"; slice de --recording 777 --key 'Kino|Lalka' --ref HEAD --threads 3 > /dev/null
check "a ref's results are kept per recording, code and filed answers; --threads reaches the JVM" "1 1" \
  "$(grep -c -- "--results $cache/slice/777/results/code-[0-9a-f]\{24\}-none " "$work/calls") $(grep -c -- '--threads 3' "$work/calls")"

# THE CODE KEY: the whole-corpus index and the results are keyed by the code a ref runs, never its commit -- a commit
# changing no code (a judgement, a doc) resolves nothing again (2026-10-15: 252 indexes under one recording, 12 distinct)
keyOf() { grep -o -- '--index-key [^ ]*' "$work/calls" | tail -1; }
rm -f "$work/calls"; slice de --recording 777 --key 'Kino|Lalka' --ref HEAD > /dev/null; at_head=$(keyOf)
echo "a judgement" > "$repo/test/resources/fixtures/identity-judgements.tsv.note"; echo "docs" > "$repo/README.md"
scratch_git "$repo" add -A && scratch_git "$repo" commit -qm "no code"
docs=$(scratch_git "$repo" rev-parse HEAD)
rm -f "$work/calls"; slice de --recording 777 --key 'Kino|Lalka' --ref "$docs" > /dev/null
check "a commit changing no code reads the index its parent's code made" "$at_head" "$(keyOf)"
check "...under a key naming the code, not a commit" "yes" "$([[ "$at_head" == "--index-key code-"* ]] && echo yes || echo "no: '$at_head'")"
rm -f "$work/calls"; slice de --recording 777 --key 'Kino|Lalka' --ref "$base" > /dev/null
check "a commit changing code (build.sbt) reads another" "yes" "$([ "$(keyOf)" != "$at_head" ] && echo yes || echo no)"
rm -f "$work/calls"; slice de --recording 777 --key 'Kino|Lalka' > /dev/null; mine=$(keyOf)
check "this checkout's code, unedited, is its commit's" "$at_head" "$mine"
mkdir -p "$repo/identity/src/main"; echo "// edited" >> "$repo/identity/src/main/Resolver.scala"
rm -f "$work/calls"; slice de --recording 777 --key 'Kino|Lalka' > /dev/null
check "...and an uncommitted edit of its code is other code" "yes" "$([ "$(keyOf)" != "$mine" ] && echo yes || echo no)"
rm -rf "$repo/identity/src/main"
echo "// a new tool" >> "$repo/worker/src/test/scala/scripts/IdentitySlice.scala"
rm -f "$work/calls"; slice de --recording 777 --key 'Kino|Lalka' --ref "$docs" > /dev/null
check "...as is an edit of the tool files dropped into every ref" "yes" "$([ "$(keyOf)" != "$at_head" ] && echo yes || echo no)"
scratch_git "$repo" checkout -q -- worker/src/test/scala/scripts/IdentitySlice.scala

# THE COMPILE SEED: a new ref's tree is seeded from the nearest compiled one (this checkout has none compiled here), and
# one with the very sources of a compiled ref keeps its classpath -- no sbt at all
slice de --recording 777 --key 'Kino|Lalka' --ref "$docs" > /dev/null   # its tree's tool files this checkout's again
echo "docs again" >> "$repo/README.md"; scratch_git "$repo" commit -qam "no code again"
again=$(scratch_git "$repo" rev-parse HEAD)
rm -f "$work/calls"; slice de --recording 777 --key 'Kino|Lalka' --ref "$again" > /dev/null
check "a new ref is seeded from the nearest compiled ref" "1" "$(grep -c "seeding its compile from $cache/refs/$docs (0 source file(s) apart)" "$work/err")"
check "...and, its sources the seed's, compiles nothing" "0" "$(grep -c "^sbt $cache/refs/$again" "$work/calls")"
check "...its classpath the seed's, pointed at its own tree" "1" \
  "$(grep -c -- "-cp $cache/refs/$again/worker/target/scala-3.9.0/test-classes:/lib/a.jar" "$work/calls")"
echo "docs a third time" >> "$repo/README.md"; scratch_git "$repo" commit -qam "no code, a third"
third=$(scratch_git "$repo" rev-parse HEAD)
echo "// this checkout's own tool" >> "$repo/worker/src/test/scala/services/identity/IdentitySlicing.scala"
rm -f "$work/calls"; slice de --recording 777 --key 'Kino|Lalka' --ref "$third" > /dev/null
check "...but a tool file dropped in that differs from the seed's compiles it" "1" "$(grep -c "^sbt $cache/refs/$third" "$work/calls")"
scratch_git "$repo" checkout -q -- worker/src/test/scala/services/identity/IdentitySlicing.scala
# TWO AGENTS AT ONE REF: dropping the tool files into a tree is done by one at a time, like its layout and compile
headsha=$(scratch_git "$repo" rev-parse HEAD)
mkdir -p "$cache/locks/tools-$headsha"; sleep 30 & holder=$!; echo "$holder" > "$cache/locks/tools-$headsha/pid"
check "a call finding another dropping tool files into the ref's tree waits, and gives up naming it" "1 1" \
  "$(KINOWO_IDENTITY_LOCK_WAIT=2 slice de --recording 777 --key 'Kino|Lalka' --ref HEAD > /dev/null; echo $?) $(grep -c "gave up after .*tools-$headsha" "$work/err")"
kill "$holder"; rm -rf "$cache/locks/tools-$headsha"
rm -f "$work/calls"; slice de --recording 777 --key 'Kino|Lalka' --ref HEAD --fresh > /dev/null
check "--fresh decides every slice again" "0" "$(grep -c -- '--results' "$work/calls")"

# --serve: the batch goes to the ref's kept JVM — started once, then reused — which writes the takes the diff reads
rm -f "$work/calls"
out=$(slice de --recording 777 --key 'Kino|Lalka' --ref HEAD --serve)
slice de --recording 777 --batch "$work/b.tsv" --ref HEAD --serve --out "$work/served.tsv" > /dev/null
check "--serve starts one kept JVM per ref and recording, and sends it each batch" "1 2" \
  "$(grep -c -- "--serve $cache/serve/" "$work/calls") $(grep -c '^request ' "$work/calls")"
check "...each request carrying the batch's own arguments and where to write its takes" "1 1" \
  "$(grep -c "^request --labels .*--batch $work/b.tsv" "$work/calls") $(grep -c 'Kino|Lalka' "$work/served.tsv")"
check "...its output streamed back" "decided by the kept JVM" "$out"
STUB="$work" PATH="$work/bin:$PATH" KINOWO_IDENTITY_CACHE="$cache" "$repo/scripts/identity-slice" --stop-served 2> /dev/null
check "--stop-served stops every kept JVM" "0" "$(find "$cache/serve" -name '*.pid' | wc -l | tr -d ' ')"
rm -f "$work/calls"; slice de --recording 777 --key 'Kino|Lalka' --ref HEAD --serve > /dev/null
check "...and the next --serve starts another" "1" "$(grep -c -- "--serve $cache/serve/" "$work/calls")"
STUB="$work" PATH="$work/bin:$PATH" KINOWO_IDENTITY_CACHE="$cache" "$repo/scripts/identity-slice" --stop-served 2> /dev/null
# a pid file outlives its JVM, and the pid comes back as someone else's process: --stop-served leaves that one alone
mkdir -p "$cache/serve"; sleep 30 & stranger=$!; echo "$stranger" > "$cache/serve/stale.pid"
STUB="$work" PATH="$work/bin:$PATH" KINOWO_IDENTITY_CACHE="$cache" "$repo/scripts/identity-slice" --stop-served 2> /dev/null
check "--stop-served kills only a pid that is still an IdentitySlice JVM, and forgets the file" "alive gone" \
  "$(kill -0 "$stranger" 2>/dev/null && echo alive || echo dead) $([ -e "$cache/serve/stale.pid" ] && echo kept || echo gone)"
kill "$stranger" 2>/dev/null
# --emit-case goes through a kept JVM: pinning cases one after another reads the recording and the index once
rm -f "$work/calls"; slice de --recording 777 --key 'Kino|Lalka' --ref "$head" --emit-case > /dev/null
check "--emit-case starts a kept JVM and asks it for the case" "1 1" \
  "$(grep -c -- "--serve $cache/serve/" "$work/calls") $(grep -c "^request .*--emit-case $repo/test/resources/fixtures/identity-slices" "$work/calls")"
STUB="$work" PATH="$work/bin:$PATH" KINOWO_IDENTITY_CACHE="$cache" "$repo/scripts/identity-slice" --stop-served 2> /dev/null
# an edit of this checkout's code is a new ref key: the kept JVM of its older state is stopped, never left holding 6 GB
slice de --recording 777 --key 'Kino|Lalka' --serve > /dev/null
mkdir -p "$repo/identity/src/main"; echo "// edited" > "$repo/identity/src/main/Resolver.scala"
slice de --recording 777 --key 'Kino|Lalka' --serve > /dev/null
check "an edit's kept JVM stops the one of the checkout's older state" "1 1" \
  "$(grep -c 'stopped the kept JVM of an older state' "$work/err") $(find "$cache/serve" -name '*.pid' | wc -l | tr -d ' ')"
rm -rf "$repo/identity/src/main"
STUB="$work" PATH="$work/bin:$PATH" KINOWO_IDENTITY_CACHE="$cache" "$repo/scripts/identity-slice" --stop-served 2> /dev/null
started=$SECONDS
code=$(STUB_SERVE_HANG=1 KINOWO_IDENTITY_SLICE_START_SECONDS=2 slice de --recording 777 --key 'Kino|Lalka' --ref HEAD~1 --serve > /dev/null; echo $?)
check "a kept JVM that never listens is stopped at the deadline, and the call fails" "1 1 0" \
  "$code $(grep -c 'did not listen within 2s' "$work/err") $(find "$cache/serve" -name '*.pid' | wc -l | tr -d ' ')"
check "...within it" "yes" "$([ $(( SECONDS - started )) -lt 20 ] && echo yes || echo "no: $(( SECONDS - started ))s")"

# --faithfulness RUN: the leg's artifact and log are read through a bounded gh, and a gh that hangs fails the call
printf '#!/usr/bin/env bash\nsleep 20\n' > "$work/bin/gh"; chmod +x "$work/bin/gh"
started=$SECONDS
code=$(GH_DOWNLOAD_ATTEMPTS=1 KINOWO_IDENTITY_DOWNLOAD_TIMEOUT=1 slice de --faithfulness 123456789 > /dev/null; echo $?)
check "--faithfulness RUN with a gh that hangs fails, saying so" "1 1" "$code $(grep -c 'could not download run 123456789' "$work/err")"
check "...within the timeout" "yes" "$([ $(( SECONDS - started )) -lt 10 ] && echo yes || echo "no: $(( SECONDS - started ))s")"
# a throttled artifact read is asked again (gh_download), as every other download is: the first answers 502
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
[ "$1 $2" = "run download" ] || exit 1
echo download >> "$STUB/gh-downloads"
[ "$(wc -l < "$STUB/gh-downloads")" -gt 1 ] || { echo "HTTP 502: Bad Gateway" >&2; exit 1; }
while [ $# -gt 0 ]; do [ "$1" = --dir ] && dir=$2; shift; done
mkdir -p "$dir/de-takes" && echo takes > "$dir/de-takes/de.json.gz"
STUB
chmod +x "$work/bin/gh"; rm -f "$work/gh-downloads"
GH_DOWNLOAD_BACKOFF=0 slice de --faithfulness 223456789 > /dev/null
check "--faithfulness RUN: a throttled artifact read is asked again, and its takes kept in the cache" "2 yes" \
  "$(wc -l < "$work/gh-downloads" | tr -d ' ') $([ -s "$cache/diagnostics/223456789/de-takes/de.json.gz" ] && echo yes || echo no)"
# a download cut short by a Ctrl-C (TERM to the group) leaves no download directory beside the cache's copy
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
while [ $# -gt 0 ]; do [ "$1" = --dir ] && dir=$2; shift; done
mkdir -p "$dir/de-takes" && echo partial > "$dir/de-takes/de.json.gz"
touch "$STUB/gh-started"; sleep 20
STUB
chmod +x "$work/bin/gh"; rm -f "$work/gh-started"
STUB="$work" PATH="$work/bin:$PATH" KINOWO_IDENTITY_CACHE="$cache" perl -e 'setpgrp(0, 0); exec @ARGV' \
  "$repo/scripts/identity-slice" de --faithfulness 323456789 > /dev/null 2> "$work/err" &
cut_short=$!
for _ in $(seq 1 100); do [ -e "$work/gh-started" ] && break; sleep 0.1; done
kill -TERM -- "-$cut_short" 2>/dev/null; wait "$cut_short" 2>/dev/null
# the download's subshell cleans up once bounded has ended gh's group (up to 2s), after the tool itself has gone
for _ in $(seq 1 50); do
  [ -z "$(find "$cache/diagnostics" -maxdepth 1 -name '323456789.dl.*'; find "$cache/locks" -maxdepth 1 -name 'diagnostics-323456789-*')" ] && break
  sleep 0.1
done
check "--faithfulness RUN: a download cut short leaves no download directory, nor a lock" "yes 0 0" \
  "$([ -e "$work/gh-started" ] && echo yes || echo no) $(find "$cache/diagnostics" -maxdepth 1 -name '323456789.dl.*' | wc -l | tr -d ' ') $(find "$cache/locks" -maxdepth 1 -name 'diagnostics-323456789-*' | wc -l | tr -d ' ')"
rm -f "$work/bin/gh"

# --prod-mirror: prod's state from the LOCAL mirror, its freshness checked against the recording first
rm -f "$work/calls"
slice de --recording 777 --key 'Kino|Lalka' --prod-mirror > /dev/null
check "--prod-mirror checks the mirror's freshness against the recording's moment" "1" \
  "$(grep -c -- '^freshness de --quiet .*--not-before 2026-10-14T05:00:00Z' "$work/calls")"
check "...then hands the slice the LOCAL mirror, never prod" "1" "$(grep -c -- '--prod-mirror mongodb://127.0.0.1:28017/' "$work/calls")"
check "...and keeps no result of prod's moment for another call" "0" "$(grep -c -- '--results' "$work/calls")"
rm -f "$work/calls"
KINOWO_DEPLOYED_WORKER=0123abcd slice de --recording 777 --key 'Kino|Lalka' --prod-mirror > /dev/null
check "...and names the deployed worker build whose code wrote the mirror's state" "1 1" \
  "$(grep -c -- '--deployed 0123abcd' "$work/calls") $(grep -c 'written by the deployed worker 0123abcd' "$work/err")"
check "...the mirror reader dropped into a ref's tree" "1" "$(slice de --recording 777 --key 'Kino|Lalka' --ref HEAD --prod-mirror > /dev/null
  grep -c ProdMirror "$cache/refs/$head/worker/src/fixtures/scala/tools/ProdMirror.scala")"
rm -f "$work/calls"
FRESHNESS_EXIT=3 slice de --recording 777 --key 'Kino|Lalka' --prod-mirror de > /dev/null
check "a stale mirror is WARNed loudly, and read" "1 1" "$(grep -c 'THE PROD MIRROR IS NOT FRESH' "$work/err") $(grep -c -- '--prod-mirror' "$work/calls")"
rm -f "$work/calls"
check "no mirror at all refuses, never falling back to prod" "2 0" \
  "$(FRESHNESS_EXIT=2 slice de --recording 777 --key 'Kino|Lalka' --prod-mirror > /dev/null; echo $?) $(grep -c '^java' "$work/calls" 2> /dev/null)"
check "another country's mirror is refused" "2" "$(slice de --recording 777 --key 'Kino|Lalka' --prod-mirror uk > /dev/null; echo $?)"
check "...and a slice without it reads no mirror" "0" "$(rm -f "$work/calls"; slice de --recording 777 --key 'Kino|Lalka' > /dev/null; grep -c -- 'freshness\|--prod-mirror' "$work/calls")"

# --explain: the WHY trace of a listing the unmatched-cluster capture holds (scripts/identity-explain is its alias)
rm -f "$work/calls"
mkdir -p "$work/capture"; echo c > "$work/capture/de.json.gz"; printf 'country\n' > "$work/capture/identity-judgements.tsv"
slice de --explain --fixture "$work/capture" --venue Kino --title Lalka --ref "$head" > /dev/null
check "--explain replays the capture it is handed, judged by its judgements" "1" \
  "$(grep -c -- "scripts.IdentityExplain de --fixture $work/capture/de.json.gz --labels $work/capture/identity-judgements.tsv --venue Kino --title Lalka" "$work/calls")"
check "...under the ref's code, the explainer dropped into its tree" "1 // worker/src/test/scala/scripts/IdentityExplain.scala" \
  "$(grep -c "java $cache/refs/$head " "$work/calls") $(cat "$cache/refs/$head/worker/src/test/scala/scripts/IdentityExplain.scala")"
check "...and no recording is fetched for it" "0" "$(grep -c -- '--recording' "$work/calls")"
rm -f "$work/calls"
STUB="$work" PATH="$work/bin:$PATH" KINOWO_IDENTITY_CACHE="$cache" "$repo/scripts/identity-explain" de --fixture "$work/capture" --venue Kino > /dev/null 2>&1
check "scripts/identity-explain is identity-slice --explain" "1" \
  "$(grep -c -- "scripts.IdentityExplain de --fixture $work/capture/de.json.gz --labels $work/capture/identity-judgements.tsv --venue Kino" "$work/calls")"

spec_summary
