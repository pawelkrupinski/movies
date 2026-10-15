#!/usr/bin/env bash
# What the local identity tools (identity-slice and its --explain, identity-judge-cards, venue-replay, title-trace,
# recording-diff, hard-clusters.sh, listing-history) share: the shared cache, a recording or a capture downloaded once
# into it, a ref's sources laid out once, a tree's worker test classpath compiled once. Sourcing it defines functions only.
#
#   identity_newest_recording [cc]  -> the newest successful "Record scrape fixtures" run (with cc: holding its corpus)
#   identity_recording <run> <cc> [corpus] -> the fixtures directory holding that run's corpus/ and enrichment-<cc>/
#        (with `corpus`: the scrape corpus alone)
#   identity_capture <src> <cc>...  -> prints the directory holding <cc>.json.gz for each cc, and (a run's or a ref's)
#        the judgements that judge it, identity-judgements.tsv (a ref from before they were one file: its labels.tsv)
#        src: "" (this checkout's), a directory, an "Identity capture" run id, or a git ref
#   identity_judgements <dir>       -> the judgements that judge the capture in <dir>: its own, else this checkout's
#   identity_tree <ref> <file>...   -> the ref's sources laid out once under the cache, the named files this checkout's
#   identity_classpath <tree>       -> prints the tree's worker test classpath, sbt run only when a source is newer
#
# Each fails (non-zero, said on stderr) when what it needs cannot be read — gh unreachable, a download cut short — and
# never hands back a stale or partial copy instead. Every gh call is bounded by `timeout`; a wait for another agent's
# download, layout or compile of the same thing (identity_locked) has a deadline, and a lock whose holder is gone is
# taken over.

IDENTITY_REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
IDENTITY_CACHE=${KINOWO_IDENTITY_CACHE:-$HOME/.cache/kinowo-identity}
IDENTITY_FIXTURES=test/resources/fixtures/identity-unmatched
IDENTITY_JUDGEMENTS=test/resources/fixtures/identity-judgements.tsv
identity_log() { echo "[${IDENTITY_TOOL:-identity}] $*" >&2; }

# identity_gh <seconds> <gh args...> -> gh, killed when it outlasts <seconds> (a hung gh fails, it never waits forever)
identity_gh() { local seconds=$1; shift; timeout "$seconds" gh "$@"; }
IDENTITY_GH_READ=${KINOWO_IDENTITY_GH_TIMEOUT:-60}
IDENTITY_GH_DOWNLOAD=${KINOWO_IDENTITY_DOWNLOAD_TIMEOUT:-1800}

# identity_locked <name> <command...> -> the command, run while no other agent runs one under the same name (a download
# or layout into the shared cache). A lock whose holder is gone — killed, the machine restarted — is taken over; a live
# holder is waited for at most $KINOWO_IDENTITY_LOCK_WAIT seconds (default 3600), then this fails naming the lock.
identity_locked() {
  local name=$1 lock=$IDENTITY_CACHE/locks/$1 waited=0 holder deadline=${KINOWO_IDENTITY_LOCK_WAIT:-3600}; shift
  mkdir -p "$IDENTITY_CACHE/locks"
  until mkdir "$lock" 2>/dev/null; do
    holder=$(cat "$lock/pid" 2>/dev/null || true)
    # no pid yet: its holder is between taking the lock and writing it — or was killed there, a minute ago or more
    if { [ -n "$holder" ] && ! kill -0 "$holder" 2>/dev/null; } ||
       { [ -z "$holder" ] && [ -n "$(find "$lock" -maxdepth 0 -mmin +1 2>/dev/null)" ]; }; then
      identity_log "taking over the lock on $name: its holder ${holder:-(none)} is gone"
      rm -rf "$lock"; continue
    fi
    if [ "$waited" -ge "$deadline" ]; then
      identity_log "gave up after ${waited}s waiting for $name (held by pid ${holder:-?}; $lock)"
      return 1
    fi
    [ $(( waited % 60 )) -ne 0 ] || identity_log "waiting for another agent's $name (pid ${holder:-?})"
    sleep 2; waited=$(( waited + 2 ))
  done
  ( echo "$BASHPID" > "$lock/pid"; trap 'rm -rf "$lock"' EXIT; "$@" )
}

identity_capture() {
  local src=$1 dir sha cc; shift
  if [ -z "$src" ]; then echo "$IDENTITY_REPO/$IDENTITY_FIXTURES"; return; fi
  if [ -d "$src" ]; then (cd "$src" && pwd); return; fi
  if [[ $src =~ ^[0-9]{6,}$ ]]; then
    dir=$IDENTITY_CACHE/captures/run-$src
    for cc in "$@"; do
      [ -f "$dir/$cc.json.gz" ] || identity_locked "capture-$src-$cc" identity_capture_download "$src" "$dir" "$cc" || return 1
    done
    # a run's capture is judged by this checkout's judgements
    identity_put "$dir/identity-judgements.tsv" cat "$IDENTITY_REPO/$IDENTITY_JUDGEMENTS" || return 1
    echo "$dir"; return
  fi
  sha=$(git -C "$IDENTITY_REPO" rev-parse --verify "$src^{commit}") || { echo "no directory, run id or ref $src" >&2; return 2; }
  dir=$IDENTITY_CACHE/captures/ref-$sha
  for cc in "$@"; do
    [ -f "$dir/$cc.json.gz" ] || identity_put "$dir/$cc.json.gz" git -C "$IDENTITY_REPO" show "$sha:$IDENTITY_FIXTURES/$cc.json.gz" || return 1
  done
  # a ref's capture is judged by that ref's judgements -- or, from before they were one file, its labels.tsv, which
  # tools.IdentityJudgements reads too
  [ -s "$dir/identity-judgements.tsv" ] || identity_put "$dir/identity-judgements.tsv" git -C "$IDENTITY_REPO" show "$sha:$IDENTITY_JUDGEMENTS" 2>/dev/null \
    || identity_put "$dir/identity-judgements.tsv" git -C "$IDENTITY_REPO" show "$sha:$IDENTITY_FIXTURES/labels.tsv" || return 1
  echo "$dir"
}

# identity_put <file> <command...> -> the command's output written to <file> whole or not at all: a reader never sees
# it half written, and two writers never interleave
identity_put() {
  local file=$1 part; shift
  mkdir -p "$(dirname "$file")"
  part=$(mktemp "$file.part.XXXXXX") || return 1
  if "$@" > "$part"; then mv -f "$part" "$file"; else rm -f "$part"; return 1; fi
}

# identity_capture_download <run> <dir> <cc> -> the run's identity-capture-<cc> artifact's <cc>.json.gz, as <dir>/<cc>.json.gz
identity_capture_download() {
  local run=$1 dir=$2 cc=$3 dl file
  [ -f "$dir/$cc.json.gz" ] && return 0
  identity_log "downloading identity-capture-$cc of run $run"
  mkdir -p "$dir"
  dl=$(mktemp -d "$dir/dl-$cc.XXXXXX")
  if ! identity_gh "$IDENTITY_GH_DOWNLOAD" run download "$run" -R "${KINOWO_REPO:-pawelkrupinski/movies}" --name "identity-capture-$cc" --dir "$dl" >&2; then
    rm -rf "$dl"; identity_log "could not download identity-capture-$cc of run $run (gh failed or timed out)"; return 1
  fi
  file=$(find "$dl" -name "$cc.json.gz" | head -1)
  [ -n "$file" ] || { rm -rf "$dl"; identity_log "run $run's identity-capture-$cc holds no $cc.json.gz"; return 1; }
  mv -f "$file" "$dir/$cc.json.gz"; rm -rf "$dl"
}

identity_judgements() {
  if [ -f "$1/identity-judgements.tsv" ]; then echo "$1/identity-judgements.tsv"; else echo "$IDENTITY_REPO/$IDENTITY_JUDGEMENTS"; fi
}

# identity_tree <ref> <file>... -> a compiled-once copy of the ref's sources under the cache, each named file (a path
# relative to the repository) dropped in from this checkout — the tool's own code, so a ref from before the tool works
identity_tree() {
  local sha dir t ref=$1; shift
  sha=$(git -C "$IDENTITY_REPO" rev-parse --verify "$ref^{commit}") || { echo "no ref $ref" >&2; return 2; }
  dir=$IDENTITY_CACHE/refs/$sha
  [ -f "$dir/build.sbt" ] || identity_locked "tree-$sha" identity_tree_layout "$ref" "$sha" "$dir" || return 1
  # copied only when it changed: an unchanged copy keeps its time, and the ref is not compiled again
  for t in "$@"; do
    mkdir -p "$(dirname "$dir/$t")"
    cmp -s "$IDENTITY_REPO/$t" "$dir/$t" 2>/dev/null || cp "$IDENTITY_REPO/$t" "$dir/$t"
  done
  echo "$dir"
}

# identity_tree_layout <ref> <sha> <dir> -> the ref's sources in <dir>, laid out beside it and moved in whole
identity_tree_layout() {
  local ref=$1 sha=$2 dir=$3 t part
  [ -f "$dir/build.sbt" ] && return 0
  identity_log "laying out $ref ($sha) in $dir"
  part=$dir.part
  rm -rf "$part" "$dir"; mkdir -p "$part"
  git -C "$IDENTITY_REPO" archive "$sha" -- . ':(exclude)test/resources' ':(exclude)ios' ':(exclude)android' | tar -x -C "$part" || return 1
  # Seed the compile from this checkout's (APFS clones, no copy): sbt recompiles only what the ref changed.
  for t in project/target common/target identity/target testkit/target worker/target target; do
    if [ -d "$IDENTITY_REPO/$t" ]; then mkdir -p "$part/$(dirname "$t")"; cp -cR "$IDENTITY_REPO/$t" "$part/$t" 2>/dev/null || cp -R "$IDENTITY_REPO/$t" "$part/$t"; fi
  done
  rm -rf "$part/target/identity-tools"
  mv "$part" "$dir"
}

# identity_recording <run> <cc> [corpus] -> the directory holding that "Record scrape fixtures" run's corpus/ and
# enrichment-<cc>/ (with `corpus`: its scrape corpus alone), downloaded once into the shared cache
# (scripts/identity-fetch-recording.sh), one download of a run's country at a time across agents
identity_recording() {
  local run=$1 cc=$2 what=${3:-} fixtures=$IDENTITY_CACHE/recordings/$1/pair/test/resources/fixtures
  identity_recorded "$fixtures" "$cc" "$what" || identity_locked "recording-$run-$cc" identity_recording_download "$run" "$cc" "$what" || return 1
  echo "$fixtures"
}

# identity_recorded <fixtures> <cc> [corpus] -> whether the recording is laid out: the corpus is laid out last
identity_recorded() {
  [ -f "$1/corpus/cinema-scrapes-$2.json.gz" ] && { [ "${3:-}" = corpus ] || [ -d "$1/enrichment-$2" ]; }
}

identity_recording_download() {
  local run=$1 cc=$2 what=$3 dir=$IDENTITY_CACHE/recordings/$1
  identity_recorded "$dir/pair/test/resources/fixtures" "$cc" "$what" && return 0
  identity_log "downloading recording $run ($cc${what:+, its $what alone}) into $dir"
  "$IDENTITY_REPO/scripts/identity-fetch-recording.sh" ${what:+--corpus-only} "$run" "$dir" "$cc" >&2 && return 0
  identity_log "could not download recording $run ($cc)"
  return 1
}

# identity_newest_recording [cc] -> the id of the newest successful "Record scrape fixtures" run; with a country, the
# newest that actually holds that country's corpus (already cached, or a live scrape-fixtures-<cc> artifact), each newer
# run skipped said on stderr. 1 when none of the newest 20 holds it — or when a run's artifacts could not be read: an
# unreadable run is never skipped for an older one.
identity_newest_recording() {
  local cc=${1:-} repo=${KINOWO_REPO:-pawelkrupinski/movies} id ids held
  ids=$(identity_gh "$IDENTITY_GH_READ" run list -R "$repo" --workflow "Record scrape fixtures" --status success \
    -L "$([ -n "$cc" ] && echo 20 || echo 1)" --json databaseId -q '.[].databaseId') ||
    { identity_log "could not list the \"Record scrape fixtures\" runs (gh failed or timed out): name one with --recording"; return 1; }
  [ -n "$ids" ] || { identity_log "no successful \"Record scrape fixtures\" run"; return 1; }
  [ -n "$cc" ] || { echo "${ids%%$'\n'*}"; return; }
  for id in $ids; do
    if [ -f "$IDENTITY_CACHE/recordings/$id/pair/test/resources/fixtures/corpus/cinema-scrapes-$cc.json.gz" ]; then held=yes
    else
      held=$(identity_gh "$IDENTITY_GH_READ" api "repos/$repo/actions/runs/$id/artifacts?per_page=100" --paginate \
        -q ".artifacts[] | select(.name == \"scrape-fixtures-$cc\" and (.expired | not)) | .name") ||
        { identity_log "could not read recording $id's artifacts (gh failed or timed out): name one with --recording"; return 1; }
    fi
    if [ -n "$held" ]; then
      identity_log "recording $id: the newest holding a $cc corpus"
      echo "$id"; return
    fi
    identity_log "recording $id skipped: it holds no $cc corpus (no live scrape-fixtures-$cc artifact)"
  done
  identity_log "none of the newest 20 successful recordings holds a $cc corpus: name one with --recording"
  return 1
}

identity_classpath() {
  local tree=$1
  identity_classpath_fresh "$tree" || identity_locked "classpath-$(printf '%s' "$tree" | shasum | cut -c1-12)" identity_classpath_compile "$tree" || return 1
  cat "$tree/target/identity-tools/classpath"
}

# identity_classpath_fresh <tree> -> whether the kept classpath is newer than every source
identity_classpath_fresh() {
  local cp=$1/target/identity-tools/classpath
  [ -s "$cp" ] && [ -z "$(find "$1/common/src" "$1/identity/src" "$1/worker/src" "$1/testkit/src" "$1/build.sbt" -newer "$cp" -name '*.s*' -print -quit 2>/dev/null)" ]
}

# identity_classpath_line <sbt log> -> the last line of it that IS a classpath (sbt prints more than `export` asked)
identity_classpath_line() { grep -E '^/[^ ]*worker/target/[^ ]*test-classes' "$1" | tail -n 1; }

# identity_classpath_compile <tree> -> sbt over the tree, its classpath kept — one sbt a tree at a time
identity_classpath_compile() {
  local tree=$1 cp=$1/target/identity-tools/classpath log=$1/target/identity-tools/sbt.log
  identity_classpath_fresh "$tree" && return 0
  mkdir -p "$(dirname "$cp")"
  identity_log "compiling $tree (sbt; then cached until a source changes)"
  if ! (cd "$tree" && sbt -batch "worker/Test/compile" "export worker/Test/fullClasspath") > "$log" 2>&1; then
    tail -40 "$log" >&2; identity_log "compile failed in $tree (log: $log)"; return 1
  fi
  identity_put "$cp" identity_classpath_line "$log" || true
  [ -s "$cp" ] || { rm -f "$cp"; tail -40 "$log" >&2; identity_log "no classpath in sbt's output ($log)"; return 1; }
}

# The JVM every identity tool runs in: English number formats, no Unsafe warnings.
# shellcheck disable=SC2034 # read by the scripts sourcing this
IDENTITY_JAVA=(java -Xmx6g -Duser.language=en -Duser.country=US --sun-misc-unsafe-memory-access=allow)
