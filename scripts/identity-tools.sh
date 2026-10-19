#!/usr/bin/env bash
# What the local identity tools (identity-slice and its --explain, identity-judge-cards, venue-replay, title-trace,
# recording-diff, hard-clusters.sh, listing-history) share: the shared cache, a recording or a capture downloaded once
# into it, a ref's sources laid out once, a tree's worker test classpath compiled once. Sourcing it defines functions only.
#
#   identity_newest_recording [cc]  -> the newest successful "Record scrape fixtures" run (with cc: holding its corpus)
#   identity_recording <run> <cc> [corpus] -> the fixtures directory holding that run's corpus/ and enrichment-<cc>/
#        (with `corpus`: the scrape corpus alone)
#   identity_recording_cached <run> <cc> -> that directory when the cache already holds it, else fails: never downloads
#   identity_capture <src> <cc>...  -> prints the directory holding <cc>.json.gz for each cc, and (a run's or a ref's)
#        the judgements that judge it, identity-judgements.tsv (a ref from before they were one file: its labels.tsv)
#        src: "" (this checkout's), a directory, an "Identity capture" run id, or a git ref
#   identity_judgements <dir>       -> the judgements that judge the capture in <dir>: its own, else this checkout's
#   identity_tree <ref> <file>...   -> the ref's sources laid out once under the cache, the named files this checkout's
#   identity_classpath <tree>       -> prints the tree's worker test classpath, sbt run only when a source is newer
#
# Each fails (non-zero, said on stderr) when what it needs cannot be read — gh unreachable, a download cut short — and
# never hands back a stale or partial copy instead. Every gh call is bounded (tool-conventions.sh's `bounded`); a wait for another agent's
# download, layout or compile of the same thing (identity_locked) has a deadline, and a lock is free the moment its
# holder has gone.

IDENTITY_REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
# shellcheck source=scripts/tool-conventions.sh
. "$(dirname "${BASH_SOURCE[0]}")/tool-conventions.sh"
# shellcheck source=scripts/jvm-gate.sh
. "$(dirname "${BASH_SOURCE[0]}")/jvm-gate.sh"
IDENTITY_CACHE=$KINOWO_IDENTITY_CACHE
IDENTITY_FIXTURES=test/resources/fixtures/identity-unmatched
IDENTITY_JUDGEMENTS=test/resources/fixtures/identity-judgements.tsv
identity_log() { echo "[${IDENTITY_TOOL:-identity}] $*" >&2; }

# identity_gh <seconds> <gh args...> -> gh, killed when it outlasts <seconds> (a hung gh fails, it never waits forever)
identity_gh() { local seconds=$1; shift; bounded "$seconds" gh "$@"; }
IDENTITY_GH_READ=${KINOWO_IDENTITY_GH_TIMEOUT:-60}
IDENTITY_GH_DOWNLOAD=${KINOWO_IDENTITY_DOWNLOAD_TIMEOUT:-1800}

# identity_locked <name> <command...> -> the command, run while no other agent runs one under the same name (a download
# or layout into the shared cache): jvm-gate.sh's kernel lock, on descriptor 8, held by a subshell, so it is free the
# moment that subshell and what it started have ended, killed or not. A live holder is waited for at most
# $KINOWO_IDENTITY_LOCK_WAIT seconds (default 3600), then this fails naming the lock.
identity_locked() {
  local name=$1 lock=$IDENTITY_CACHE/locks/$1.flock deadline=${KINOWO_IDENTITY_LOCK_WAIT:-3600}; shift
  mkdir -p "$IDENTITY_CACHE/locks"
  ( waited=0
    until lock_take 8 "$lock"; do
      holder=$(cat "$lock" 2>/dev/null || true)
      if [ "$waited" -ge "$deadline" ]; then
        identity_log "gave up after ${waited}s waiting for $name (held by pid ${holder:-?}; $lock)"
        exit 1
      fi
      [ $(( waited % 60 )) -ne 0 ] || identity_log "waiting for another agent's $name (pid ${holder:-?})"
      sleep 2; waited=$(( waited + 2 ))
    done
    # the holder named is this subshell: sh's parent (bash 3.2, macOS's /bin/bash, has no $BASHPID, and $$ is the caller's)
    # shellcheck disable=SC2016 # sh expands it
    sh -c 'echo "$PPID"' > "$lock"
    "$@"; rc=$?
    lock_give 8 "$lock"
    exit "$rc" )
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
  if ! gh_download "$IDENTITY_GH_DOWNLOAD" run download "$run" -R "$KINOWO_GH_REPO" --name "identity-capture-$cc" --dir "$dl" >&2; then
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
# relative to the repository) dropped in from this checkout — the tool's own code, so a ref from before the tool works.
# The tree is shared by every checkout, so two checkouts whose tool files differ must not drop theirs into one tree (the
# compile would be a mix, and each would run the other's code): the first set of tool files to reach refs/<sha> claims it
# (target/identity-tools/tools-hash); a checkout with other tool files gets refs/<sha>-t<hash of its tool files>.
identity_tree() {
  local sha dir ref=$1; shift
  sha=$(git -C "$IDENTITY_REPO" rev-parse --verify "$ref^{commit}") || { echo "no ref $ref" >&2; return 2; }
  dir=$IDENTITY_CACHE/refs/$sha
  [ -f "$dir/build.sbt" ] || identity_locked "tree-$sha" identity_tree_layout "$ref" "$sha" "$dir" || return 1
  # one agent a tree drops the files in: a second, comparing while the first copies, would copy over it again and move
  # the time the first's compile reads — or read (and remove) the seed marker the first still needs
  identity_locked "tools-$sha" identity_tree_claimed "$ref" "$sha" "$dir" "$@"
}

# The hash of this checkout's <file>... (names and contents).
identity_tools_hash() {
  local t
  for t in "$@"; do printf '%s\n' "$t"; cat "$IDENTITY_REPO/$t" 2>/dev/null; done | shasum | cut -c1-12
}

# identity_tree_claimed <ref> <sha> <dir> <file>... -> prints the tree holding exactly this checkout's tool files:
# <dir> when unclaimed or claimed by the same tool files, else the variant tree for their hash
identity_tree_claimed() {
  local ref=$1 sha=$2 dir=$3 mark hash held; shift 3
  hash=$(identity_tools_hash "$@")
  mark=target/identity-tools/tools-hash
  held=$(cat "$dir/$mark" 2>/dev/null || true)
  if [ -n "$held" ] && [ "$held" != "$hash" ]; then
    dir=$dir-t$hash
    [ -f "$dir/build.sbt" ] || identity_locked "tree-$sha-t$hash" identity_tree_layout "$ref" "$sha" "$dir" || return 1
  fi
  identity_tree_tools "$dir" "$@" || return 1
  mkdir -p "$dir/target/identity-tools"; echo "$hash" > "$dir/$mark"
  echo "$dir"
}

# identity_tree_tools <dir> <file>... -> each named file dropped into the laid-out tree from this checkout, whole or not at all
identity_tree_tools() {
  local dir=$1 t seed=$1/target/identity-tools/seed; shift
  # copied only when it changed: an unchanged copy keeps its time, and the ref is not compiled again
  for t in "$@"; do
    mkdir -p "$(dirname "$dir/$t")"
    cmp -s "$IDENTITY_REPO/$t" "$dir/$t" 2>/dev/null || { cp "$IDENTITY_REPO/$t" "$dir/$t.part" && mv -f "$dir/$t.part" "$dir/$t"; }
  done
  # a classpath kept from a seed (identity_tree_layout) holds the classes of the SEED's tool files: one differing from
  # those now dropped in is made newer than it, and compiles
  if [ -f "$seed" ]; then
    for t in "$@"; do [ ! -f "$dir/$t" ] || cmp -s "$(cat "$seed")/$t" "$dir/$t" 2>/dev/null || touch "$dir/$t"; done
    rm -f "$seed"
  fi
}

# identity_tree_layout <ref> <sha> <dir> -> the ref's sources in <dir>, laid out beside it and moved in whole
identity_tree_layout() {
  local ref=$1 sha=$2 dir=$3 t part
  [ -f "$dir/build.sbt" ] && return 0
  identity_log "laying out $ref ($sha) in $dir"
  part=$dir.part
  rm -rf "$part" "$dir"; mkdir -p "$part"
  git -C "$IDENTITY_REPO" archive "$sha" -- . ':(exclude)test/resources' ':(exclude)ios' ':(exclude)android' | tar -x -C "$part" || return 1
  # Seed the compile from the nearest compiled code (APFS clones, no copy): sbt recompiles only what the ref changed.
  local seed apart=""
  seed=$(identity_nearest_compiled "$sha")
  [ -z "$seed" ] || { apart=$(identity_seed_distance "$seed" "$sha"); identity_log "seeding its compile from ${seed} ($apart source file(s) apart)"; }
  for t in project/target common/target identity/target testkit/target worker/target target; do
    if [ -n "$seed" ] && [ -d "$seed/$t" ]; then mkdir -p "$part/$(dirname "$t")"; cp -cR "$seed/$t" "$part/$t" 2>/dev/null || cp -R "$seed/$t" "$part/$t"; fi
  done
  rm -rf "$part/target/identity-tools"
  # The very sources of a compiled ref (a commit changing no source: a judgement, a doc): its classes ARE this ref's, so
  # its classpath is kept, pointed here -- no sbt start to find nothing to compile (~15 s). The tool files dropped in
  # afterwards (identity_tree) are newer than it when they differ, and recompile as ever.
  if [ "$apart" = 0 ] && [ "$seed" != "$IDENTITY_REPO" ] && [ -s "$seed/target/identity-tools/classpath" ]; then
    mkdir -p "$part/target/identity-tools"
    sed "s#$seed/#$dir/#g" "$seed/target/identity-tools/classpath" > "$part/target/identity-tools/classpath"
    echo "$seed" > "$part/target/identity-tools/seed"
  fi
  mv "$part" "$dir"
}

# The source paths a compile reads: how far two commits' compiles stand apart is how many of these files differ.
IDENTITY_COMPILED_PATHS=(common/src identity/src worker/src testkit/src build.sbt project)
# identity_seed_distance <tree> <sha> -> the source files the compiled <tree> differs from <sha> by (this checkout: its HEAD's)
identity_seed_distance() {
  local from
  if [ "$1" = "$IDENTITY_REPO" ]; then from=$(git -C "$IDENTITY_REPO" rev-parse HEAD); else from=$(basename "$1"); fi
  # a commit this clone lacks (another clone's tree) is no distance -- never a failed diff read as none
  git -C "$IDENTITY_REPO" cat-file -e "$from^{commit}" 2>/dev/null || return 0
  local files
  files=$(git -C "$IDENTITY_REPO" diff --name-only "$from" "$2" -- "${IDENTITY_COMPILED_PATHS[@]}") || return 0
  if [ -z "$files" ]; then echo 0; else printf '%s\n' "$files" | wc -l | tr -d ' '; fi
}
# identity_nearest_compiled <sha> -> the compiled tree nearest <sha> by changed source files: this checkout, or one of the
# cache's most recently compiled refs (a fresh worktree has nothing compiled: seeded from it, a new ref compiled all
# 1,766 sources, 108 s idle and ~10 min with five agents compiling beside it -- 2026-10-15). Empty when none is compiled.
identity_nearest_compiled() {
  local sha=$1 best="" best_n="" t n lock candidates=()
  [ -d "$IDENTITY_REPO/worker/target" ] && candidates+=("$IDENTITY_REPO")
  while IFS= read -r t; do candidates+=("$(dirname "$(dirname "$(dirname "$t")")")"); done < <(
    ls -t "$IDENTITY_CACHE"/refs/*/target/identity-tools/classpath 2>/dev/null | head -n "${KINOWO_IDENTITY_SEED_CANDIDATES:-40}")
  for t in ${candidates[@]+"${candidates[@]}"}; do
    [ "$(basename "$t")" != "$sha" ] || continue
    # a tree compiling now (identity_classpath_compile holds its lock) may hold half-written classes
    # (a lock given back holds no pid: only one with a pid in is asked)
    lock=$IDENTITY_CACHE/locks/$(identity_classpath_lock "$t").flock
    { [ -s "$lock" ] && lock_busy "$lock"; } && continue
    n=$(identity_seed_distance "$t" "$sha")
    [ -n "$n" ] || continue
    if [ -z "$best_n" ] || [ "$n" -lt "$best_n" ]; then best=$t best_n=$n; fi
  done
  echo "$best"
}

# identity_recording <run> <cc> [corpus] -> the directory holding that "Record scrape fixtures" run's corpus/ and
# enrichment-<cc>/ (with `corpus`: its scrape corpus alone), downloaded once into the shared cache
# (scripts/identity-fetch-recording.sh), one download of a run's country at a time across agents
identity_recording() {
  local run=$1 cc=$2 what=${3:-} fixtures
  fixtures=$(identity_recording_dir "$run")
  identity_recorded "$fixtures" "$cc" "$what" || identity_locked "recording-$run-$cc" identity_recording_download "$run" "$cc" "$what" || return 1
  echo "$fixtures"
}

identity_recording_dir() { echo "$IDENTITY_CACHE/recordings/$1/pair/test/resources/fixtures"; }

# identity_recording_cached <run> <cc> -> the directory holding the run's corpus/ and enrichment-<cc>/ when the cache
# holds both, else fails saying so: for a caller that must not reach GitHub (identity-capture.sh --offline)
identity_recording_cached() {
  local fixtures
  fixtures=$(identity_recording_dir "$1")
  identity_recorded "$fixtures" "$2" || { identity_log "recording $1 holds no $2 corpus and tree in $fixtures (fetch it: scripts/identity-fetch-recording.sh)"; return 1; }
  echo "$fixtures"
}

# identity_recorded <fixtures> <cc> [corpus] -> whether the recording is laid out: the corpus is laid out last
identity_recorded() {
  [ -f "$1/corpus/cinema-scrapes-$2.json.gz" ] && { [ "${3:-}" = corpus ] || [ -d "$1/enrichment-$2" ]; }
}

identity_recording_download() {
  local run=$1 cc=$2 what=$3 dir=$IDENTITY_CACHE/recordings/$1
  identity_recorded "$(identity_recording_dir "$run")" "$cc" "$what" && return 0
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
  local cc=${1:-} repo=$KINOWO_GH_REPO id ids held
  # newest by CREATION time, not listing order: GitHub's status-filtered listing is not in creation order
  # (`--limit 1` named 2026-09-25's run on 2026-10-07, the newest being 2026-10-06's -- 2026-10-17)
  ids=$(identity_gh "$IDENTITY_GH_READ" run list -R "$repo" --workflow "Record scrape fixtures" --status success \
    -L 100 --json databaseId,createdAt -q '.[] | "\(.databaseId)\t\(.createdAt)"') &&
    ids=$(printf '%s\n' "$ids" | sort -t "$(printf '\t')" -k2 -r | cut -f1 | head -n "$([ -n "$cc" ] && echo 20 || echo 1)") ||
    { identity_log "could not list the \"Record scrape fixtures\" runs (gh failed or timed out): name one with --recording"; return 1; }
  [ -n "$ids" ] || { identity_log "no successful \"Record scrape fixtures\" run"; return 1; }
  [ -n "$cc" ] || { echo "${ids%%$'\n'*}"; return; }
  for id in $ids; do
    if identity_recorded "$(identity_recording_dir "$id")" "$cc" corpus; then held=yes
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
  identity_classpath_fresh "$tree" || identity_locked "$(identity_classpath_lock "$tree")" identity_classpath_compile "$tree" || return 1
  cat "$tree/target/identity-tools/classpath"
}

# identity_classpath_lock <tree> -> the name of the lock its compile holds (identity_locked)
identity_classpath_lock() { echo "classpath-$(printf '%s' "$1" | shasum | cut -c1-12)"; }

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
  jvm_slot_take "${IDENTITY_TOOL:-identity}"
  # shellcheck disable=SC2046 # jvm_cpu_options: none, or one option
  if ! (cd "$tree" && sbt $(jvm_cpu_options | sed 's/^/-J/') -batch "worker/Test/compile" "export worker/Test/fullClasspath") > "$log" 2>&1; then
    jvm_slot_release
    tail -40 "$log" >&2; identity_log "compile failed in $tree (log: $log)"; return 1
  fi
  jvm_slot_release
  identity_put "$cp" identity_classpath_line "$log" || true
  [ -s "$cp" ] || { rm -f "$cp"; tail -40 "$log" >&2; identity_log "no classpath in sbt's output ($log)"; return 1; }
}

# The JVM every identity tool runs in: English number formats ("0.97", never "0,97"), no Unsafe warnings. IDENTITY_SBT
# is sbt in the same locale (a heap and the commands appended: "${IDENTITY_SBT[@]}" -J-Xmx6g -batch ...).
IDENTITY_LOCALE=(-Duser.language=en -Duser.country=US)
# shellcheck disable=SC2034 # read by the scripts sourcing this
IDENTITY_JAVA=(java -Xmx6g "${IDENTITY_LOCALE[@]}" --sun-misc-unsafe-memory-access=allow)
# sized for a share of the cores on a Mac agents share (scripts/jvm-gate.sh): none, or one option
identity_cpus=$(jvm_cpu_options); [ -z "$identity_cpus" ] || IDENTITY_JAVA+=("$identity_cpus")
# shellcheck disable=SC2034 # read by the scripts sourcing this
IDENTITY_SBT=(sbt "${IDENTITY_LOCALE[@]/#/-J}")
