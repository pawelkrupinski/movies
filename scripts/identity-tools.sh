#!/usr/bin/env bash
# What scripts/identity-slice (and its --explain) and scripts/identity-judge-cards share: the shared cache, the unmatched-cluster capture
# fetched from wherever it is named, and a tree's worker test classpath compiled once. Sourcing it defines functions only.
#
#   identity_capture <src> <cc>...  -> prints the directory holding <cc>.json.gz for each cc, and (a run's or a ref's)
#        the judgements that judge it, identity-judgements.tsv (a ref from before they were one file: its labels.tsv)
#   identity_judgements <dir>       -> the judgements that judge the capture in <dir>: its own, else this checkout's
#        src: "" (this checkout's), a directory, an "Identity capture" run id, or a git ref
#   identity_classpath <tree>       -> prints the tree's worker test classpath, sbt run only when a source is newer

IDENTITY_REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
IDENTITY_CACHE=${KINOWO_IDENTITY_CACHE:-$HOME/.cache/kinowo-identity}
IDENTITY_FIXTURES=test/resources/fixtures/identity-unmatched
IDENTITY_JUDGEMENTS=test/resources/fixtures/identity-judgements.tsv
identity_log() { echo "[${IDENTITY_TOOL:-identity}] $*" >&2; }

identity_capture() {
  local src=$1 dir sha cc; shift
  if [ -z "$src" ]; then echo "$IDENTITY_REPO/$IDENTITY_FIXTURES"; return; fi
  if [ -d "$src" ]; then (cd "$src" && pwd); return; fi
  if [[ $src =~ ^[0-9]{6,}$ ]]; then
    dir=$IDENTITY_CACHE/captures/run-$src
    for cc in "$@"; do
      [ -f "$dir/$cc.json.gz" ] && continue
      identity_log "downloading identity-capture-$cc of run $src"
      mkdir -p "$dir/dl-$cc"
      gh run download "$src" -R "${KINOWO_REPO:-pawelkrupinski/movies}" --name "identity-capture-$cc" --dir "$dir/dl-$cc" >&2 || return 1
      cp "$(find "$dir/dl-$cc" -name "$cc.json.gz" | head -1)" "$dir/$cc.json.gz"
    done
    # a run's capture is judged by this checkout's judgements
    cp "$IDENTITY_REPO/$IDENTITY_JUDGEMENTS" "$dir/identity-judgements.tsv"
    echo "$dir"; return
  fi
  sha=$(git -C "$IDENTITY_REPO" rev-parse --verify "$src^{commit}") || { echo "no directory, run id or ref $src" >&2; return 2; }
  dir=$IDENTITY_CACHE/captures/ref-$sha
  mkdir -p "$dir"
  for cc in "$@"; do
    [ -f "$dir/$cc.json.gz" ] && continue
    git -C "$IDENTITY_REPO" show "$sha:$IDENTITY_FIXTURES/$cc.json.gz" > "$dir/$cc.json.gz.part" && mv "$dir/$cc.json.gz.part" "$dir/$cc.json.gz"
  done
  # a ref's capture is judged by that ref's judgements -- or, from before they were one file, its labels.tsv, which
  # tools.IdentityJudgements reads too
  [ -s "$dir/identity-judgements.tsv" ] || git -C "$IDENTITY_REPO" show "$sha:$IDENTITY_JUDGEMENTS" > "$dir/identity-judgements.tsv" 2>/dev/null \
    || git -C "$IDENTITY_REPO" show "$sha:$IDENTITY_FIXTURES/labels.tsv" > "$dir/identity-judgements.tsv"
  echo "$dir"
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
  if [ ! -f "$dir/build.sbt" ]; then
    identity_log "laying out $ref ($sha) in $dir"
    rm -rf "$dir.part"; mkdir -p "$dir.part"
    git -C "$IDENTITY_REPO" archive "$sha" -- . ':(exclude)test/resources' ':(exclude)ios' ':(exclude)android' | tar -x -C "$dir.part"
    # Seed the compile from this checkout's (APFS clones, no copy): sbt recompiles only what the ref changed.
    for t in project/target common/target identity/target testkit/target worker/target target; do
      if [ -d "$IDENTITY_REPO/$t" ]; then mkdir -p "$dir.part/$(dirname "$t")"; cp -cR "$IDENTITY_REPO/$t" "$dir.part/$t" 2>/dev/null || cp -R "$IDENTITY_REPO/$t" "$dir.part/$t"; fi
    done
    rm -rf "$dir.part/target/identity-tools"
    mv "$dir.part" "$dir"
  fi
  # copied only when it changed: an unchanged copy keeps its time, and the ref is not compiled again
  for t in "$@"; do
    mkdir -p "$(dirname "$dir/$t")"
    cmp -s "$IDENTITY_REPO/$t" "$dir/$t" 2>/dev/null || cp "$IDENTITY_REPO/$t" "$dir/$t"
  done
  echo "$dir"
}

# identity_recording <run> <cc> -> the directory holding that "Record scrape fixtures" run's corpus/ and enrichment-<cc>/,
# downloaded once into the shared cache (scripts/identity-fetch-recording.sh), one download at a time across agents
identity_recording() {
  local run=$1 cc=$2 dir=$IDENTITY_CACHE/recordings/$1 lock
  if [ ! -f "$dir/pair/test/resources/fixtures/corpus/cinema-scrapes-$cc.json.gz" ] || [ ! -d "$dir/pair/test/resources/fixtures/enrichment-$cc" ]; then
    lock=$IDENTITY_CACHE/recordings/.lock-$run-$cc
    mkdir -p "$IDENTITY_CACHE/recordings"
    until mkdir "$lock" 2>/dev/null; do identity_log "waiting for another download of recording $run ($cc)"; sleep 10; done
    if [ ! -d "$dir/pair/test/resources/fixtures/enrichment-$cc" ]; then
      identity_log "downloading recording $run ($cc) into $dir"
      "$IDENTITY_REPO/scripts/identity-fetch-recording.sh" "$run" "$dir" "$cc" >&2 || { rmdir "$lock"; return 1; }
    fi
    rmdir "$lock"
  fi
  echo "$dir/pair/test/resources/fixtures"
}

# identity_newest_recording [cc] -> the id of the newest successful "Record scrape fixtures" run; with a country, the
# newest that actually holds that country's corpus (already cached, or a live scrape-fixtures-<cc> artifact), each newer
# run skipped said on stderr. 1 when none of the newest 20 holds it.
identity_newest_recording() {
  local cc=${1:-} repo=${KINOWO_REPO:-pawelkrupinski/movies} id ids
  ids=$(gh run list -R "$repo" --workflow "Record scrape fixtures" --status success -L "$([ -n "$cc" ] && echo 20 || echo 1)" --json databaseId -q '.[].databaseId') || return 1
  [ -n "$cc" ] || { echo "${ids%%$'\n'*}"; return; }
  for id in $ids; do
    if [ -f "$IDENTITY_CACHE/recordings/$id/pair/test/resources/fixtures/corpus/cinema-scrapes-$cc.json.gz" ] ||
       [ -n "$(gh api "repos/$repo/actions/runs/$id/artifacts?per_page=100" --paginate -q ".artifacts[] | select(.name == \"scrape-fixtures-$cc\" and (.expired | not)) | .name")" ]; then
      identity_log "recording $id: the newest holding a $cc corpus"
      echo "$id"; return
    fi
    identity_log "recording $id skipped: it holds no $cc corpus (no live scrape-fixtures-$cc artifact)"
  done
  identity_log "none of the newest 20 successful recordings holds a $cc corpus: name one with --recording"
  return 1
}

identity_classpath() {
  local tree=$1 cp=$1/target/identity-tools/classpath log=$1/target/identity-tools/sbt.log
  if [ -s "$cp" ] && [ -z "$(find "$tree/common/src" "$tree/identity/src" "$tree/worker/src" "$tree/testkit/src" "$tree/build.sbt" -newer "$cp" -name '*.s*' -print -quit 2>/dev/null)" ]; then
    cat "$cp"; return
  fi
  mkdir -p "$(dirname "$cp")"
  identity_log "compiling $tree (sbt; then cached until a source changes)"
  if ! (cd "$tree" && sbt -batch "worker/Test/compile" "export worker/Test/fullClasspath") > "$log" 2>&1; then
    tail -40 "$log" >&2; identity_log "compile failed in $tree (log: $log)"; return 1
  fi
  grep -E '^/[^ ]*worker/target/[^ ]*test-classes' "$log" | tail -n 1 > "$cp"
  [ -s "$cp" ] || { tail -40 "$log" >&2; identity_log "no classpath in sbt's output ($log)"; return 1; }
  cat "$cp"
}

# The JVM every identity tool runs in: English number formats, no Unsafe warnings.
# shellcheck disable=SC2034 # read by the scripts sourcing this
IDENTITY_JAVA=(java -Xmx6g -Duser.language=en -Duser.country=US --sun-misc-unsafe-memory-access=allow)
