#!/usr/bin/env bash
# What scripts/identity-explain and scripts/identity-judge-cards share: the shared cache, the unmatched-cluster capture
# fetched from wherever it is named, and a tree's worker test classpath compiled once. Sourcing it defines functions only.
#
#   identity_capture <src> <cc>...  -> prints the directory holding <cc>.json.gz for each cc, and labels.tsv
#        src: "" (this checkout's), a directory, an "Identity capture" run id, or a git ref
#   identity_classpath <tree>       -> prints the tree's worker test classpath, sbt run only when a source is newer

IDENTITY_REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
IDENTITY_CACHE=${KINOWO_IDENTITY_CACHE:-$HOME/.cache/kinowo-identity}
IDENTITY_FIXTURES=test/resources/fixtures/identity-unmatched
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
    # a run's capture is judged by this checkout's labels
    cp "$IDENTITY_REPO/$IDENTITY_FIXTURES/labels.tsv" "$dir/labels.tsv"
    echo "$dir"; return
  fi
  sha=$(git -C "$IDENTITY_REPO" rev-parse --verify "$src^{commit}") || { echo "no directory, run id or ref $src" >&2; return 2; }
  dir=$IDENTITY_CACHE/captures/ref-$sha
  mkdir -p "$dir"
  for cc in "$@"; do
    [ -f "$dir/$cc.json.gz" ] && continue
    git -C "$IDENTITY_REPO" show "$sha:$IDENTITY_FIXTURES/$cc.json.gz" > "$dir/$cc.json.gz.part" && mv "$dir/$cc.json.gz.part" "$dir/$cc.json.gz"
  done
  [ -f "$dir/labels.tsv" ] || git -C "$IDENTITY_REPO" show "$sha:$IDENTITY_FIXTURES/labels.tsv" > "$dir/labels.tsv"
  echo "$dir"
}

identity_classpath() {
  local tree=$1 cp=$1/target/identity-tools/classpath log=$1/target/identity-tools/sbt.log
  if [ -s "$cp" ] && [ -z "$(find "$tree/common/src" "$tree/worker/src" "$tree/testkit/src" "$tree/build.sbt" -newer "$cp" -name '*.s*' -print -quit 2>/dev/null)" ]; then
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
