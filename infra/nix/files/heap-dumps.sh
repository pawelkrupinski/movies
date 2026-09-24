#!/usr/bin/env bash
# Keep the JVM heap dumps of every kinowo pod bounded, oldest deleted first.
#
#   heap-dumps.sh dump-file <dir>     # prints the -XX:HeapDumpPath FILE this JVM start should use
#   heap-dumps.sh prune <dir>         # one app-country directory: count + size budget
#   heap-dumps.sh prune-all <root>    # every <root>/<app>-<country>/, then a total budget over all
#   heap-dumps.sh compress <root>     # gzip every settled .hprof under <root> (one level of subdirs)
#   heap-dumps.sh report <root>       # Prometheus text: files/bytes/newest per directory
#
# ONE FILE, TWO CALLERS. The image copies it to bin/heap-dumps.sh (build.sbt) and the container
# CMD runs `dump-file` + `prune` against its OWN directory before the JVM starts. The node runs
# `compress` + `prune-all` + `report` from a systemd timer (modules/fleet/heap-dumps.nix) over the
# whole hostPath, so the budget holds even for a pod that never restarts. It lives under infra/
# because the flake root is infra/ and nix cannot read a file outside it; the image can read
# anything in the repo.
#
# WHERE THE DUMPS LIVE: /var/lib/kinowo/heapdumps/<app>-<country>/ on k3s-worker-1, a hostPath
# every web and worker pod mounts at /data/heapdumps through a per-app-country subPath. It
# outlives the pod, which the web tier's emptyDir did not: web-pl heap-OOMed on 2026-09-23 and
# the dump went with the pod. See docs/heap-dumps.md.
#
# ------------------------------------------------------------------------------------------------
# THE BUDGET, and why these numbers
# ------------------------------------------------------------------------------------------------
#
# A dump is roughly the LIVE heap, so it is at most -Xmx: 384m for most web pods, 1024m web-us,
# 1280m worker-us, whose one real dump measured 1.39 GB. The node's root disk is 150G with ~129G
# free (2026-09-24), shared with the container image store (~13G after its daily collection).
#
#   HEAPDUMP_KEEP=3                   per app-country. The newest explains the latest death; two
#                                     more say whether it is the same death. Beyond that it is a
#                                     crash loop, and the first three of a loop are the evidence.
#   HEAPDUMP_MAX_BYTES=4 GiB          per app-country: three worst-case worker-us dumps fit.
#   HEAPDUMP_TOTAL_MAX_BYTES=16 GiB   over all ten app-countries. ~11% of the disk: with images and
#                                     the OS the root stays near 30%, far below the "under 40%
#                                     free" clause of FilesystemWillFillWithin7Days, so forensics
#                                     can never be what pages about the disk.
#
# Defaults live HERE rather than in a ConfigMap on purpose -- the Dockerfile's note on the old
# HEAPDUMP_KEEP says why: a ConfigMap value on this fleet reaches the cluster only by hand. The
# environment overrides exist for the tests.
#
# ------------------------------------------------------------------------------------------------
# A DUMP BEING WRITTEN IS NEVER DELETED
# ------------------------------------------------------------------------------------------------
#
# The JVM writes straight to its final name, so an in-progress dump looks like any other .hprof.
# Anything modified within HEAPDUMP_SETTLE_MINUTES (10) is left alone by every command here: it
# still COUNTS toward the budget, so older files go first, but it is never itself removed,
# renamed or compressed. A 1.4 GB dump takes well under a minute to write. `compress` writes to
# `<name>.gz.tmp` and renames, so a half-written .gz is never counted as a dump; a stale .tmp (a
# killed compression) is cleaned up once it has settled.
set -euo pipefail

KEEP="${HEAPDUMP_KEEP:-3}"
MAX_BYTES="${HEAPDUMP_MAX_BYTES:-$((4 * 1024 * 1024 * 1024))}"
TOTAL_MAX_BYTES="${HEAPDUMP_TOTAL_MAX_BYTES:-$((16 * 1024 * 1024 * 1024))}"
SETTLE_SECONDS=$(( ${HEAPDUMP_SETTLE_MINUTES:-10} * 60 ))

log() { echo "heap-dumps: $*"; }

# GNU first (the image and the node), BSD second (a developer's macOS running the tests).
mtime_of() { stat -c %Y "$1" 2>/dev/null || stat -f %m "$1"; }
size_of()  { stat -c %s "$1" 2>/dev/null || stat -f %z "$1"; }
utc_stamp() { date -u -d "@$1" +%Y%m%dT%H%M%SZ 2>/dev/null || date -u -r "$1" +%Y%m%dT%H%M%SZ; }

now=$(date +%s)
settled() { [ $(( now - $(mtime_of "$1") )) -ge "$SETTLE_SECONDS" ]; }

# "<mtime> <bytes> <path>" per dump in the given directories, OLDEST FIRST. A dump is a .hprof
# or a compressed .hprof.gz; .tmp files are never dumps.
list_dumps() {
  local d f
  for d in "$@"; do
    [ -d "$d" ] || continue
    for f in "$d"/*.hprof "$d"/*.hprof.gz; do
      [ -f "$f" ] || continue
      echo "$(mtime_of "$f") $(size_of "$f") $f"
    done
  done | sort -n -k1,1 -k3,3
}

# Delete oldest-first until at most <keep> files and <max> bytes remain. Unsettled files count
# but are skipped.
enforce() {
  local keep="$1" max="$2" scope="$3"; shift 3
  local listing count=0 total=0 m s p
  listing="$(list_dumps "$@")"
  [ -n "$listing" ] || return 0
  while read -r m s p; do count=$((count + 1)); total=$((total + s)); done <<< "$listing"
  while read -r m s p; do
    if [ "$count" -le "$keep" ] && [ "$total" -le "$max" ]; then break; fi
    if [ $(( now - m )) -lt "$SETTLE_SECONDS" ]; then
      log "kept $p ($s bytes): written within the last $((SETTLE_SECONDS / 60)) min, may still be in progress"
      continue
    fi
    rm -f -- "$p"
    count=$((count - 1)); total=$((total - s))
    log "deleted $p ($s bytes) to bring $scope under $keep files / $max bytes"
  done <<< "$listing"
}

# Stale temporaries from a compression that was killed part way.
drop_stale_tmp() {
  local f
  for f in "$1"/*.tmp; do
    [ -f "$f" ] || continue
    if settled "$f"; then rm -f -- "$f"; log "deleted stale temporary $f"; fi
  done
}

# The fixed name a JVM picks when -XX:HeapDumpPath is a directory (java_pid1.hprof, since the JVM
# is pid 1 in a container). It refuses to overwrite it, so every OOM after the first would write
# nothing. Only an image older than dump-file produces it; rename it to its own write time.
rotate_fixed_names() {
  local f
  for f in "$1"/java_pid*.hprof; do
    [ -f "$f" ] || continue
    settled "$f" || continue
    mv -- "$f" "$1/oom-$(utc_stamp "$(mtime_of "$f")")-${f##*/java_}"
  done
}

summary() {
  local files=0 bytes=0 m s p listing
  listing="$(list_dumps "$1")"
  if [ -n "$listing" ]; then
    while read -r m s p; do files=$((files + 1)); bytes=$((bytes + s)); done <<< "$listing"
  fi
  log "dir=$1 files=$files bytes=$bytes keep=$KEEP max_bytes=$MAX_BYTES"
}

cmd="${1:-}"; shift || true
case "$cmd" in
  dump-file)
    dir="${1:?usage: heap-dumps.sh dump-file <dir>}"
    # POD, COUNTRY AND START TIME, so no two JVM starts can ever name the same file. The worker
    # names its country KINOWO_COUNTRIES, the web KINOWO_COUNTRY (see Country.soleFromEnv).
    country="${KINOWO_COUNTRY:-${KINOWO_COUNTRIES:-unknown}}"
    echo "$dir/${BIN:-app}-${country}_${HOSTNAME:-$(hostname)}_$(utc_stamp "$now").hprof"
    ;;
  prune)
    dir="${1:?usage: heap-dumps.sh prune <dir>}"
    if [ ! -d "$dir" ]; then log "dir=$dir absent, nothing to prune"; exit 0; fi
    rotate_fixed_names "$dir"
    drop_stale_tmp "$dir"
    enforce "$KEEP" "$MAX_BYTES" "$dir" "$dir"
    summary "$dir"
    ;;
  prune-all)
    root="${1:?usage: heap-dumps.sh prune-all <root>}"
    if [ ! -d "$root" ]; then log "root=$root absent, nothing to prune"; exit 0; fi
    dirs=()
    for d in "$root"/*/; do if [ -d "$d" ]; then dirs+=("${d%/}"); fi; done
    [ "${#dirs[@]}" -gt 0 ] || { log "root=$root holds no dump directories"; exit 0; }
    for d in "${dirs[@]}"; do
      rotate_fixed_names "$d"
      drop_stale_tmp "$d"
      enforce "$KEEP" "$MAX_BYTES" "$d" "$d"
    done
    # THE TOTAL LAST, across directories: a country that crash-loops must not keep another
    # country's older evidence alive past the node's budget, and must not be spared its own.
    enforce 2147483647 "$TOTAL_MAX_BYTES" "$root" "${dirs[@]}"
    for d in "${dirs[@]}"; do summary "$d"; done
    ;;
  compress)
    root="${1:?usage: heap-dumps.sh compress <root>}"
    [ -d "$root" ] || exit 0
    # hprof is mostly zeroed padding and repeated ids: gzip -1 takes a 1.4 GB dump to roughly a
    # quarter in ~15s of one core, which is why it runs on the node's timer and never on a boot.
    for f in "$root"/*.hprof "$root"/*/*.hprof; do
      [ -f "$f" ] || continue
      settled "$f" || continue
      gzip -1 -c -- "$f" > "$f.gz.tmp"
      touch -r "$f" "$f.gz.tmp"
      mv -- "$f.gz.tmp" "$f.gz"
      rm -f -- "$f"
      log "compressed $f -> $f.gz ($(size_of "$f.gz") bytes)"
    done
    ;;
  report)
    root="${1:?usage: heap-dumps.sh report <root>}"
    echo "# HELP kinowo_heapdumps_files Heap dumps kept on the node, per app-country directory."
    echo "# TYPE kinowo_heapdumps_files gauge"
    echo "# HELP kinowo_heapdumps_bytes Bytes of heap dumps kept on the node, per app-country directory."
    echo "# TYPE kinowo_heapdumps_bytes gauge"
    echo "# HELP kinowo_heapdumps_newest_timestamp_seconds Write time of the newest heap dump per directory (0 = none)."
    echo "# TYPE kinowo_heapdumps_newest_timestamp_seconds gauge"
    if [ -d "$root" ]; then
      for d in "$root"/*/; do
        [ -d "$d" ] || continue
        d="${d%/}"; name="${d##*/}"
        files=0; bytes=0; newest=0
        listing="$(list_dumps "$d")"
        if [ -n "$listing" ]; then
          while read -r m s p; do
            files=$((files + 1)); bytes=$((bytes + s))
            if [ "$m" -gt "$newest" ]; then newest=$m; fi
          done <<< "$listing"
        fi
        echo "kinowo_heapdumps_files{dir=\"$name\"} $files"
        echo "kinowo_heapdumps_bytes{dir=\"$name\"} $bytes"
        echo "kinowo_heapdumps_newest_timestamp_seconds{dir=\"$name\"} $newest"
      done
    fi
    echo "# HELP kinowo_heapdumps_budget_bytes The total the node-side prune holds all heap dumps under."
    echo "# TYPE kinowo_heapdumps_budget_bytes gauge"
    echo "kinowo_heapdumps_budget_bytes $TOTAL_MAX_BYTES"
    ;;
  *)
    echo "usage: heap-dumps.sh {dump-file|prune|prune-all|compress|report} <dir>" >&2
    exit 2
    ;;
esac
