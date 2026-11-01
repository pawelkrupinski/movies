#!/usr/bin/env bash
# One cache's outcome as a `ci-cache <name> <hit|partial|miss> <key>` log line and a {"kind":"cache",...} line in the
# job's metrics file ($CI_METRICS_FILE; .github/actions/job-telemetry, scripts/ci/ci_trends.py): the restore's time
# and the restored size, so `ci-timings` reports hit rates and what a miss costs.
#
#   cache-marker.sh begin <name>                          # before the restore: remember when it started
#   cache-marker.sh end <name> <exact-hit> <key> <path>...  # after it: <exact-hit> is actions/cache's cache-hit
#
# Without an exact hit, the outcome is `partial` when any <path> holds files after the restore (a restore-key
# matched), else `miss`: the combined actions/cache reports no matched key (twenty-runners doc, section 4).
# NEVER FAILS: exits 0 whatever happens. `du` over a restored tree is ~0.2 s for the sbt target cache.
set -u
name="${2:-cache}"
stamp="${RUNNER_TEMP:-/tmp}/ci-cache-$name.start"
out="${CI_METRICS_FILE:-${RUNNER_TEMP:-/tmp}/ci-metrics/metrics.jsonl}"

case "${1:-}" in
  begin)
    date +%s.%N > "$stamp" 2>/dev/null
    ;;
  end)
    exact="${3:-}" key="${4:-}"
    shift 4 2>/dev/null || shift $#
    status=miss kb=0
    for path in "$@"; do
      case "$path" in '!'*) continue ;; esac
      for expanded in ${path/#\~/$HOME}; do
        [ -e "$expanded" ] || continue
        size=$(du -sk "$expanded" 2>/dev/null | awk '{ print $1 }')
        kb=$((kb + ${size:-0}))
      done
    done
    if [ "$exact" = true ]; then status=hit; elif [ "$kb" -gt 64 ]; then status=partial; fi
    seconds=$(awk -v a="$(cat "$stamp" 2>/dev/null || echo 0)" -v b="$(date +%s.%N)" 'BEGIN { if (a > 0) printf "%.1f", b - a; else print "null" }')
    echo "ci-cache $name $status $key"
    mkdir -p "$(dirname "$out")" 2>/dev/null
    printf '{"kind":"cache","cache":"%s","status":"%s","key":"%s","size_mb":%d,"restore_s":%s}\n' \
      "$name" "$status" "${key//\"/}" $((kb / 1024)) "$seconds" >> "$out" 2>/dev/null
    ;;
esac
exit 0
