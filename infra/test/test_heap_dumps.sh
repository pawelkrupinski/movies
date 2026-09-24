#!/usr/bin/env bash
# Cases for nix/files/heap-dumps.sh -- the budget that keeps every pod's heap dumps from filling
# k3s-worker-1's disk, run both in each container before its JVM starts and from the node's timer.
#
# IT IS TESTED HERE BECAUSE THE ONLY OTHER WAY TO EXERCISE IT IS TO OOM A PRODUCTION JVM. Every
# case builds a throwaway directory, back-dates files with `touch -t`, runs the real script and
# reads what is left. Sizes are scaled down with the HEAPDUMP_* overrides; the rules are the same.
#
# Run: bash infra/test/test_heap_dumps.sh
set -uo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
script="$here/../nix/files/heap-dumps.sh"
# shellcheck source=scripts/shell-spec.sh
. "$here/../../scripts/shell-spec.sh"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

# dump <path> <bytes> <touch -t stamp>   -- a fake dump of an exact size and write time.
dump() { mkdir -p "$(dirname "$1")"; head -c "$2" /dev/zero > "$1"; touch -t "$3" "$1"; }
mtime() { stat -c %Y "$1" 2>/dev/null || stat -f %m "$1"; }
# The file names left in a directory, sorted, space-separated. The fixtures are all plain names.
# shellcheck disable=SC2012
left() { (cd "$1" && ls -1 2>/dev/null | sort | tr '\n' ' ' | sed 's/ $//'); }
# A mountinfo naming every directory `prune` is pointed at as a mount point -- the hostPath case,
# which is what the cases below describe. The fallback cases pass one that does not.
mounted="$tmp/mountinfo-hostpath"
run()  { if [ "${1:-}" = prune ] && [ -d "${2:-}" ]; then
           echo "1 2 0:1 / $(cd "$2" && pwd -P) rw - ext4 /dev/sda2 rw" >> "$mounted"; fi
         HEAPDUMP_KEEP="${KEEP:-3}" HEAPDUMP_MAX_BYTES="${MAX:-1000000}" \
         HEAPDUMP_TOTAL_MAX_BYTES="${TOTAL:-1000000}" HEAPDUMP_MOUNTINFO="${MOUNTINFO:-$mounted}" \
         bash "$script" "$@"; }

echo "prune: count limit"
d="$tmp/count"
dump "$d/a.hprof" 10 202601010000; dump "$d/b.hprof" 10 202601020000
dump "$d/c.hprof" 10 202601030000; dump "$d/d.hprof" 10 202601040000
dump "$d/e.hprof.gz" 10 202601050000
run prune "$d" >/dev/null
check "keeps the 3 newest (.hprof and .hprof.gz alike), deletes the 2 oldest" \
  "c.hprof d.hprof e.hprof.gz" "$(left "$d")"

echo "prune: size limit"
d="$tmp/size"
dump "$d/old.hprof" 400 202601010000; dump "$d/mid.hprof" 400 202601020000
dump "$d/new.hprof" 400 202601030000
MAX=1000 run prune "$d" >/dev/null
check "under the count limit but over 1000 bytes: the OLDEST goes, not the biggest or newest" \
  "mid.hprof new.hprof" "$(left "$d")"
d="$tmp/huge"
dump "$d/huge.hprof" 2000 202601010000
MAX=1000 run prune "$d" >/dev/null
check "a single settled dump bigger than the whole budget is deleted" "" "$(left "$d")"

echo "prune: oldest first by write time, not by name"
d="$tmp/order"
dump "$d/zzz.hprof" 10 202601010000; dump "$d/aaa.hprof" 10 202601030000
dump "$d/mmm.hprof" 10 202601020000
KEEP=1 run prune "$d" >/dev/null
check "keeps the newest-written, whatever it is called" "aaa.hprof" "$(left "$d")"

echo "prune: a dump being written is never deleted"
d="$tmp/inprogress"
dump "$d/old1.hprof" 10 202601010000; dump "$d/old2.hprof" 10 202601020000
head -c 10 /dev/zero > "$d/writing.hprof"   # mtime = now
KEEP=1 run prune "$d" >/dev/null
check "the fresh file counts toward the limit, so BOTH older ones go, and it survives" \
  "writing.hprof" "$(left "$d")"
d="$tmp/inprogress-only"
head -c 5000 /dev/zero > "$d.hprof"; mkdir -p "$d"; mv "$d.hprof" "$d/writing.hprof"
out="$(MAX=1000 run prune "$d")"
check "...even when it alone is over the size budget" "writing.hprof" "$(left "$d")"
check "...and says so in the log" "1" "$(grep -c 'kept .*writing.hprof' <<< "$out")"
d="$tmp/tmpfiles"
dump "$d/x.hprof.gz.tmp" 10 202601010000
head -c 10 /dev/zero > "$d/y.hprof.gz.tmp"
run prune "$d" >/dev/null
check "a settled .tmp from a killed compression is removed, a fresh one is left alone" \
  "y.hprof.gz.tmp" "$(left "$d")"

echo "prune: empty or missing directory"
mkdir -p "$tmp/empty"
out="$(run prune "$tmp/empty")"; rc=$?
check "an empty directory is fine" "0" "$rc"
check "...and logs zero files" "heap-dumps: dir=$tmp/empty files=0 bytes=0 keep=3 max_bytes=1000000" "$out"
out="$(run prune "$tmp/does-not-exist")"; rc=$?
check "a missing directory is fine too" "0" "$rc"
check "...and says it is absent" "heap-dumps: dir=$tmp/does-not-exist absent, nothing to prune" "$out"
out="$(run prune-all "$tmp/no-root")"; rc=$?
check "prune-all over a missing root exits 0" "0" "$rc"
mkdir -p "$tmp/empty-root"
out="$(run prune-all "$tmp/empty-root")"; rc=$?
check "prune-all over a root with no directories exits 0" "0" "$rc"

echo "prune: the fixed java_pid1.hprof name is rotated, not lost"
d="$tmp/pid1"
dump "$d/java_pid1.hprof" 10 202601020304
run prune "$d" >/dev/null
check "renamed to its own write time (oom-<mtime UTC>), so the next OOM can write" "1" "$(left "$d" | grep -cE '^oom-2026010[12]T[0-9]{2}0400Z-pid1\.hprof$')"

echo "prune: off the hostPath (older manifests), keep ONE dump so the pod's volume cannot overflow"
d="$tmp/emptydir"
dump "$d/a.hprof" 10 202601010000; dump "$d/b.hprof" 10 202601020000
dump "$d/c.hprof" 10 202601030000
echo "1 2 0:1 / /data rw - ext4 /dev/sda2 rw" > "$tmp/mountinfo-emptydir"
out="$(MOUNTINFO="$tmp/mountinfo-emptydir" run prune "$d")"
check "a plain directory inside /data keeps only the newest" "c.hprof" "$(left "$d")"
check "...and says why" "1" "$(grep -c 'is not the node.s heap-dump hostPath' <<< "$out")"
d="$tmp/emptydir-writing"
dump "$d/a.hprof" 10 202601010000; mkdir -p "$d"; head -c 10 /dev/zero > "$d/writing.hprof"
MOUNTINFO="$tmp/mountinfo-emptydir" run prune "$d" >/dev/null
check "...still never deleting a dump being written" "writing.hprof" "$(left "$d")"
d="$tmp/nomountinfo"
dump "$d/a.hprof" 10 202601010000; dump "$d/b.hprof" 10 202601020000
MOUNTINFO="$tmp/does-not-exist" run prune "$d" >/dev/null
check "an unreadable mountinfo falls back to the safe side (one dump)" "b.hprof" "$(left "$d")"
d="$tmp/onhostpath"
dump "$d/a.hprof" 10 202601010000; dump "$d/b.hprof" 10 202601020000
run prune "$d" >/dev/null
check "on its own mount (the hostPath) the full budget applies" "a.hprof b.hprof" "$(left "$d")"

echo "prune-all: per directory, then the total across directories"
r="$tmp/root"
dump "$r/web-pl/a.hprof" 100 202601010000; dump "$r/web-pl/b.hprof" 100 202601040000
dump "$r/worker-us/c.hprof" 100 202601020000; dump "$r/worker-us/d.hprof" 100 202601030000
dump "$r/worker-us/e.hprof" 100 202601050000
KEEP=2 TOTAL=300 run prune-all "$r" >/dev/null
check "worker-us drops its oldest to meet KEEP=2" "d.hprof e.hprof" "$(left "$r/worker-us")"
check "then the 400-byte total drops the globally oldest (web-pl's a) to reach 300" \
  "b.hprof" "$(left "$r/web-pl")"

echo "compress"
r="$tmp/gz"
dump "$r/web-pl/old.hprof" 1000 202601010000
head -c 1000 /dev/zero > "$r/web-pl/fresh.hprof"
run compress "$r" >/dev/null
check "a settled dump is gzipped in place, a fresh one is left for its writer" \
  "fresh.hprof old.hprof.gz" "$(left "$r/web-pl")"
check "the .gz keeps the dump's write time, so oldest-first still means what it says" \
  "$(touch -t 202601010000 "$tmp/ref"; mtime "$tmp/ref")" "$(mtime "$r/web-pl/old.hprof.gz")"
check "and it decompresses to the original" "1000" "$(gunzip -c "$r/web-pl/old.hprof.gz" | wc -c | tr -d ' ')"

echo "report"
r="$tmp/rep"
dump "$r/web-pl/a.hprof" 100 202601010000; dump "$r/web-pl/b.hprof.gz" 50 202601020000
mkdir -p "$r/worker-de"
out="$(TOTAL=12345 run report "$r")"
check "files per directory" "kinowo_heapdumps_files{dir=\"web-pl\"} 2" "$(grep '^kinowo_heapdumps_files{dir="web-pl"}' <<< "$out")"
check "bytes per directory" "kinowo_heapdumps_bytes{dir=\"web-pl\"} 150" "$(grep '^kinowo_heapdumps_bytes{dir="web-pl"}' <<< "$out")"
check "an empty directory reports zero, not nothing" "kinowo_heapdumps_files{dir=\"worker-de\"} 0" "$(grep '^kinowo_heapdumps_files{dir="worker-de"}' <<< "$out")"
check "the budget is published beside what it bounds" "kinowo_heapdumps_budget_bytes 12345" "$(grep '^kinowo_heapdumps_budget_bytes' <<< "$out")"
check "a missing root still yields a parseable file with the budget" "1" \
  "$(run report "$tmp/nope" | grep -c '^kinowo_heapdumps_budget_bytes')"

echo "dump-file"
check "names pod, country and start time for the web (KINOWO_COUNTRY)" "1" \
  "$(BIN=web KINOWO_COUNTRY=pl HOSTNAME=web-pl-7c9f-x2k4 bash "$script" dump-file /data/heapdumps \
      | grep -cE '^/data/heapdumps/web-pl_web-pl-7c9f-x2k4_[0-9]{8}T[0-9]{6}Z\.hprof$')"
check "...and for the worker (KINOWO_COUNTRIES)" "1" \
  "$(env -u KINOWO_COUNTRY BIN=worker KINOWO_COUNTRIES=us HOSTNAME=worker-us-abc bash "$script" dump-file /d \
      | grep -cE '^/d/worker-us_worker-us-abc_[0-9]{8}T[0-9]{6}Z\.hprof$')"

echo "usage"
bash "$script" nonsense /x >/dev/null 2>&1; rc=$?
check "an unknown command is refused" "2" "$rc"

spec_summary
