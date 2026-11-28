#!/usr/bin/env bash
# Purpose: What the local identity tools share (sourced): the recording cache, classpaths, countries
# What the local identity tools (identity-slice and its --explain, identity-judge-cards, venue-replay, title-trace,
# recording-diff, hard-clusters.sh, listing-history) share: the shared cache, a recording or a capture downloaded once
# into it, a ref's sources laid out once, a tree's worker test classpath compiled once. Sourcing it defines functions only: the shim of
# scripts/lib/identity_tools.py (the cache, the captures, the trees, the classpaths, the recordings, the verdict record),
# and what must run in the calling shell (a lock held around a shell function, a JVM slot, a kept JVM).
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
#   identity_push_measure_branch <remote> <commit> <branch> -> the commit pushed as refs/heads/<branch>, one such push
#        at a time on this machine; on a failure MEASURE_PUSH_ERROR says why, in the remote's words
#
# Each fails (non-zero, said on stderr) when what it needs cannot be read — gh unreachable, a download cut short — and
# never hands back a stale or partial copy instead. Every gh call is bounded (tool-conventions.sh's `bounded`); a wait for another agent's
# download, layout or compile of the same thing (identity_locked) has a deadline, and a lock is free the moment its
# holder has gone.

IDENTITY_REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
IDENTITY_LIB="$IDENTITY_REPO/scripts/lib"
# shellcheck source=scripts/tool-conventions.sh
. "$(dirname "${BASH_SOURCE[0]}")/tool-conventions.sh"
# shellcheck source=scripts/jvm-gate.sh
. "$(dirname "${BASH_SOURCE[0]}")/jvm-gate.sh"
IDENTITY_CACHE=$KINOWO_IDENTITY_CACHE
# shellcheck disable=SC2034 # read by the scripts sourcing this
IDENTITY_FIXTURES=test/resources/fixtures/identity-unmatched
# shellcheck disable=SC2034 # read by the scripts sourcing this
IDENTITY_JUDGEMENTS=test/resources/fixtures/identity-judgements.tsv
identity_log() { echo "[${IDENTITY_TOOL:-identity}] $*" >&2; }

# identity_gh <seconds> <gh args...> -> gh, killed when it outlasts <seconds> (a hung gh fails, it never waits forever)
identity_gh() { local seconds=$1; shift; bounded "$seconds" "$KINOWO_GH" "$@"; }
# shellcheck disable=SC2034 # read by the scripts sourcing this
IDENTITY_GH_READ=${KINOWO_IDENTITY_GH_TIMEOUT:-60}
# shellcheck disable=SC2034 # read by the scripts sourcing this
IDENTITY_GH_DOWNLOAD=${KINOWO_IDENTITY_DOWNLOAD_TIMEOUT:-1800}

# THE FUNCTIONS OF scripts/lib/identity_tools.py: each runs it, the repository and the settings this shell holds passed on
# (IDENTITY_REPO, IDENTITY_TOOL and IDENTITY_FILLS are often plain variables of the sourcing tool, never exported). Its
# stdout is the function's answer; a failure is said on stderr and is its status.
identity_py() {
  IDENTITY_REPO=$IDENTITY_REPO IDENTITY_TOOL=${IDENTITY_TOOL:-identity} IDENTITY_FILLS=${IDENTITY_FILLS:-} \
    KINOWO_GH=$KINOWO_GH KINOWO_GH_REPO=$KINOWO_GH_REPO KINOWO_IDENTITY_CACHE=$IDENTITY_CACHE \
    python3 -I -B "$IDENTITY_LIB/identity_tools.py" "$@"
}
identity_capture() { identity_py capture "$@"; }
identity_judgements() { identity_py judgements "$@"; }
identity_tree() { identity_py tree "$@"; }
identity_tree_judgements() { identity_py tree-judgements "$@"; }
identity_tools_hash() { identity_py tools-hash "$@"; }
identity_classpath() { identity_py classpath "$@"; }
identity_classpath_fresh() { identity_py classpath-fresh "$@"; }
identity_classpath_line() { identity_py classpath-line "$@"; }
identity_recording() { identity_py recording "$@"; }
identity_recording_cached() { identity_py recording-cached "$@"; }
identity_recording_run() { identity_py recording-run "$@"; }
identity_recorded() { identity_py recorded "$@"; }
identity_newest_recording() { identity_py newest-recording "$@"; }
identity_takes_download() { identity_py takes-download "$@"; }
identity_main_ci_over() { identity_py main-ci-over "$@"; }
identity_measure_branch() { identity_py measure-branch "$@"; }
what_if_verdicts_file() { identity_py what-if-verdicts-file "$@"; }
what_if_record_verdict() { identity_py what-if-record-verdict "$@"; }
what_if_verdicts() { identity_py what-if-verdicts "$@"; }
kept_jvm_call() { python3 -I -B "$IDENTITY_LIB/kept_jvm_call.py" "$@"; }

# identity_push_measure_branch <remote> <commit> <branch> -> <commit> pushed as refs/heads/<branch> (bounded), while no
# other measure/ push of this machine is under way, git's output passed through. Fails with MEASURE_PUSH_ERROR set to the
# remote's reason ("remote rejected (unable to migrate objects to permanent storage)").
identity_push_measure_branch() {
  local reason_file rc=0
  MEASURE_PUSH_ERROR=""
  reason_file=$(mktemp) || return 1
  identity_py push-measure-branch "$reason_file" "$@" || rc=$?
  # shellcheck disable=SC2034 # read by the callers
  MEASURE_PUSH_ERROR=$(cat "$reason_file")
  rm -f "$reason_file"
  return "$rc"
}

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

# identity_country_code <country name> -> its code (poland -> pl); identity_country_name <code> -> its name. Anything
# else passes through.
identity_country_code() { case "$1" in poland) echo pl ;; germany) echo de ;; united-kingdom) echo uk ;; spain) echo es ;; united-states) echo us ;; *) echo "$1" ;; esac; }
identity_country_name() { case "$1" in pl) echo poland ;; de) echo germany ;; uk) echo united-kingdom ;; es) echo spain ;; us) echo united-states ;; *) echo "$1" ;; esac; }

# identity_put <file> <command...> -> the command's output written to <file> whole or not at all: a reader never sees
# it half written, and two writers never interleave
identity_put() {
  local file=$1 part; shift
  mkdir -p "$(dirname "$file")"
  part=$(mktemp "$file.part.XXXXXX") || return 1
  if "$@" > "$part"; then mv -f "$part" "$file"; else rm -f "$part"; return 1; fi
}

# The JVM every identity tool runs in: English number formats ("0.97", never "0,97"), no Unsafe warnings, AWT headless
# (on macOS a JVM touching AWT without it registers as a foreground app and steals focus: every what-if resolve did;
# scripts/tool-jvm-headless-test.sh guards it). IDENTITY_SBT is sbt in the same locale, headless from .jvmopts (a heap
# and the commands appended: "${IDENTITY_SBT[@]}" -J-Xmx6g -batch ...).
IDENTITY_LOCALE=(-Duser.language=en -Duser.country=US)
# shellcheck disable=SC2034 # read by the scripts sourcing this
IDENTITY_JAVA=(java -Xmx6g "${IDENTITY_LOCALE[@]}" -Djava.awt.headless=true --sun-misc-unsafe-memory-access=allow)
# sized for a share of the cores on a Mac agents share (scripts/jvm-gate.sh): none, or one option
identity_cpus=$(jvm_cpu_options); [ -z "$identity_cpus" ] || IDENTITY_JAVA+=("$identity_cpus")
# shellcheck disable=SC2034 # read by the scripts sourcing this
IDENTITY_SBT=(sbt "${IDENTITY_LOCALE[@]/#/-J}")

# THE JVM SEQUENCES A PYTHON TOOL RUNS (identity-what-if's and identity-slice's, scripts/identity_what_if_cli.py and
# scripts/identity_slice_cli.py, through scripts/lib/tool_shell.py): a JVM slot (jvm-gate.sh) is a lock held on
# descriptor 7 by the PROCESS that took it and its children, so the take, the JVM it is for and the release are one bash
# process's, never the Python tool's.
#
# identity_java_slotted <label> <tree> <log> <java args...> -> IDENTITY_JAVA run in <tree> under a JVM slot of its own,
# its stdout and stderr into <log>; the JVM's status
identity_java_slotted() { local label=$1 tree=$2 log=$3; shift 3; identity_java_slotted_to "$label" "$tree" "$log" '&1' "$@"; }
# identity_java_slotted_to <label> <tree> <stdout> <stderr> <java args...> -> the same, its stdout into <stdout> and its
# stderr into <stderr>: `-` this call's own, a stderr of `&1` with the stdout
identity_java_slotted_to() {
  local label=$1 tree=$2 out=$3 err=$4 rc=0; shift 4
  jvm_slot_take "$label"
  ( [ "$out" = - ] || exec > "$out"
    case $err in -) ;; '&1') exec 2>&1 ;; *) exec 2> "$err" ;; esac
    cd "$tree" && "${IDENTITY_JAVA[@]}" "$@" ) || rc=$?
  jvm_slot_release
  return "$rc"
}
# identity_kept_jvm_start <label> <tree> <log> <pid file> <keeper log> <keeper> <java args...> -> a KEPT JVM started in
# <tree>, its pid in <pid file>: its own process group (set -m), outliving this call, the lock descriptors closed; the
# JVM slot taken for its memory handed to its KEEPER, `<keeper> --keep-served <pid>` (jvm_slot_keeper_start), which
# holds it for the JVM's whole life (identity_kept_jvm_keep)
identity_kept_jvm_start() {
  local label=$1 tree=$2 log=$3 pidf=$4 keeper_log=$5 keeper=$6; shift 6
  KINOWO_GOV_JOB=keeper jvm_slot_take "$label"   # the kept JVM's memory; each request its cores (identity_kept_jvm_request)
  (cd "$tree" && set -m && { nohup "${IDENTITY_JAVA[@]}" "$@" > "$log" 2>&1 < /dev/null 6>&- 7>&- 8>&- &
    echo $! > "$pidf"; })
  jvm_slot_keeper_start "$IDENTITY_REPO" "$keeper_log" "$keeper" --keep-served "$(cat "$pidf")"
}
# identity_kept_jvm_request <label> <job> <log> <socket> <args...> -> one request to a kept JVM (kept_jvm_call), its
# answer into <log> (`-`: on this call's stdout, as it streams), under a cores ticket of its own (jvm_cpu_take: the
# keeper holds only the memory); the request's status, 111 / 112 as kept_jvm_call's
identity_kept_jvm_request() {
  local label=$1 job=$2 log=$3 rc=0; shift 3
  jvm_cpu_take "$label" "$job"
  if [ "$log" = - ]; then kept_jvm_call "$@" || rc=$?; else kept_jvm_call "$@" > "$log" 2>&1 || rc=$?; fi
  jvm_slot_release
  return "$rc"
}
# identity_kept_jvm_keep <pid> <pattern> -> THE KEEPER's loop: runs while the kept JVM <pid> does, taking a JVM slot as
# soon as one is free when it started without, and ends once the JVM has (its idle exit, a stop, a crash), which frees
# the slot. Started the moment the JVM's process is, it may find it still the shell about to exec java (no <pattern> in
# its command line yet), so that pid counts as the JVM until it has been seen as one; after that, only while it still is.
identity_kept_jvm_keep() {
  local seen=0
  while kill -0 "$1" 2>/dev/null; do
    if ps -o args= -p "$1" 2>/dev/null | grep -q "$2"; then seen=1; elif [ "$seen" = 1 ]; then break; fi
    if jvm_slot_take_if_free; then identity_log "took JVM slot ${JVM_SLOT##*/}"; fi
    sleep "${KINOWO_IDENTITY_SLICE_KEEPER_POLL:-10}"
  done
}
