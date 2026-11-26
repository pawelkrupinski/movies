#!/usr/bin/env bash
# tool-conventions.sh (what every Bash tool shares): `die` a runtime failure (1, or the status a tool's contract names
# in DIE_STATUS) and `usage_error` a caller's mistake (2); one country flag, `--country cc[,cc]` repeatable and
# `--countries a,b` alike; and `gh_download`, every gh download bounded, retried on a failed read, never on an absent
# one, which it tells apart.
# Run: bash scripts/tool-conventions-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/tool-conventions.sh" # bounded: a parse loop that never ends fails here in seconds

printf '\033[36m▸\033[0m tool-conventions.sh\n'
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin"

# Every case takes well under a second (0.04-1 s on a laptop, ~2x that on a loaded runner), so CASE_BOUND, not the
# runner's 600 s for the whole test, ends one that stalls: Main #1873 spent all 600 s in the lone-comma case and the log
# said only TIMEOUT. A stalled case names itself and says what it was stuck on (scripts/stall-report.sh) on stderr.
CASE_BOUND=${CASE_BOUND:-120}
# shellcheck disable=SC2016 # the lines are run by the library's shell, expanded there
lib() { # <shell line> -> its stdout and stderr, then "exit=<status>"; run with the library sourced, as a tool named demo
  local status
  rm -f "$work/stall"
  STUB="$work" PATH="$work/bin:$PATH" STALL_REPORT_FILE="$work/stall" BOUNDED_ON_TIMEOUT="$REPO_ROOT/scripts/stall-report.sh" \
    bounded "$CASE_BOUND" bash -c '. "$1"; TOOL_NAME=demo; eval "$2"' _ "$REPO_ROOT/scripts/tool-conventions.sh" "$1" 2>&1
  status=$?
  [ ! -e "$work/stall" ] || { echo "lib: stalled past ${CASE_BOUND}s: $1"; cat "$work/stall"; } >&2
  echo "exit=$status"
}

# ── die / usage_error ─────────────────────────────────────────────────────────────────────────────
check "die: a runtime failure, said under the tool's name, exits 1" "demo: no such run|exit=1" "$(lib 'die no such run' | paste -sd'|' -)"
check "...a tool whose contract names another status for 'no verdict' sets it once" "demo: gh down|exit=2" \
  "$(lib 'DIE_STATUS=2; die gh down' | paste -sd'|' -)"
check "usage_error: the caller's mistake exits 2, pointing at --help" "demo: unknown option -x|demo: see demo --help|exit=2" \
  "$(lib 'usage_error unknown option -x' | paste -sd'|' -)"
check "...whatever DIE_STATUS says" "exit=2" "$(lib 'DIE_STATUS=1; usage_error bad' | tail -1)"
check "...the tool's name defaults to the script's own" "my-tool: x" \
  "$(printf '. %q\nusage_error x\n' "$REPO_ROOT/scripts/tool-conventions.sh" > "$work/my-tool"; bash "$work/my-tool" 2>&1 | sed -n 1p)"

# ── the country flag ──────────────────────────────────────────────────────────────────────────────
# a tool's argument loop, as every tool taking countries writes it
# shellcheck disable=SC2016
parse='while [ $# -gt 0 ]; do case "$1" in
  --country|--countries|--country=*|--countries=*) country_flag "$@"; shift "$COUNTRY_FLAG_ARGS" ;;
  *) rest+=("$1"); shift ;; esac; done; echo "$(selected_countries_csv) rest=${rest[*]:-}"'
countries() { lib "rest=(); set -- $*; $parse" | paste -sd'|' -; }
check "no flag: every country, in the one order" "pl,de,uk,us,es rest=x|exit=0" "$(countries x)"
check "--countries a,b" "uk,de rest=x|exit=0" "$(countries --countries uk,de x)"
check "--country, repeatable" "uk,de rest=x|exit=0" "$(countries --country uk x --country de)"
check "--country a,b (a list too)" "uk,de rest=|exit=0" "$(countries --country uk,de)"
check "--country=… and --countries=…, upper case and repeats folded" "de,pl rest=|exit=0" "$(countries --country=DE --countries=pl,de)"
check "a code no tool knows is a usage error" "demo: unknown country xx (one of pl,de,uk,us,es)|demo: see demo --help|exit=2" \
  "$(countries --countries pl,xx)"
check "a value naming no code (a lone comma) is a usage error, not every country" \
  "demo: --country names no country code (one of pl,de,uk,us,es)|demo: see demo --help|exit=2" "$(countries --country ,)"
mkdir -p "$work/glob" && touch "$work/glob/pl"
check "a glob is no country code, never the files it matches (here: pl)" "demo: unknown country * (one of pl,de,uk,us,es)|demo: see demo --help|exit=2" \
  "$(cd "$work/glob" && countries --country "'*'")"
check "a flag without its value is a usage error" "demo: --country needs a country code (one of pl,de,uk,us,es)|demo: see demo --help|exit=2" \
  "$(countries --country)"

# ── a flag's value ────────────────────────────────────────────────────────────────────────────────
# shellcheck disable=SC2016
valued='while [ $# -gt 0 ]; do case "$1" in --out) flag_value "$@"; out=$2; shift 2 ;; *) shift ;; esac; done; echo "out=$out"'
check "flag_value: a flag with its value passes it on" "out=dir|exit=0" "$(lib "set -- --out dir; $valued" | paste -sd'|' -)"
check "...an empty value is still a value" "out=|exit=0" "$(lib "set -- --out ''; $valued" | paste -sd'|' -)"
check "...the last argument, with no value after it, is a usage error (never a shift 2 that fails and loops)" \
  "demo: --out needs a value|demo: see demo --help|exit=2" "$(lib "set -- --out; $valued" | paste -sd'|' -)"

mkdir -p "$work/here/sub"
check "path_flag: a relative path made absolute against the caller's directory" "$(cd "$work/here" && pwd)/sub/f|exit=0" \
  "$(cd "$work/here" && lib 'path_flag --out sub/f' | paste -sd'|' -)"
check "...a directory that is not there is a usage error, never /<name>" "demo: --out nope/f: no directory nope|demo: see demo --help|exit=2" \
  "$(cd "$work/here" && lib 'path_flag --out nope/f' | paste -sd'|' -)"

# ── bounded under a terminal ──────────────────────────────────────────────────────────────────────
# A pseudo-terminal from script(1) (BSD's/macOS's and util-linux's take their arguments differently): the line runs in
# its foreground with it on stdin, as a tool run by hand does. The feed holds the terminal's input open until the line
# is done or its shell gone (writing a Ctrl-C into it once the line says it is ready, with `ctrl-c`), all bounded --
# generously (shell-spec.sh's SPEC_HANG_BOUND), as a bound that only stops a hang, never one a slow machine reaches. Run
# as a background job (run-script-tests runs each test so) the test ignores SIGINT, which bash cannot undo: perl does.
# shellcheck disable=SC2016 # the lines are run by the shell in the terminal
in_terminal() { # [ctrl-c] <shell line> -> what the terminal showed, \r dropped
  local interrupt=""; [ "$1" = ctrl-c ] && { interrupt=1; shift; }
  rm -f "$work/ready" "$work/done" "$work/shell"
  local line='echo $$ > "$2/shell"; . "$1"; TOOL_NAME=demo; '"$1"'; touch "$2/done"' run
  if grep -q util-linux <<<"$(script -V 2>&1)"; then
    run=(script -qec "$(printf '%q ' bash -c "$line" _ "$REPO_ROOT/scripts/tool-conventions.sh" "$work")" /dev/null)
  else
    run=(script -q /dev/null bash -c "$line" _ "$REPO_ROOT/scripts/tool-conventions.sh" "$work")
  fi
  { for _ in $(seq 1 $((SPEC_HANG_BOUND * 10))); do
      [ -e "$work/done" ] && break
      [ -s "$work/shell" ] && ! kill -0 "$(cat "$work/shell")" 2>/dev/null && break
      if [ -n "$interrupt" ] && [ -e "$work/ready" ]; then printf '\003'; interrupt=""; fi
      sleep 0.1
    done; } | bounded "$SPEC_HANG_BOUND" perl -e '$SIG{INT} = $SIG{QUIT} = "DEFAULT"; exec @ARGV or exit 127' "${run[@]}" 2>&1 | tr -d '\r'
}
if command -v script > /dev/null; then
  check "in a terminal, a command touching it is not stopped (SIGTTOU) until the deadline" "tty-ok rc=0" \
    "$(in_terminal 'bounded 10 sh -c "stty -echo; stty echo; echo tty-ok"; echo rc=$?' | grep -E 'tty-ok|rc=' | paste -sd' ' -)"
  check "...the terminal the caller's again once the command ends, at the deadline too" "rc=124 back" \
    "$(in_terminal 'bounded 1 sleep 20; echo rc=$?; stty -echo; stty echo; echo back' | grep -E 'rc=|back' | paste -sd' ' -)"
  # Nothing in the line ends on its own (its bound outlasts the terminal's): the caller not carrying on and the grandchild
  # gone prove the Ctrl-C did it, never elapsed wall time (shell-spec.sh's HUNG STUBS)
  check "...a Ctrl-C there ends the command's whole group and its caller" "gone" \
    "$(in_terminal ctrl-c 'bounded '$((SPEC_HANG_BOUND * 2))' sh -c "sleep 86400 & echo \$! > $2/grandchild; touch $2/ready; sleep 86400"; echo carried-on' > "$work/terminal"
       left=$(spec_still_running "$work/grandchild") # killed whichever way the case goes: a failure strands nothing
       if grep -q carried-on "$work/terminal"; then echo "the caller carried on"
       elif [ "$left" != 0 ]; then echo alive
       else echo gone; fi)"
else
  echo "  - no script(1): bounded under a terminal not checked"
fi

# ── gh_download ───────────────────────────────────────────────────────────────────────────────────
# gh: answers each call from the next line of $STUB/answers ("ok", "absent", "gone", "expired", "partial", "egress",
# "busy", "fail", "hang"), recording it
# `gh api rate_limit` (gh_download's reset lookup) answers $STUB/reset's epoch, or fails when there is none: not a
# download, so not an answer or a call.
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
if [ "$1" = api ]; then [ -s "$STUB/reset" ] && cat "$STUB/reset" && exit 0; echo "HTTP 403" >&2; exit 1; fi
n=$(( $(cat "$STUB/n" 2>/dev/null || echo 0) + 1 )); echo "$n" > "$STUB/n"
echo "gh $*" >> "$STUB/calls"
dir=""; while [ $# -gt 0 ]; do [ "$1" = --dir ] && dir=$2; shift; done
case "$(sed -n "${n}p" "$STUB/answers")" in
  ok) [ -z "$dir" ] || { mkdir -p "$dir"; echo payload > "$dir/got"; }; exit 0 ;;
  absent) echo "no assets match the file pattern" >&2; exit 1 ;;
  gone) echo "no valid artifacts found to download" >&2; exit 1 ;;
  expired) echo "HTTP 410: Gone (https://api.github.com/repos/o/r/actions/artifacts/9/zip)" >&2; exit 1 ;;
  partial) [ -z "$dir" ] || { mkdir -p "$dir"; echo half > "$dir/got"; }; echo "HTTP 502: Bad Gateway" >&2; exit 1 ;;
  # what Actions' artifact storage (Azure) answered run 38763455103 while it throttled a shared storage account
  egress) echo "error downloading a: HTTP 503: 503 Egress is over the account limit. (https://productionresultssa17.blob.core.windows.net/x.zip)" >&2; exit 1 ;;
  busy) echo "HTTP 429: API rate limit exceeded" >&2; exit 1 ;;
  # what every uk-2 attempt of run 41228549464 met while two convergence runs spent the repository's token budget
  spent) echo "HTTP 403: API rate limit exceeded for installation. If you reach out to GitHub Support for help, please include the request ID 2C01:C5FB6" >&2; exit 1 ;;
  hang) sleep 30 ;;
  stdout) echo "the asset, on stdout"; echo "a warning" >&2; exit 0 ;;
  stdout-absent) echo "no assets match, said on stdout"; echo "HTTP 403: Resource not accessible by integration" >&2; exit 1 ;;
  *) echo "HTTP 403: Resource not accessible by integration" >&2; exit 1 ;;
esac
STUB
chmod +x "$work/bin/gh"
# Every call bounded generously (SPEC_HANG_BOUND), as the in_terminal cases are: a bound a loaded runner reaches turns a
# transient answer into a timed-out, non-transient one, ending its retries early (Main #1849: a 503 series stopped at
# the 4th call, "1 4" for "1 10"). Only the hung-gh case sets a tight one (DOWNLOAD_BOUND).
download() { # <answers, space-separated> <gh download args...> -> "<status> <calls>"
  rm -f "$work/n" "$work/calls"
  # shellcheck disable=SC2086 # one answer a word
  printf '%s\n' $1 > "$work/answers"; shift
  local out; out=$(lib "GH_DOWNLOAD_BACKOFF=0 gh_download ${DOWNLOAD_BOUND:-$SPEC_HANG_BOUND} $*" | tail -1)
  echo "${out#exit=} $( { cat "$work/calls" 2>/dev/null || true; } | wc -l | tr -d ' ')"
}
check "downloaded at once: 0, one call" "0 1" "$(download ok release download t --pattern a --dir "$work/d1")"
check "a failed read is asked again (GH_DOWNLOAD_ATTEMPTS, 3): 0 on the second" "0 2" "$(download "fail ok" release download t --pattern a --dir "$work/d2")"
check "...and fails (1) once every attempt failed" "1 3" "$(download "fail fail fail ok" run download 1 --name a --dir "$work/d3")"
check "nothing there (gh's 'no assets match') is absent (3), not asked again" "3 1" "$(download "absent ok" release download t --pattern a --dir "$work/d4")"
check "...an expired artifact likewise" "3 1" "$(download "gone ok" run download 1 --name a --dir "$work/d5")"
check "...a 410 (an expired artifact's zip) likewise" "3 1" "$(download "expired ok" run download 1 --name a --dir "$work/d8")"
check "a throttled storage (503 Egress is over the account limit) is asked again past the 3 attempts" "0 6" \
  "$(download "egress egress egress egress egress ok" run download 1 --name a --dir "$work/d9")"
check "...as is a 429 rate limit" "0 4" "$(download "busy busy busy ok" release download t --pattern a --dir "$work/d10")"
check "...but not forever: GH_DOWNLOAD_TRANSIENT_ATTEMPTS (10) and it fails (1)" "1 10" \
  "$(download "egress egress egress egress egress egress egress egress egress egress ok" run download 1 --name a --dir "$work/d11")"
rm -f "$work/n" "$work/calls"; echo egress > "$work/answers"
check "gh's own error is printed, not a guess at it" "yes" \
  "$(lib "GH_DOWNLOAD_BACKOFF=0 GH_DOWNLOAD_TRANSIENT_ATTEMPTS=1 gh_download $SPEC_HANG_BOUND run download 1 --name a --dir $work/d12" | grep -c 'HTTP 503: 503 Egress is over the account limit' | sed 's/^[1-9][0-9]*$/yes/')"
# The `ok` after the hang runs under the same 5 s bound, so a spare `ok` answers one a loaded runner times out too.
check "a hung gh is ended by the bound (5 s here) and asked again" "0 asked again" \
  "$(DOWNLOAD_BOUND=5 download "hang ok ok" release download t --pattern a --dir "$work/d6" | awk '{ print $1, ($2 >= 2 ? "asked again" : "asked " $2 " time(s)") }')"
rm -f "$work/n" "$work/calls"; printf 'stdout\n' > "$work/answers"
lib "gh_download $SPEC_HANG_BOUND release download t --pattern a --output - 2> \"\$STUB/stderr\"" > "$work/stdout"
check "gh's stdout passes through (--output -), never read as its messages" "the asset, on stdout|exit=0" "$(paste -sd'|' - < "$work/stdout")"
check "...its stderr said on stderr" "a warning" "$(cat "$work/stderr")"
check "only gh's stderr says absent: an 'absent' phrase on stdout is a failed read, asked again" "1 3" \
  "$(download "stdout-absent stdout-absent stdout-absent" release download t --pattern a --output -)"
mkdir -p "$work/d7"; echo stale > "$work/d7/got"
download "partial ok" run download 1 --name a --dir "$work/d7" > /dev/null
check "a run download cut short is retried into an emptied --dir (gh will not overwrite what it left)" "payload" "$(cat "$work/d7/got")"
check "...whose files of before the retry are gone" "1" "$(find "$work/d7" -type f | wc -l | tr -d ' ')"

# ── a spent API rate limit: wait for its reset, not blind ─────────────────────────────────────────
date +%s > "$work/reset"
check "a spent rate limit waits for its reset (gh api rate_limit), then asks once more" "0 2" \
  "$(GH_RATE_LIMIT_SLACK=0 GH_RATE_LIMIT_JITTER=0 download "spent ok" release download t --pattern a --dir "$work/r1")"
rm -f "$work/n" "$work/calls"; printf 'spent\nok\n' > "$work/answers"
check "...and says when it resets" "yes" "$(lib "GH_RATE_LIMIT_SLACK=0 GH_RATE_LIMIT_JITTER=0 gh_download $SPEC_HANG_BOUND release download t --pattern a --dir $work/r2" | grep -c 'rate limit is spent; it resets in 0 s' | sed 's/^1$/yes/')"
echo $(( $(date +%s) + 7200 )) > "$work/reset"
rm -f "$work/n" "$work/calls"; printf 'spent\nok\n' > "$work/answers"
check "...a reset further than GH_RATE_LIMIT_MAX_WAIT backs off as any throttle" "yes" \
  "$(lib "GH_DOWNLOAD_BACKOFF=0 gh_download $SPEC_HANG_BOUND release download t --pattern a --dir $work/r3" | grep -c 'attempt 1 of 10 failed' | sed 's/^1$/yes/')"
: > "$work/reset"
check "...as does a limit it cannot read" "0 2" "$(download "spent ok" release download t --pattern a --dir "$work/r4")"

# ── the public asset URL: no API request ──────────────────────────────────────────────────────────
# curl: answers from $STUB/curl_answers' next line -- "200" (the asset, to -o), "404", "stall" (curl's 28 after a
# partial 200: under --speed-limit for --speed-time) -- and records the URL it was asked
cat > "$work/bin/curl" <<'STUB'
#!/usr/bin/env bash
n=$(( $(cat "$STUB/cn" 2>/dev/null || echo 0) + 1 )); echo "$n" > "$STUB/cn"
out=""; url=""; while [ $# -gt 0 ]; do case "$1" in -o) out=$2; shift ;; https://*) url=$1 ;; esac; shift; done
echo "$url" >> "$STUB/curls"
case "$(sed -n "${n}p" "$STUB/curl_answers")" in
  200) echo public > "$out"; printf 200 ;;
  404) echo "Not Found" > "$out"; printf 404 ;;
  stall) echo half > "$out"; printf 200; exit 28 ;;
  *) printf 000; exit 6 ;;
esac
STUB
chmod +x "$work/bin/curl"
public() { # <curl answers> <gh answers> <gh download args...> -> "<status> <gh calls> <curl calls>"
  rm -f "$work/cn" "$work/curls"
  # shellcheck disable=SC2086 # one answer a word
  printf '%s\n' $1 > "$work/curl_answers"; shift
  local out; out=$(download "$@"); echo "$out $( { cat "$work/curls" 2>/dev/null || true; } | wc -l | tr -d ' ')"
}
check "an exact asset name is fetched from the public release URL: no gh, no API request" "0 0 1" \
  "$(GH_DOWNLOAD_PUBLIC=1 GH_REPO=o/r public 200 ok release download t --pattern a.tar.zst --dir "$work/p1")"
check "...github.com's browser URL for the asset" "https://github.com/o/r/releases/download/t/a.tar.zst" "$(cat "$work/curls")"
check "...into --dir under its own name" "public" "$(cat "$work/p1/a.tar.zst")"
check "...-R names the repository" "https://github.com/x/y/releases/download/t/a" \
  "$(GH_DOWNLOAD_PUBLIC=1 public 200 ok release download t -R x/y --pattern a --dir "$work/p2" > /dev/null; cat "$work/curls")"
check "...--output - on stdout" "public|exit=0" \
  "$(rm -f "$work/cn"; echo 200 > "$work/curl_answers"; lib "GH_DOWNLOAD_PUBLIC=1 GH_REPO=o/r gh_download $SPEC_HANG_BOUND release download t --pattern a --output -" | paste -sd'|' -)"
check "a 404 there (a private repository, a missing asset) leaves the verdict to gh: absent stays absent" "3 1 1" \
  "$(GH_DOWNLOAD_PUBLIC=1 GH_REPO=o/r public 404 absent release download t --pattern a --dir "$work/p3")"
check "a STALLED transfer (under GH_DOWNLOAD_STALL_BYTES a second) falls back to gh at once" "0 1 1" \
  "$(GH_DOWNLOAD_PUBLIC=1 GH_REPO=o/r public stall ok release download t --pattern a --dir "$work/p4")"
check "...leaving no partial file behind" "payload" "$(cat "$work/p4"/* | paste -sd' ' -)"
check "a glob is gh's (only the API can match it)" "0 1 0" \
  "$(GH_DOWNLOAD_PUBLIC=1 GH_REPO=o/r public 200 ok release download t --pattern 'a.tar.*' --dir "$work/p5")"
check "...as are several patterns" "0 1 0" \
  "$(GH_DOWNLOAD_PUBLIC=1 GH_REPO=o/r public 200 ok release download t --pattern a --pattern b --dir "$work/p6")"
check "...and a run's artifact" "0 1 0" "$(GH_DOWNLOAD_PUBLIC=1 GH_REPO=o/r public 200 ok run download 1 --name a --dir "$work/p7")"
check "by default on an Actions runner (GITHUB_REPOSITORY)" "0 0 1" \
  "$(GH_DOWNLOAD_PUBLIC='' GITHUB_ACTIONS=true GITHUB_REPOSITORY=o/r public 200 ok release download t --pattern a --dir "$work/p8")"
check "...nor off a runner" "0 1 0" \
  "$(GH_DOWNLOAD_PUBLIC='' GITHUB_ACTIONS='' GITHUB_REPOSITORY=o/r public 200 ok release download t --pattern a --dir "$work/p10")"

# ── a stall says what it was stuck on ─────────────────────────────────────────────────────────────
# shellcheck disable=SC2016 # expanded by the bounded command's shell
inherited=$(BOUNDED_ON_TIMEOUT=/bin/echo bounded "$SPEC_HANG_BOUND" sh -c 'echo "${BOUNDED_ON_TIMEOUT:-unset}"'); rc=$?
check "BOUNDED_ON_TIMEOUT is that bound's alone: the command does not inherit it" "unset rc=0" "$inherited rc=$rc"
BOUNDED_ON_TIMEOUT="$REPO_ROOT/scripts/stall-report.sh" bounded 1 sh -c 'exec sleep 30' 2> "$work/report"; rc=$?
check "at the deadline it reports the group, still alive, before ending it (124)" "rc=124 report sleep" \
  "rc=$rc $(grep -c '── stall report: process group' "$work/report" | sed 's/^1$/report/') $(grep -q 'sleep 30' "$work/report" && echo sleep)"
stalled_exit=$(CASE_BOUND=1 lib 'sleep 30' 2> "$work/stalled" | tail -1)
check "a stalled lib case names itself (CASE_BOUND, 1 s here) and carries the report" "named report exit=124" \
  "$(grep -q 'lib: stalled past 1s: sleep 30' "$work/stalled" && printf named) $(grep -q 'stall report: process group' "$work/stalled" && printf report) $stalled_exit"

# ── TOOL_NAME is the tool's own, never its children's ───────────────────────────────────────────────
# Exported, it renames every tool the script starts: land-precheck's export made identity-ci-run die as
# "land-precheck: could not push ..." (2026-11-07, its self-test red in every precheck and nowhere else).
check "no script exports TOOL_NAME (a child tool would die under the parent's name)" "" \
  "$(cd "$REPO_ROOT" && grep -rlE '^[[:space:]]*export[[:space:]]+TOOL_NAME' scripts .github/scripts infra/bin 2>/dev/null)"

spec_summary
