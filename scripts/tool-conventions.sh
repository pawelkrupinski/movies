#!/usr/bin/env bash
# Purpose: What every Bash tool in this repository shares, one copy each
# What every Bash tool in this repository shares, one copy each: a bounded command that works on stock macOS, the usage
# text read from a tool's own header, how a tool fails, its country flag, a bounded and retried gh download, the
# country codes, and the shared identity cache's path. Sourcing it defines
# functions and constants only (and exports KINOWO_IDENTITY_CACHE, so a Python tool started from a Bash one reads the
# same cache). bash 3.2 compatible: no mapfile, no associative arrays, no ${x,,}.
# A SHIM over scripts/lib (since 2026-11-27): bounded is tool_bounded.py, the country flag tool_countries.py, the gh
# download gh_download.py; what stays here must run in the calling shell (an exit, a variable it sets). A tree that
# copies this file copies the files of tool-conventions-files.txt beside it.
#
# THE CONVENTIONS
#   set -euo pipefail   a tool whose every step must succeed for its answer to mean anything: the first failure ends it.
#   set -uo pipefail    (no -e) a spec, which runs every check and tallies them (scripts/shell-spec.sh), and a CI step or
#                       tool that decides what each failure means — 2 "no verdict" against 1 "failed", a retry, a step
#                       that carries on with less. Such a script checks every command whose failure matters itself
#                       (`|| die`, `|| return 1`, `rc=$?`); a failure it leaves unchecked is one carrying on past is right for.
#   Every network call (gh, curl, git fetch/push/ls-remote) runs under `bounded <seconds>`: a hung API never holds a
#   tool, a push or a CI row forever.
#   A temporary file or directory is removed by a trap set where it is made: Ctrl-C does not leak it.
#   A write into a cache another agent reads goes through identity_locked/identity_put (scripts/identity-tools.sh).
#   -h/--help prints the tool's header comment: usage_from_header.
#
#   bounded <seconds> <command...>  -> the command, ended once it outlasts <seconds> (exit 124), else its own status.
#        timeout(1)'s contract without timeout(1), which stock macOS lacks: the command runs in a process group of its
#        own, ended whole (TERM, then KILL two seconds later: an sbt-forked JVM too); and unlike GNU timeout, which
#        leaves the terminal's group, a Ctrl-C reaches it — passed on to the group, then to the caller as the signal.
#        Run in a terminal's foreground with it on stdin, the command's group holds the foreground while it runs, so
#        one touching the terminal (stty, a prompt) is not stopped, and a Ctrl-C there reaches the caller too.
#        BOUNDED_ON_TIMEOUT=<executable>: run at the deadline with the group's id, BEFORE the group is ended, to say
#        what it was stuck on (scripts/stall-report.sh). This call's alone: the command does not inherit it.
#   usage_from_header [file]        -> the header comment of <file> (default the running script, $0): from line 2 to the
#        first `set ` line, the `# ` stripped, a leading blank line dropped
#   KINOWO_COUNTRIES                -> the country codes every tool loops over, in one order (kinowo_countries_csv: "pl,de,…")
#   KINOWO_IDENTITY_CACHE           -> the cache every identity tool shares (default ~/.cache/kinowo-identity)
#   KINOWO_GH_REPO                  -> the GitHub repository every gh call names with -R ($KINOWO_REPO, default
#        pawelkrupinski/movies: a test points it elsewhere)
#   KINOWO_GH                       -> what every gh call runs (default gh; a spec points it at a stub)
#   die / usage_error <message...>  -> exit 1 (DIE_STATUS) for a runtime failure, 2 for a caller's mistake (below)
#   country_flag / selected_countries_csv -> `--country cc[,cc]` and `--countries a,b`, one parser (below)
#   flag_value "$@"                 -> a usage_error when the flag in $1 has no value after it (below)
#   path_flag "$@"                  -> the flag's value made absolute, a usage_error when its directory is not there (below)
#   gh_download <seconds> <gh download args...> -> 0 got it, 3 absent, 1 failed: bounded and retried; a public release
#        asset from its public URL, no API request (below)
#   local_mongo_port / local_mongo_uri [database] -> the local, password-free Mongo the prod mirror, the it/ suites
#        and the identity tools share: 127.0.0.1:$LOCAL_MIRROR_PORT (default 28017), a direct connection

KINOWO_COUNTRIES=(pl de uk us es)
kinowo_countries_csv() { local IFS=,; echo "${KINOWO_COUNTRIES[*]}"; }

KINOWO_IDENTITY_CACHE=${KINOWO_IDENTITY_CACHE:-$HOME/.cache/kinowo-identity}
export KINOWO_IDENTITY_CACHE
# shellcheck disable=SC2034 # read by the tools that source this
KINOWO_GH_REPO=${KINOWO_REPO:-pawelkrupinski/movies}
# The `gh` every tool runs: gh, unless $KINOWO_GH names another.
KINOWO_GH=${KINOWO_GH:-gh}

local_mongo_port() { echo "${LOCAL_MIRROR_PORT:-28017}"; }
local_mongo_uri() { echo "mongodb://127.0.0.1:$(local_mongo_port)/${1:-}?directConnection=true"; }

# builtins only: a bare PATH (a spec's, stock macOS's) still finds it
case "${BASH_SOURCE[0]}" in */*) TOOL_CONVENTIONS_LIB=${BASH_SOURCE[0]%/*} ;; *) TOOL_CONVENTIONS_LIB=. ;; esac
TOOL_CONVENTIONS_LIB="$(cd "$TOOL_CONVENTIONS_LIB" && pwd)/lib"

bounded() {
  case "${1:-}" in
    ''|*[!0-9]*|0) echo "bounded: <seconds> must be a positive whole number, not '${1:-}'" >&2; return 64 ;;
  esac
  python3 -I -B "$TOOL_CONVENTIONS_LIB/tool_bounded.py" "$@"
}

usage_from_header() { sed -n '2,/^set /p' "${1:-$0}" | sed -e '$d' -e 's/^# \{0,1\}//' -e 's/^#$//' | sed -e '1{/^$/d;}'; }

# ── Failing: a runtime failure, or the caller's mistake ─────────────────────────────────────────────
#   die <message...>          -> "<tool>: <message>" on stderr, then exit $DIE_STATUS (default 1): the tool could not do
#        its work. A tool whose documented exit contract gives "no verdict" a status of its own (fail-before's,
#        identity-measure's 2, beside their 1 for a verdict of "no") sets DIE_STATUS=2 once,
#        next to that contract.
#   usage_error <message...>  -> the same and a pointer to --help, then exit 2 whatever DIE_STATUS says: asked wrongly.
#   TOOL_NAME (default the running script's file name) names the tool in both.
die() { echo "${TOOL_NAME:-$(basename "$0")}: $*" >&2; exit "${DIE_STATUS:-1}"; }
usage_error() {
  local tool=${TOOL_NAME:-$(basename "$0")}
  echo "$tool: $*" >&2
  echo "$tool: see $tool --help" >&2
  exit 2
}

# ── The country flag ────────────────────────────────────────────────────────────────────────────────
# One flag, both spellings the tools grew: `--country cc[,cc]` (repeatable) and `--countries a,b`, each also as
# `--flag=value`. In a tool's argument loop:
#   --country|--countries|--country=*|--countries=*) country_flag "$@"; shift "$COUNTRY_FLAG_ARGS" ;;
# and after it `selected_countries_csv` (every country when none was named). Codes are lower-cased, a repeat dropped,
# and one KINOWO_COUNTRIES lacks — or a flag without its value, or one naming no code (`,`) — is a usage_error. COUNTRY_FLAG_ARGS is always 1 or 2.
SELECTED_COUNTRIES=()
# shellcheck disable=SC2034 # COUNTRY_FLAG_ARGS is the calling loop's
country_flag() {
  local flag=$1 value codes
  COUNTRY_FLAG_ARGS=1
  case "$flag" in
    --country=*|--countries=*) value=${flag#*=}; flag=${flag%%=*} ;;
    *) value=${2:-}; [ $# -lt 2 ] || COUNTRY_FLAG_ARGS=2 ;;
  esac
  codes=$(python3 -I -B "$TOOL_CONVENTIONS_LIB/tool_countries.py" "$flag" "$value" ${SELECTED_COUNTRIES[@]+"${SELECTED_COUNTRIES[@]}"}) ||
    usage_error "$codes"
  read -r -a SELECTED_COUNTRIES <<< "$codes"
}
selected_countries_csv() {
  if [ ${#SELECTED_COUNTRIES[@]} -eq 0 ]; then kinowo_countries_csv; else local IFS=,; echo "${SELECTED_COUNTRIES[*]}"; fi
}

# ── A flag's value ──────────────────────────────────────────────────────────────────────────────────
# In a tool's argument loop, first in an arm that reads a value:   --out) flag_value "$@"; out=$2; shift 2 ;;
# A flag given last has no $2: without this `shift 2` fails without shifting, and a loop without set -e reads the same
# flag again forever (identity-measure-local --base did); under set -u a bare $2 dies as "unbound variable" instead.
flag_value() { [ $# -ge 2 ] || usage_error "$1 needs a value"; }
# A path a tool hands to a JVM or a worktree that runs elsewhere, made absolute while the caller's directory is the
# current one:   --out) keep=$(path_flag "$@"); shift 2 ;;
# A directory that is not there is the caller's mistake: `$(cd "$(dirname x)" && pwd)/$(basename x)` turned it into
# /<name> silently -- the assignment's status is the last substitution's -- and a takes file was then written at /.
path_flag() {
  local dir
  flag_value "$@"
  dir=$(cd "$(dirname "$2")" 2>/dev/null && pwd) || usage_error "$1 $2: no directory $(dirname "$2")"
  echo "$dir/$(basename "$2")"
}

# ── Downloading from GitHub ─────────────────────────────────────────────────────────────────────────
#   gh_download <seconds> <run|release> download <args...> -> 0 downloaded, 3 absent, 1 failed: bounded and retried
#   gh_release_public <seconds> release download <args...>  -> one asset from its public URL (4: not one it serves)
#   gh_rate_limit_wait                                      -> the seconds until a spent API rate limit resets
#   prod_latest_run <cc>                                    -> the freshest complete nightly recording's run id
#   All in scripts/lib/gh_download.py, whose header holds the contract (the retries, absent against failed, the
#   public URL, the rate limit) and every knob.
gh_download() { python3 -I -B "$TOOL_CONVENTIONS_LIB/gh_download.py" download "$@"; }
gh_rate_limit_wait() { python3 -I -B "$TOOL_CONVENTIONS_LIB/gh_download.py" rate-limit-wait; }
gh_release_public() { python3 -I -B "$TOOL_CONVENTIONS_LIB/gh_download.py" public "$@"; }
prod_latest_run() { python3 -I -B "$TOOL_CONVENTIONS_LIB/gh_download.py" prod-latest "$1"; }
