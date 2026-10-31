#!/usr/bin/env bash
# What every Bash tool in this repository shares, one copy each: a bounded command that works on stock macOS, the usage
# text read from a tool's own header, how a tool fails, its country flag, a bounded and retried gh download, the
# country codes, and the shared identity cache's path. Sourcing it defines
# functions and constants only (and exports KINOWO_IDENTITY_CACHE, so a Python tool started from a Bash one reads the
# same cache). bash 3.2 compatible: no mapfile, no associative arrays, no ${x,,}.
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

bounded() {
  perl -e '
    use POSIX ":sys_wait_h";
    my $seconds = shift;
    my $on_timeout = delete $ENV{BOUNDED_ON_TIMEOUT}; # for this bound alone, never one the command sets itself
    # The terminal: when bounded runs in its foreground with it on stdin, the command'"'"'s group is handed the foreground
    # while it runs, and it is taken back after -- a group of its own in the background is stopped (SIGTTOU/SIGTTIN)
    # the moment it touches the terminal (stty, a password prompt, a JLine sbt client), until the deadline.
    my $home = getpgrp();
    my $tty = (-t STDIN && POSIX::tcgetpgrp(0) == $home) ? 0 : undef;
    $SIG{TTOU} = "IGNORE" if defined $tty; # a group out of the foreground may then move it, as a shell does
    my $pid = fork() // die "fork: $!";
    if ($pid == 0) {
      setpgrp(0, 0);
      if (defined $tty) { POSIX::tcsetpgrp($tty, $$); $SIG{TTOU} = "DEFAULT"; }
      exec @ARGV or exit 127;
    }
    if (defined $tty) { setpgrp($pid, $pid); POSIX::tcsetpgrp($tty, $pid); } # whichever of the two runs first
    my $take_back = sub { POSIX::tcsetpgrp($tty, $home) if defined $tty; };
    # the whole group: TERM, a moment to go, then KILL whatever is left of it (a forked JVM included)
    my $end_group = sub {
      kill "TERM", -$pid;
      for (1 .. 20) { waitpid($pid, WNOHANG); last unless kill 0, -$pid; select(undef, undef, undef, 0.1); }
      kill "KILL", -$pid;
      $take_back->();
    };
    my $stop = sub {
      my ($code, $signal) = @_;
      $end_group->();
      # a signal is passed on as one: the calling shell sees it interrupted, as it would the command itself
      if ($signal) { $SIG{$signal} = "DEFAULT"; kill $signal, $$; }
      exit $code;
    };
    $SIG{ALRM} = sub { system($on_timeout, $pid) if $on_timeout; $stop->(124) };
    $SIG{INT} = sub { $stop->(130, "INT") };
    $SIG{TERM} = sub { $stop->(143, "TERM") };
    alarm $seconds;
    waitpid($pid, 0);
    my $status = $?;
    $take_back->();
    # A Ctrl-C at the terminal reached only the command, the foreground: what it left running (a background job,
    # which ignores it) is ended with it, and the signal passed on to the group bounded was called from, as the
    # terminal would have sent it, so the caller stops as it would had the command run in its group.
    if (defined $tty && grep { ($status & 127) == $_ } (2, 3)) {
      $end_group->();
      $SIG{INT} = $SIG{QUIT} = "DEFAULT";
      kill $status & 127, -$home;
    }
    exit($status & 127 ? 128 + ($status & 127) : $status >> 8);' "$@"
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
  local flag=$1 value cc known codes=() named=0
  COUNTRY_FLAG_ARGS=1
  case "$flag" in
    --country=*|--countries=*) value=${flag#*=}; flag=${flag%%=*} ;;
    *) value=${2:-}; COUNTRY_FLAG_ARGS=2; [ $# -ge 2 ] || COUNTRY_FLAG_ARGS=1 ;;
  esac
  [ -n "$value" ] && [ "${value#-}" = "$value" ] || usage_error "$flag needs a country code (one of $(kinowo_countries_csv))"
  # split on commas and spaces into an array, never through an unquoted expansion: a `*` stays a `*`
  IFS=', ' read -r -a codes <<< "$(printf '%s' "$value" | tr '[:upper:]' '[:lower:]')"
  for cc in ${codes[@]+"${codes[@]}"}; do
    [ -n "$cc" ] || continue
    named=1
    known=""
    case ",$(kinowo_countries_csv)," in *",$cc,"*) known=1 ;; esac
    [ -n "$known" ] || usage_error "unknown country $cc (one of $(kinowo_countries_csv))"
    case " ${SELECTED_COUNTRIES[*]:-} " in *" $cc "*) ;; *) SELECTED_COUNTRIES+=("$cc") ;; esac
  done
  [ "$named" = 1 ] || usage_error "$flag names no country code (one of $(kinowo_countries_csv))"
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
#   gh_download <seconds> <run|release> download <args...> -> 0 downloaded; 3 ABSENT (gh says there is nothing to
#        download: no asset matches, no such or an expired artifact, a 404 or 410); 1 a FAILED read — every attempt
#        failed, each ended once it outlasts <seconds> (a hung gh is a failed attempt, not a wait forever).
#   A failed read is asked again, GH_DOWNLOAD_ATTEMPTS times in all (3), the wait doubling from GH_DOWNLOAD_BACKOFF
#   seconds (5). A THROTTLED one — a 5xx, a 429 or a rate limit, Azure's "Egress is over the account limit" from the
#   artifact storage — is asked up to GH_DOWNLOAD_TRANSIENT_ATTEMPTS times (10): 5, 10, 20, 40, 80, then 120 s
#   (GH_DOWNLOAD_MAX_WAIT) a wait, each plus up to half again of jitter, ~11-16 minutes in all. The storage account
#   behind Actions artifacts is shared by many repositories and throttles in bursts (run 38763455103: one leg's
#   download refused while its 16 sibling jobs' succeeded, the same artifact downloading in 3 s two hours later);
#   jitter keeps a matrix's legs from asking again in step, and past ~15 minutes it is an outage a re-run answers, not
#   a wait worth a runner. An absent one is an answer, never asked again — and never confused with a failed one,
#   which a caller treating "absent" as "carry on with less" must not read as it.
#   A `run download`'s --dir is emptied before each retry (gh refuses to overwrite what a cut-short attempt left), so
#   it must be a directory of the download's own. gh's own messages go to stderr.
#
#   PUBLIC ASSETS, NO API. A `release download <tag> --pattern <one exact name>` is fetched from the release's public
#   URL, https://github.com/<repo>/releases/download/<tag>/<name>, which the CDN serves without the REST API — where gh
#   spends three API requests on each (the release by tag, a GraphQL lookup, the asset). A convergence run downloads
#   ~500 fills that way; two runs side by side drew the repository's GITHUB_TOKEN budget dry (run 41228549464: every
#   uk-2 retry 403 "API rate limit exceeded for installation" for 14 minutes; run 41228572378's de-2 waited out its
#   900 s tree-restore bound inside the same window, its fill retries silenced). Taken when GH_DOWNLOAD_PUBLIC=1, and
#   by default on a github.com Actions runner (GITHUB_ACTIONS, GITHUB_SERVER_URL https://github.com) — never against
#   Forgejo, whose forge-gh answers the same command. The repository is -R's, else GH_REPO's, else GITHUB_REPOSITORY's.
#   A 404 there (a private repository, a missing asset) and any other failure fall back to gh from then on: the API's
#   answer decides absent from failed, as it always did.
#   PROGRESS, NOT A CLOCK: the public fetch ends once it moves under GH_DOWNLOAD_STALL_BYTES (1024) bytes a second for
#   GH_DOWNLOAD_STALL_SECONDS (60) — a stalled connection is a failed attempt within a minute, not one that holds the
#   whole <seconds> bound — and gh, on a fresh connection, takes over.
#   RATE LIMIT: a gh refused with "API rate limit exceeded" waits for that limit's reset (`gh api rate_limit`, which
#   the limit does not count; X-RateLimit-Reset's value) plus a few seconds of jitter, said in the log, when the reset is at
#   most GH_RATE_LIMIT_MAX_WAIT (600) seconds away: one request after the reset instead of blind ones inside the empty
#   window. Further than that, or unreadable, it backs off as any throttle.
gh_download() {
  local seconds=$1 attempt=1 attempts err status dir="" arg prev="" wait public=1
  shift
  for arg in "$@"; do [ "$prev" = --dir ] && dir=$arg; prev=$arg; done
  while :; do
    if [ -n "$public" ]; then
      gh_release_public "$seconds" "$@"
      status=$?
      [ "$status" -eq 0 ] && return 0
      public=""
      # 4: not a download the public URL serves -- gh, quietly, as before
      [ "$status" -eq 4 ] || echo "gh $1 $2: the public asset URL did not serve it (status $status); asking the API" >&2
    fi
    # gh's stderr read into a variable, its stdout left where it was (fd 3, opened outside the substitution, is this
    # function's stdout): no temporary file for a signal to strand
    { err=$(bounded "$seconds" "$KINOWO_GH" "$@" 2>&1 1>&3 3>&-); status=$?; } 3>&1
    [ -z "$err" ] || printf '%s\n' "$err" >&2
    [ "$status" -eq 0 ] && return 0
    if printf '%s' "$err" | grep -Eq 'no assets match|release not found|no artifact matches|no valid artifacts found|HTTP 404|HTTP 410'; then
      return 3
    fi
    attempts=${GH_DOWNLOAD_ATTEMPTS:-3}
    if printf '%s' "$err" | grep -Eiq 'HTTP 5[0-9][0-9]|HTTP 429|rate limit|Egress is over|ServerBusy'; then
      attempts=${GH_DOWNLOAD_TRANSIENT_ATTEMPTS:-10}
    fi
    if [ "$attempt" -ge "$attempts" ]; then
      echo "gh $1 $2: failed after $attempt attempt(s) (exit $status)" >&2
      return 1
    fi
    if printf '%s' "$err" | grep -Eiq 'API rate limit exceeded' && wait=$(gh_rate_limit_wait); then
      echo "gh $1 $2: the API rate limit is spent; it resets in $wait s -- asking again then (attempt $attempt of $attempts)" >&2
    else
      wait=$(( ${GH_DOWNLOAD_BACKOFF:-5} << (attempt - 1) ))
      [ "$wait" -le "${GH_DOWNLOAD_MAX_WAIT:-120}" ] || wait=${GH_DOWNLOAD_MAX_WAIT:-120}
      wait=$(( wait + RANDOM % (wait / 2 + 1) ))
      echo "gh $1 $2: attempt $attempt of $attempts failed (exit $status); asking again in ${wait}s" >&2
    fi
    sleep "$wait"
    if [ "$1" = run ] && [ -n "$dir" ] && [ -d "$dir" ]; then find "$dir" -mindepth 1 -delete; fi
    attempt=$((attempt + 1))
  done
}

# gh_rate_limit_wait -> the seconds until the spent API rate limit (REST core or GraphQL, whichever is at 0) resets,
#   plus GH_RATE_LIMIT_SLACK (5) and up to GH_RATE_LIMIT_JITTER (10) seconds, so a matrix's legs do not ask in step;
#   status 1 when none is spent, it cannot be read, or the reset is over GH_RATE_LIMIT_MAX_WAIT
#   (600) seconds away. `gh api rate_limit` is not counted against the limit it reads.
gh_rate_limit_wait() {
  local reset now wait
  # allow-silenced: an unreadable limit only means the caller backs off blind, as it did before.
  reset=$(bounded 30 "$KINOWO_GH" api rate_limit \
            --jq '[.resources.core, .resources.graphql] | map(select(.remaining == 0) | .reset) | max // empty' 2>/dev/null) || return 1
  [[ "$reset" =~ ^[0-9]+$ ]] || return 1
  now=$(date +%s)
  wait=$(( reset - now ))
  [ "$wait" -ge 0 ] || wait=0
  [ "$wait" -le "${GH_RATE_LIMIT_MAX_WAIT:-600}" ] || return 1
  echo $(( wait + ${GH_RATE_LIMIT_SLACK:-5} + RANDOM % (${GH_RATE_LIMIT_JITTER:-10} + 1) ))
}

# gh_release_public <seconds> <gh release download args...> -> 0 fetched from the public asset URL; 3 that URL said 404;
#   4 not a download it serves (see gh_download: off, not github.com, a glob or several patterns, an option it does not
#   know, a file it would have to overwrite without --clobber); 1 any other failure, said on stderr. KINOWO_CURL
#   overrides curl (a test's stub, a counting wrapper).
gh_release_public() {
  local seconds=$1 tag repo name="" dir=. output="" clobber="" dest tmp code status
  shift
  case "${GH_DOWNLOAD_PUBLIC:-}" in
    1) ;;
    0) return 4 ;;
    *) [ "${GITHUB_ACTIONS:-}" = true ] && [ "${GITHUB_SERVER_URL:-}" = https://github.com ] || return 4 ;;
  esac
  [ "${1:-}" = release ] && [ "${2:-}" = download ] && [ $# -ge 3 ] || return 4
  tag=$3 repo=${GH_REPO:-${GITHUB_REPOSITORY:-}}
  shift 3
  while [ $# -gt 0 ]; do
    case "$1" in
      -R|--repo) [ $# -ge 2 ] || return 4; repo=$2; shift 2 ;;
      -p|--pattern) [ $# -ge 2 ] && [ -z "$name" ] || return 4; name=$2; shift 2 ;;
      -D|--dir) [ $# -ge 2 ] || return 4; dir=$2; shift 2 ;;
      -O|--output) [ $# -ge 2 ] || return 4; output=$2; shift 2 ;;
      --clobber) clobber=1; shift ;;
      *) return 4 ;;
    esac
  done
  # owner/name, and a tag and an asset name of URL-safe characters only: a glob, a host prefix or anything to escape
  # stays gh's
  [[ "$repo" =~ ^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$ ]] || return 4
  [[ "$tag" =~ ^[A-Za-z0-9_.~+-]+$ ]] && [[ "$name" =~ ^[A-Za-z0-9_.~+-]+$ ]] || return 4
  if [ -n "$output" ]; then dest=$output; else dest=$dir/$name; fi
  [ "$dest" = - ] || [ -n "$clobber" ] || [ ! -e "$dest" ] || return 4
  if [ "$dest" = - ]; then tmp=$(mktemp) || return 1; else mkdir -p "$(dirname "$dest")" || return 1; tmp=$dest.part; fi
  code=$(bounded "$seconds" "${KINOWO_CURL:-curl}" -sSL --connect-timeout 20 \
           --speed-limit "${GH_DOWNLOAD_STALL_BYTES:-1024}" --speed-time "${GH_DOWNLOAD_STALL_SECONDS:-60}" \
           -o "$tmp" -w '%{http_code}' "https://github.com/$repo/releases/download/$tag/$name")
  status=$?
  if [ "$status" -eq 0 ] && [ "$code" = 200 ]; then
    if [ "$dest" = - ]; then cat "$tmp"; rm -f "$tmp"; else mv -f "$tmp" "$dest"; fi
    return 0
  fi
  rm -f "$tmp"
  [ "$code" != 404 ] || return 3
  # curl 28: the transfer stalled under the speed limit (or the bound ended it)
  echo "public asset $repo $tag/$name: curl exit $status, HTTP ${code:-none}" >&2
  return 1
}

# prod_latest_run <cc> -> the run id of the freshest COMPLETE nightly recording of that country (`--recording prod-latest`):
# its corpus scraped from prod that night, its enrichment tree filled live over it, as prod-latest-<cc>.txt on the
# convergence-fixtures release names it (.github/scripts/convergence-publish.sh latest). Not the hermetic pair the
# identity measure lands on (573). 1, said, when there is none yet or the release cannot be read.
prod_latest_run() {
  local cc=$1 dir run
  dir=$(mktemp -d)
  gh_download "${KINOWO_IDENTITY_GH_TIMEOUT:-60}" release download "${FIXTURE_RELEASE_TAG:-convergence-fixtures}" \
    -R "$KINOWO_GH_REPO" --pattern "prod-latest-$cc.txt" --dir "$dir" >/dev/null ||
    { rm -rf "$dir"; echo "no prod-latest $cc recording (no prod-latest-$cc.txt on ${FIXTURE_RELEASE_TAG:-convergence-fixtures}, or it could not be read): the nightly \"Record scrape fixtures\" writes it once a $cc leg completes" >&2; return 1; }
  run=$(cut -d' ' -f1 "$dir/prod-latest-$cc.txt"); rm -rf "$dir"
  [[ "$run" =~ ^[0-9]+$ ]] || { echo "prod-latest-$cc.txt names no run" >&2; return 1; }
  echo "$run"
}
