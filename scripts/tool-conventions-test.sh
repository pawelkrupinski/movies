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

# shellcheck disable=SC2016 # the lines are run by the library's shell, expanded there
lib() { # <shell line> -> its stdout and stderr, then "exit=<status>"; run with the library sourced, as a tool named demo
  STUB="$work" PATH="$work/bin:$PATH" bounded 20 bash -c '. "$1"; TOOL_NAME=demo; eval "$2"' _ "$REPO_ROOT/scripts/tool-conventions.sh" "$1" 2>&1
  echo "exit=$?"
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
check "a flag without its value is a usage error" "demo: --country needs a country code (one of pl,de,uk,us,es)|demo: see demo --help|exit=2" \
  "$(countries --country)"

# ── bounded under a terminal ──────────────────────────────────────────────────────────────────────
# A pseudo-terminal from script(1) (BSD's/macOS's and util-linux's take their arguments differently): the line runs in
# its foreground with it on stdin, as a tool run by hand does. The feed holds the terminal's input open until the line
# is done or its shell gone (writing a Ctrl-C into it once the line says it is ready, with `ctrl-c`), all bounded.
# shellcheck disable=SC2016 # the lines are run by the shell in the terminal
in_terminal() { # [ctrl-c] <shell line> -> what the terminal showed, \r dropped
  local interrupt=""; [ "$1" = ctrl-c ] && { interrupt=1; shift; }
  rm -f "$work/ready" "$work/done" "$work/shell"
  local line='echo $$ > "$2/shell"; . "$1"; TOOL_NAME=demo; '"$1"'; touch "$2/done"' run
  if script -V 2>&1 | grep -q util-linux; then
    run=(script -qec "$(printf '%q ' bash -c "$line" _ "$REPO_ROOT/scripts/tool-conventions.sh" "$work")" /dev/null)
  else
    run=(script -q /dev/null bash -c "$line" _ "$REPO_ROOT/scripts/tool-conventions.sh" "$work")
  fi
  { for _ in $(seq 1 200); do
      [ -e "$work/done" ] && break
      [ -s "$work/shell" ] && ! kill -0 "$(cat "$work/shell")" 2>/dev/null && break
      if [ -n "$interrupt" ] && [ -e "$work/ready" ]; then printf '\003'; interrupt=""; fi
      sleep 0.1
    done; } | bounded 30 "${run[@]}" 2>&1 | tr -d '\r'
}
if command -v script > /dev/null; then
  check "in a terminal, a command touching it is not stopped (SIGTTOU) until the deadline" "tty-ok rc=0" \
    "$(in_terminal 'bounded 10 sh -c "stty -echo; stty echo; echo tty-ok"; echo rc=$?' | grep -E 'tty-ok|rc=' | paste -sd' ' -)"
  check "...the terminal the caller's again once the command ends, at the deadline too" "rc=124 back" \
    "$(in_terminal 'bounded 1 sleep 20; echo rc=$?; stty -echo; stty echo; echo back' | grep -E 'rc=|back' | paste -sd' ' -)"
  check "...a Ctrl-C there ends the command's whole group and its caller" "gone" \
    "$(in_terminal ctrl-c 'bounded 30 sh -c "sleep 30 & echo \$! > $2/grandchild; touch $2/ready; sleep 30"; echo carried-on' > "$work/terminal"
       if grep -q carried-on "$work/terminal"; then echo "the caller carried on"
       elif kill -0 "$(cat "$work/grandchild")" 2>/dev/null; then echo alive; kill "$(cat "$work/grandchild")"
       else echo gone; fi)"
else
  echo "  - no script(1): bounded under a terminal not checked"
fi

# ── gh_download ───────────────────────────────────────────────────────────────────────────────────
# gh: answers each call from the next line of $STUB/answers ("ok", "absent", "fail", "hang"), recording it
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
n=$(( $(cat "$STUB/n" 2>/dev/null || echo 0) + 1 )); echo "$n" > "$STUB/n"
echo "gh $*" >> "$STUB/calls"
dir=""; while [ $# -gt 0 ]; do [ "$1" = --dir ] && dir=$2; shift; done
case "$(sed -n "${n}p" "$STUB/answers")" in
  ok) [ -z "$dir" ] || { mkdir -p "$dir"; echo payload > "$dir/got"; }; exit 0 ;;
  absent) echo "no assets match the file pattern" >&2; exit 1 ;;
  gone) echo "no valid artifacts found to download" >&2; exit 1 ;;
  partial) [ -z "$dir" ] || { mkdir -p "$dir"; echo half > "$dir/got"; }; echo "HTTP 502: Bad Gateway" >&2; exit 1 ;;
  hang) sleep 30 ;;
  *) echo "HTTP 500: Internal Server Error" >&2; exit 1 ;;
esac
STUB
chmod +x "$work/bin/gh"
download() { # <answers, space-separated> <gh download args...> -> "<status> <calls>"
  rm -f "$work/n" "$work/calls"
  # shellcheck disable=SC2086 # one answer a word
  printf '%s\n' $1 > "$work/answers"; shift
  local out; out=$(lib "GH_DOWNLOAD_BACKOFF=0 gh_download 2 $*" | tail -1)
  echo "${out#exit=} $(wc -l < "$work/calls" | tr -d ' ')"
}
check "downloaded at once: 0, one call" "0 1" "$(download ok release download t --pattern a --dir "$work/d1")"
check "a failed read is asked again (GH_DOWNLOAD_ATTEMPTS, 3): 0 on the second" "0 2" "$(download "fail ok" release download t --pattern a --dir "$work/d2")"
check "...and fails (1) once every attempt failed" "1 3" "$(download "fail fail fail ok" run download 1 --name a --dir "$work/d3")"
check "nothing there (gh's 'no assets match') is absent (3), not asked again" "3 1" "$(download "absent ok" release download t --pattern a --dir "$work/d4")"
check "...an expired artifact likewise" "3 1" "$(download "gone ok" run download 1 --name a --dir "$work/d5")"
check "a hung gh is ended by the bound (2 s here) and asked again" "0 2" "$(download "hang ok" release download t --pattern a --dir "$work/d6")"
mkdir -p "$work/d7"; echo stale > "$work/d7/got"
download "partial ok" run download 1 --name a --dir "$work/d7" > /dev/null
check "a run download cut short is retried into an emptied --dir (gh will not overwrite what it left)" "payload" "$(cat "$work/d7/got")"
check "...whose files of before the retry are gone" "1" "$(find "$work/d7" -type f | wc -l | tr -d ' ')"

spec_summary
