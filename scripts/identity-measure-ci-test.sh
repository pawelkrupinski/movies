#!/usr/bin/env bash
# identity-measure-ci.sh against stub `git` and `gh`: what it reports once the watched run ends, and that no
# wait it makes -- for Main to go idle, for the run -- outlives its deadline or reads a failed read as "done".
# Run: bash scripts/identity-measure-ci-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m identity-measure-ci.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin"
cat > "$work/bin/git" <<'STUB'
#!/usr/bin/env bash
case "$1" in merge-base) echo abc123 ;; log) echo "abc123 base" ;; esac
STUB
# The run ends $CONCLUSION (or stays in progress when it is empty); $DOWNLOAD is "none" (a run that uploaded
# nothing) or "reports"; $MAIN is "idle", "busy" or "unreadable".
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
echo "$*" >> "$CALLS"
case "$1 $2" in
  "run list")
    if [[ "$*" == *main.yml* ]]; then
      case "$MAIN" in idle) echo 0 ;; busy) echo 1 ;; *) echo "HTTP 502" >&2; exit 1 ;; esac
    else echo 4242; fi ;;
  "run view")
    if [[ "$*" == *status,conclusion* ]]; then
      if [ -n "$CONCLUSION" ]; then printf 'completed\t%s\n' "$CONCLUSION"; else printf 'in_progress\t\n'; fi
    else echo "https://example.test/run/4242"; fi ;;
  "run download")
    [ "$DOWNLOAD" = reports ] || { echo "no valid artifacts found to download" >&2; exit 1; }
    while [ "$#" -gt 0 ]; do [ "$1" = "--dir" ] && dir="$2"; shift; done
    mkdir -p "$dir/identity-measure-v-pl"; echo decided > "$dir/identity-measure-v-pl/full-pl-decisions.txt" ;;
esac
exit 0
STUB
chmod +x "$work/bin/git" "$work/bin/gh"

measure() { # <conclusion> <download> [main state]: exit code; output in $work/log-<download>, gh calls in $work/calls
  : > "$work/calls"
  ( cd "$work" && CONCLUSION="$1" DOWNLOAD="$2" MAIN="${3:-idle}" CALLS="$work/calls" BASE_ONLY=1 \
      DISPATCH_POLL_SECONDS=0 MAIN_IDLE_MINUTES=0 MEASURE_MINUTES=0 PATH="$work/bin:$PATH" \
      bash "$REPO_ROOT/scripts/identity-measure-ci.sh" v "$work/out-$2" > "$work/log-$2" 2>&1 )
  echo $?
}
dispatched() { grep -c '^workflow run identity-measure.yml --ref main -f variant=v ' "$work/calls"; }

check "a green run's reports land in the out dir" "0" "$(measure success reports)"
check "...one file per report" "decided" "$(cat "$work/out-reports/full-pl-decisions.txt" 2>/dev/null)"
check "a run that failed before uploading anything exits 1, the RUN's verdict, not the download's" \
  "1" "$(measure failure none)"
check "...saying it found no reports" "true" "$(grep -q 'no reports to download' "$work/log-none" && echo true || echo false)"
check "Main still running past MAIN_IDLE_MINUTES is no dispatch" "2" "$(measure success reports busy)"
check "...nothing dispatched" "0" "$(dispatched)"
check "Main's runs that cannot be read are not 'Main is idle'" "2" "$(measure success reports unreadable)"
check "...nothing dispatched" "0" "$(dispatched)"
check "a run still going past MEASURE_MINUTES ends the wait, no measurement" "2" "$(measure "" reports)"

spec_summary
