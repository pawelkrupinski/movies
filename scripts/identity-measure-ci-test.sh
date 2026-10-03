#!/usr/bin/env bash
# identity-measure-ci.sh against stub `git` and `gh`: what it reports once the watched run ends.
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
# The run ends with $WATCH_STATUS; $DOWNLOAD is "none" (a run that uploaded nothing) or "reports".
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
case "$1 $2" in
  "run list") [[ "$*" == *main.yml* ]] || echo 4242 ;;
  "run view") echo "https://example.test/run/4242" ;;
  "run watch") exit "$WATCH_STATUS" ;;
  "run download")
    [ "$DOWNLOAD" = reports ] || { echo "no valid artifacts found to download" >&2; exit 1; }
    while [ "$#" -gt 0 ]; do [ "$1" = "--dir" ] && dir="$2"; shift; done
    mkdir -p "$dir/identity-measure-v-pl"; echo decided > "$dir/identity-measure-v-pl/full-pl-decisions.txt" ;;
esac
exit 0
STUB
chmod +x "$work/bin/git" "$work/bin/gh"

measure() {
  ( cd "$work" && WATCH_STATUS="$1" DOWNLOAD="$2" BASE_ONLY=1 PATH="$work/bin:$PATH" \
      bash "$REPO_ROOT/scripts/identity-measure-ci.sh" v "$work/out-$2" > "$work/log-$2" 2>&1 )
  echo $?
}

check "a green run's reports land in the out dir" "0" "$(measure 0 reports)"
check "...one file per report" "decided" "$(cat "$work/out-reports/full-pl-decisions.txt" 2>/dev/null)"
check "a run that failed before uploading anything exits with the RUN's status, not the download's" \
  "3" "$(measure 3 none)"
check "...saying it found no reports" "true" "$(grep -q 'no reports to download' "$work/log-none" && echo true || echo false)"

spec_summary
