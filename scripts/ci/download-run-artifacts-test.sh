#!/usr/bin/env bash
# download-run-artifacts.sh against a stub `gh` that answers STUB_ERR (exit 1) for its first STUB_TIMES calls, then lays
# the two matching artifacts out by name as `gh run download -p` does.
# Run: bash scripts/ci/download-run-artifacts-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m download-run-artifacts.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin"
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$STUB_LOG"
n=$(( $(cat "$STUB_COUNT" 2>/dev/null || echo 0) + 1 )); echo "$n" > "$STUB_COUNT"
if [ "$n" -le "${STUB_TIMES:-0}" ]; then
  dir=""; prev=""; for a in "$@"; do [ "$prev" = --dir ] && dir=$a; prev=$a; done
  mkdir -p "$dir/half"; : > "$dir/half/cut-short" # what an attempt cut short leaves behind
  echo "$STUB_ERR" >&2; exit 1
fi
dir=""; prev=""; for a in "$@"; do [ "$prev" = --dir ] && dir=$a; prev=$a; done
[ "$(ls -A "$dir")" = "" ] || { echo "error: file exists" >&2; exit 1; }
mkdir -p "$dir/identity-drift-uk-pass1" "$dir/identity-drift-uk-pass2"
echo a > "$dir/identity-drift-uk-pass1/takes.tsv"; echo b > "$dir/identity-drift-uk-pass2/takes.tsv"
STUB
chmod +x "$work/bin/gh"

# fetch <times> <stderr> -> "<exit>"; its output in $work/out
fetch() {
  rm -rf "$work/dest" "$work/count"; : > "$work/log"
  STUB_TIMES=$1 STUB_ERR=$2 STUB_COUNT="$work/count" STUB_LOG="$work/log" PATH="$work/bin:$PATH" GH_DOWNLOAD_BACKOFF=0 \
    GH_DOWNLOAD_PUBLIC=0 GITHUB_RUN_ID=42 GITHUB_REPOSITORY=o/r \
    bash "$REPO_ROOT/scripts/ci/download-run-artifacts.sh" 'identity-drift-uk-pass*' "$work/dest" > "$work/out" 2>&1
  echo $?
}

check "every matching artifact lands in a directory of its name" "0|a b" \
  "$(fetch 0 "")|$(cat "$work/dest/identity-drift-uk-pass1/takes.tsv" "$work/dest/identity-drift-uk-pass2/takes.tsv" | tr '\n' ' ' | sed 's/ $//')"
check "...asking gh for this run's pattern, nested by name" "run download 42 -R o/r --pattern identity-drift-uk-pass* --dir $work/dest" \
  "$(cat "$work/log")"
check "...and naming what it downloaded" "downloaded identity-drift-uk-pass1 downloaded identity-drift-uk-pass2" \
  "$(grep '^downloaded' "$work/out" | tr '\n' ' ' | sed 's/ $//')"
# Identity takes' A/A compare read its passes with one `gh run download`: GitHub's first 5xx failed the job.
check "a 5xx is asked again, the cut-short attempt cleared, and downloads" "0|2|absent" \
  "$(fetch 1 "HTTP 502: Bad Gateway")|$(cat "$work/count")|$([ -e "$work/dest/half" ] && echo present || echo absent)"
check "...each failed attempt said in the log" "1" "$(grep -c 'attempt 1 of 10 failed' "$work/out")"
check "a timeout-shaped failure is asked again too" "0|2" "$(fetch 1 "read tcp: i/o timeout")|$(cat "$work/count")"
check "a 5xx through every attempt fails, as a failed read" "1|10|1" \
  "$(fetch 99 "HTTP 503: Service Unavailable")|$(cat "$work/count")|$(grep -c '::error::could not download .* a failed read' "$work/out")"
check "no matching artifact fails at once, never asked again" "1|1|1" \
  "$(fetch 99 "no artifact matches any of the names or patterns provided")|$(cat "$work/count")|$(grep -c '::error::no artifact of run 42 matches' "$work/out")"

spec_summary
