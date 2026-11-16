#!/usr/bin/env bash
# seed-sbt-runner.sh against a stub `curl` that logs its arguments and writes a fake sbt release zip, or fails with a
# chosen exit code (6, as the GitHub-hosted runner's DNS did in Identity model convergence #1195).
# Run: bash scripts/ci/seed-sbt-runner-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m seed-sbt-runner.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/release/sbt/bin" "$work/empty/sbt/conf"
printf '#!/usr/bin/env bash\necho fake-sbt\n' > "$work/release/sbt/bin/sbt"
chmod +x "$work/release/sbt/bin/sbt"
(cd "$work/release" && zip -qr "$work/good.zip" sbt)
(cd "$work/empty" && zip -qr "$work/empty.zip" sbt)
good_sha=$(sha256sum "$work/good.zip" | cut -d' ' -f1)
empty_sha=$(sha256sum "$work/empty.zip" | cut -d' ' -f1)
mkdir -p "$work/bin"
cat > "$work/bin/curl" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$STUB_LOG"
[ -n "${STUB_FAIL:-}" ] && exit "$STUB_FAIL"
out=""; while [ $# -gt 0 ]; do [ "$1" = "-o" ] && { out="$2"; shift; }; shift; done
cp "$STUB_ZIP" "$out"
STUB
chmod +x "$work/bin/curl"

# seed <tool cache> [sha256] -> prints the exit status; stderr to $work/stderr.
seed() {
  rm -f "$work/curls"
  PATH="$work/bin:$PATH" STUB_LOG="$work/curls" STUB_ZIP="${STUB_ZIP:-$work/good.zip}" \
    RUNNER_TOOL_CACHE="$1" RUNNER_TEMP="$work/temp-$(basename "$1")" FETCH_RETRY_MAX_TIME=1 \
    bash "$REPO_ROOT/scripts/ci/seed-sbt-runner.sh" 2.0.10 "${2:-$good_sha}" > "$work/stdout" 2> "$work/stderr"
  echo $?
}

check "an empty tool cache is seeded from the release" "0" "$(seed "$work/cold")"
check "...the seeded runner is where setup-sbt looks first" "fake-sbt" "$("$work/cold/sbt/2.0.10/sbt/bin/sbt")"
check "...from the pinned release URL" "1" \
  "$(grep -c 'https://github.com/sbt/sbt/releases/download/v2.0.10/sbt-2.0.10.zip' "$work/curls")"
check "...through fetch-verified's retries, every error class" "1" "$(grep -c -- '--retry-all-errors' "$work/curls")"

check "a tool cache that already holds the runner downloads nothing, even while DNS fails" "0" \
  "$(STUB_FAIL=6 seed "$work/cold")"
check "...curl never ran" "absent" "$( [ -f "$work/curls" ] && echo present || echo absent)"

check "a download that keeps failing to resolve the host fails with curl's exit" "6" "$(STUB_FAIL=6 seed "$work/dns")"
check "...and names the cause" "1" "$(grep -c 'curl exit 6, could not resolve the host (DNS)' "$work/stderr")"
check "...and seeds nothing" "absent" "$( [ -e "$work/dns/sbt/2.0.10/sbt/bin/sbt" ] && echo present || echo absent)"

check "a download that fails its checksum fails the seed" "1" "$(seed "$work/tampered" "$empty_sha")"
check "a zip with no sbt/bin/sbt fails the seed" "1" "$(STUB_ZIP="$work/empty.zip" seed "$work/hollow" "$empty_sha")"
check "...and says so" "1" "$(grep -c 'holds no runner launcher' "$work/stderr")"

spec_summary
