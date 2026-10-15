#!/usr/bin/env bash
# identity-measure-run.sh: the identity measurement's two sbt runs (boot the base, measure the variant), over a fake sbt.
# Run: bash scripts/ci/identity-measure-run-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m identity-measure-run.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin"
# The fake sbt records its arguments and the identity environment it was given, and narrates one line.
cat > "$work/bin/sbt" <<'SBT'
#!/usr/bin/env bash
{ printf 'arg %s\n' "$@"; env | grep -E '^(MONGODB_URI|KINOWO_IDENTITY_[A-Z_]+)=' | sort; } > "$FAKE_SBT_CALL"
echo "[info] Tests: succeeded 1, failed 0"
exit "${FAKE_SBT_STATUS:-0}"
SBT
chmod +x "$work/bin/sbt"
run() { (cd "$work" && env -u KINOWO_IDENTITY_FOCUS -u KINOWO_IDENTITY_ROBUSTNESS -u MONGODB_URI -u KINOWO_IDENTITY_CORPUS_DIR \
    PATH="$work/bin:$PATH" FAKE_SBT_CALL="$work/call" "$@"); }
script="$REPO_ROOT/scripts/ci/identity-measure-run.sh"

run "$script" boot pl "$work/boot" "$work/cache" > /dev/null; status=$?
check "boot runs the shadow spec boot-only, into the pipeline cache" "0" "$status"
check "...with the measurement's JVM and spec" \
  "$(printf '%s\n' -J-Xmx10g -J-Duser.language=en -J-Duser.country=US 'worker/IntegrationTest/testOnly integration.IdentityShadowIntegrationSpec')" \
  "$(sed -n 's/^arg //p' "$work/call")"
check "...and its environment, defaulted for a local run" \
  "$(printf '%s\n' "KINOWO_IDENTITY_BOOT_ONLY=true" "KINOWO_IDENTITY_CORPUS_DIR=$REPO_ROOT/test/resources/fixtures/corpus" \
     "KINOWO_IDENTITY_FULL=pl" "KINOWO_IDENTITY_OUT=$work/boot" "KINOWO_IDENTITY_PIPELINE_CACHE=$work/cache" \
     "MONGODB_URI=mongodb://127.0.0.1:27017/?directConnection=true")" \
  "$(grep -v '^arg ' "$work/call")"

KINOWO_CONVERGENCE_CORPUS_RUN=123 run env KINOWO_IDENTITY_ROBUSTNESS=on KINOWO_IDENTITY_FOCUS='Diabły' \
  "$script" measure de "$work/out" "$work/cache" > /dev/null; status=$?
check "measure succeeds when the spec does" "0" "$status"
check "...measuring, not booting, with the robustness and focus it was asked for" \
  "$(printf '%s\n' "KINOWO_IDENTITY_FOCUS=Diabły" "KINOWO_IDENTITY_FULL=de" "KINOWO_IDENTITY_OUT=$work/out" "KINOWO_IDENTITY_ROBUSTNESS=on")" \
  "$(grep -E '^KINOWO_IDENTITY_(BOOT_ONLY|FOCUS|FULL|OUT|ROBUSTNESS)=' "$work/call")"
check "...keeping the country's log under its own name" "[info] Tests: succeeded 1, failed 0" "$(cat "$work/out/run-de.log")"
check "...and naming the recording it replayed" "recording 123 tree ?" "$(sed -n 2p "$work/out/variant-de.txt")"

run env FAKE_SBT_STATUS=1 "$script" measure de "$work/out2" "$work/cache" > /dev/null; status=$?
check "a failing spec fails the measurement, through the log's pipe" "1" "$status"

run "$script" measure 'pl;id' "$work/out3" "$work/cache" > /dev/null 2>&1; status=$?
check "a country that is not a code is refused" "64" "$status"
run "$script" sample pl "$work/out3" "$work/cache" > /dev/null 2>&1; status=$?
check "and so is a mode that is neither boot nor measure" "64" "$status"

spec_summary
