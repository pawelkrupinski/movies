#!/usr/bin/env bash
# Assertions for scripts/prod-ops.sh: the mode parser, the exact-names guard, the refusal helpers and the session
# re-exec, through a throwaway tool against the shared fake prod (scripts/prod-ops-fixture.sh).
#
#   scripts/prod-ops-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/prod-ops-fixture.sh"
prod_ops_fixture_init prod-ops

# A tool in the harness's shape: --check | --apply, --wipe a b, one session, a refusal when the probe says so.
TOOL="$SCRATCH/tool"
cat > "$TOOL" <<'TOOL_SH'
#!/usr/bin/env bash
# A throwaway prod-ops tool.
#
#   tool --check | --apply | --wipe a b
set -euo pipefail
. "$PROD_OPS_LIB"
prod_ops_init tool
args=("$@")
case "${1:-}" in
  --wipe) shift; prod_ops_exact_names --wipe "a b" "$@"; PROD_OPS_MODE=wipe ;;
  *) prod_ops_mode "$@" ;;
esac
prod_ops_session --both "${args[@]}"
prod_ops_phase "mode $PROD_OPS_MODE"
as_root 'ping' | last_line
[ -z "${REFUSE:-}" ] || prod_ops_refuse "$REFUSE"
TOOL_SH
chmod +x "$TOOL"
fake_mongosh <<'BODY'
echo "root probe as $as" >> "$CALLS"; echo "answered"
BODY
export PROD_OPS_LIB="$REPO_ROOT/scripts/prod-ops.sh"
tool() { prod_ops_fixture_fresh; PROD_OPS_LIB="$PROD_OPS_LIB" REFUSE="${REFUSE:-}" prod_ops_run "$TOOL" "$@"; }

echo "[spec] modes"
out="$(tool --check)"; status=$?
check "--check runs the body inside one session: exit 0, the phase line, the fake's answer" \
  "0 == mode check" "$status $(printf '%s\n' "$out" | grep '^==')"
check "…one vault run for both credentials, one tunnel" "1 1" \
  "$(grep -c '^secrets run movies MONGODB_URI MONGO_ROOT_URI ' "$CALLS") $(grep -c '^ssh' "$CALLS")"
out="$(tool --apply)"; check "--apply is the other mode" "== mode apply" "$(printf '%s\n' "$out" | grep '^==')"
for bad in "" "--check --apply" "--apply x" "--other"; do
  read -r -a words <<< "$bad"; tool ${words[@]+"${words[@]}"} >/dev/null; status=$?
  check "refused with usage, exit 2, vault untouched: '$bad'" "2 " "$status $(cat "$CALLS" 2>/dev/null)"
done
tool --help >/dev/null; check "--help: exit 0" "0" "$?"

echo "[spec] exact names"
out="$(tool --wipe b a)"; check "the names in any order pass" "== mode wipe" "$(printf '%s\n' "$out" | grep '^==')"
for bad in "--wipe" "--wipe a" "--wipe a a" "--wipe a b c" "--wipe a*"; do
  read -r -a words <<< "$bad"; tool "${words[@]}" >/dev/null; status=$?
  check "refused, exit 2, vault untouched: $bad" "2 " "$status $(cat "$CALLS" 2>/dev/null)"
done

echo "[spec] refusal"
out="$(REFUSE='state says no' tool --apply)"; status=$?
check "prod_ops_refuse: exit 1, the tool's name and the reason" "1 [tool] state says no" \
  "$status $(printf '%s\n' "$out" | grep '^\[tool\]')"
spec_summary
