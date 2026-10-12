#!/usr/bin/env bash
# Assertions for the environment-else-vault secret lookup: scripts/vault-env.sh (bash),
# scripts/vault_env.py (python), and the scripts that use them outside the prod-Mongo path. A fake
# `secrets` on PATH records how it was called and exits; the real vault is never touched.
#
#   scripts/vault-env-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

SCRATCH="$(mktemp -d "${TMPDIR:-/tmp}/kinowo-vault-env.XXXXXX")"
trap 'rm -rf "$SCRATCH"' EXIT
FAKE="$SCRATCH/bin"; mkdir -p "$FAKE"
export CALLS="$SCRATCH/calls"
printf '#!/bin/sh\necho "$*" >> "$CALLS"\n' > "$FAKE/secrets"; chmod +x "$FAKE/secrets"
PY="$(command -v python3)"
PY_EXE="$("$PY" -c 'import sys; print(sys.executable)')"   # what a re-exec names
SECRET_VARS=(-u TMDB_API_KEY -u DECODO_PROXY_USER -u DECODO_PROXY_PASS -u ZYTE_API_KEY)

# Runs "$@" with the fake vault first on PATH and none of the secrets set; prints what the vault saw.
with_fake_vault() { rm -f "$CALLS"; env "${SECRET_VARS[@]}" PATH="$FAKE:$PATH" "$@" >/dev/null 2>&1; cat "$CALLS" 2>/dev/null; }
# …and with no `secrets` CLI at all, as on a CI runner; prints the exit status and stderr.
without_vault() { { env "${SECRET_VARS[@]}" PATH=/usr/bin:/bin "$@" >/dev/null; } 2>&1; echo "exit=$?"; }

echo "[spec] ensure_vault_env (bash)"
HELPER=". '$REPO_ROOT/scripts/vault-env.sh'; ensure_vault_env A B=KEY_B -- echo ran-under-vault"
check "every variable set: no vault, the caller carries on" "" "$(A=1 B=2 with_fake_vault "$BASH" -c "$HELPER")"
check "only the missing specs go to the vault, VAR=KEY kept" "run movies B=KEY_B -- echo ran-under-vault" \
  "$(A=1 with_fake_vault "$BASH" -c "$HELPER")"
check "no secrets CLI: returns 1 and names the variable" \
  "[vault] A B not set, and no \`secrets\` CLI on PATH to read it from the vault exit=0" \
  "$(without_vault "$BASH" -c ". '$REPO_ROOT/scripts/vault-env.sh'; ensure_vault_env A B=KEY_B -- true || echo returned-\$? >&2" | tr '\n' ' ' | sed 's/returned-1 //;s/ $//')"

echo "[spec] record-country-fixture.sh"
REC="$REPO_ROOT/.github/scripts/record-country-fixture.sh"
check "locally, the missing upstream keys are read from the vault" \
  "run movies DECODO_PROXY_USER DECODO_PROXY_PASS -- $BASH $REC" \
  "$(with_fake_vault env TMDB_API_KEY=t "$BASH" "$REC")"
check "in CI (no secrets CLI) a missing key still fails loudly" "1" \
  "$(without_vault "$BASH" "$REC" | grep -c '::error::Missing required key(s): TMDB_API_KEY DECODO_PROXY_USER DECODO_PROXY_PASS')"

echo "[spec] vault_env.py"
AMC="$REPO_ROOT/data/us/scripts/generate_amc_map.py"
FILMSTARTS="$REPO_ROOT/data/germany/scripts/crawl_filmstarts.py"
check "generate_amc_map.py re-runs itself under the vault for ZYTE_API_KEY" \
  "run movies ZYTE_API_KEY -- $PY_EXE $AMC" "$(with_fake_vault "$PY" "$AMC")"
check "crawl_filmstarts.py re-runs itself under the vault for the proxy credentials" \
  "run movies DECODO_PROXY_USER DECODO_PROXY_PASS -- $PY_EXE $FILMSTARTS" "$(with_fake_vault "$PY" "$FILMSTARTS")"
check "crawl_filmstarts.py without a vault fails loudly instead of crawling unproxied" \
  "DECODO_PROXY_USER DECODO_PROXY_PASS not set, and no \`secrets\` CLI on PATH to read it from the Touch ID vault exit=1" \
  "$(without_vault "$PY" "$FILMSTARTS" | tr '\n' ' ' | sed 's/ $//')"
check "importing crawl_filmstarts asks the vault for nothing" "" \
  "$(with_fake_vault "$PY" -c "import sys; sys.path.insert(0, '$REPO_ROOT/data/germany/scripts'); import crawl_filmstarts")"

spec_summary
