#!/usr/bin/env bash
# push-k8s-secrets --dry-run against a stub vault and a stub ssh: which vault namespace each map
# row reads, and which rows it skips.
# Run: bash infra/bin/push-k8s-secrets-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m push-k8s-secrets\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
stub="$work/bin"; vault="$work/vault"
mkdir -p "$stub" "$vault/movies" "$vault/filmowo"

# The stub vault: one file per key under <namespace>/; it records each `get` it answers.
cat > "$stub/secrets" <<'STUB'
#!/usr/bin/env bash
case "$1" in
  unlock) exit 0 ;;
  list)   ls -1 "$STUB_VAULT/$2" ;;
  get)    [ -f "$STUB_VAULT/$2/$3" ] || { echo "error: not found" >&2; exit 1; }
          echo "$2:$3" >> "$STUB_VAULT/gets"; cat "$STUB_VAULT/$2/$3" ;;
  *)      exit 2 ;;
esac
STUB
# The stub server: every Secret is empty, so every key the vault holds would change. STUB_SSH_FAIL_GET makes the
# read of the current Secret fail; a patch's stdin is kept in $STUB_VAULT/patch.
cat > "$stub/ssh" <<'STUB'
#!/usr/bin/env bash
case "$*" in
  *"get secret"*) exec bash -c "${@: -1}" ;;   # run the remote pipeline here, against the stub kubectl
  *"patch secret"*) cat > "$STUB_VAULT/patch" ;;
esac
exit 0
STUB
# The stub kubectl: an empty Secret, or a failure.
cat > "$stub/kubectl" <<'STUB'
#!/usr/bin/env bash
[ -z "${STUB_SSH_FAIL_GET:-}" ] || exit 1
echo '{"data":{}}'
STUB
# A jq that records every argument it is started with, to prove no secret is among them.
real_jq=$(command -v jq)
cat > "$stub/jq" <<STUB
#!/usr/bin/env bash
printf '%s\n' "\$@" >> "\$STUB_VAULT/jq-argv"
exec $real_jq "\$@"
STUB
chmod +x "$stub/secrets" "$stub/ssh" "$stub/jq" "$stub/kubectl"

printf 'kinowo-tmdb' > "$vault/movies/TMDB_API_KEY"
printf 'filmowo-tmdb' > "$vault/filmowo/TMDB_API_KEY"
printf 'filmowo-only' > "$vault/filmowo/RAPIDAPI_KEY"
cat > "$work/secrets.map" <<'MAP'
# <namespace>/<secret>  <SECRET_KEY>  <VAULT_KEY>
kinowo/worker-secrets   TMDB_API_KEY  TMDB_API_KEY
filmowo/filmowo-secrets TMDB_API_KEY  filmowo:TMDB_API_KEY
filmowo/filmowo-secrets RAPIDAPI_KEY  filmowo:RAPIDAPI_KEY
filmowo/filmowo-secrets TRAKT_KEY     filmowo:TRAKT_KEY
kinowo/worker-secrets   RAPIDAPI_KEY  RAPIDAPI_KEY
MAP

out=$(PATH="$stub:$PATH" STUB_VAULT="$vault" K8S_SECRETS_MAP="$work/secrets.map" \
      bash "$REPO_ROOT/infra/bin/push-k8s-secrets" --dry-run 2>&1)

check "a plain vault key reads namespace movies, a filmowo: key reads namespace filmowo" \
  "filmowo:RAPIDAPI_KEY filmowo:TMDB_API_KEY movies:TMDB_API_KEY" "$(sort "$vault/gets" | tr '\n' ' ' | sed 's/ $//')"
check "a filmowo Secret is fed from the filmowo vault" \
  "filmowo/filmowo-secrets: WOULD update TMDB_API_KEY RAPIDAPI_KEY" "$(grep '^filmowo/' <<<"$out")"
check "a key missing from its namespace is skipped, named with that namespace" \
  "  filmowo/filmowo-secrets TRAKT_KEY: vault key filmowo:TRAKT_KEY not set yet -- skipped" "$(grep TRAKT_KEY <<<"$out")"
check "a plain key is never looked up in another namespace" \
  "  kinowo/worker-secrets RAPIDAPI_KEY: vault key movies:RAPIDAPI_KEY not set yet -- skipped" "$(grep 'worker-secrets RAPIDAPI' <<<"$out")"

# A real push: the patch reaches the server on stdin with each value base64'd, and no value is in any jq argv.
rm -f "$vault/gets" "$vault/jq-argv" "$vault/patch"
PATH="$stub:$PATH" STUB_VAULT="$vault" K8S_SECRETS_MAP="$work/secrets.map" \
  bash "$REPO_ROOT/infra/bin/push-k8s-secrets" kinowo/worker-secrets >/dev/null 2>&1
check "the patch carries the base64 value, one data entry per changed key" \
  '{"data":{"TMDB_API_KEY":"a2lub3dvLXRtZGI="}}' "$(jq -c . "$vault/patch" 2>/dev/null)"
check "no secret value appears in any jq argument" "0" "$(grep -c 'kinowo-tmdb' "$vault/jq-argv")"

# An unreadable Secret is an error, not "every key changed".
rm -f "$vault/patch"
PATH="$stub:$PATH" STUB_VAULT="$vault" STUB_SSH_FAIL_GET=1 K8S_SECRETS_MAP="$work/secrets.map" \
  bash "$REPO_ROOT/infra/bin/push-k8s-secrets" kinowo/worker-secrets >/dev/null 2>&1; code=$?
check "a failed read of the current Secret aborts without patching" "1 absent" \
  "$code $([ -e "$vault/patch" ] && echo patched || echo absent)"

spec_summary
