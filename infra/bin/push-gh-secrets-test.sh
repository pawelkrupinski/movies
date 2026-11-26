#!/usr/bin/env bash
# push-gh-secrets --dry-run against a stub vault: --all with no names (an empty array, which bash 3.2 under
# `set -u` calls unbound), and a named push.
# Run: bash infra/bin/push-gh-secrets-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m push-gh-secrets\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
stub="$work/bin"
mkdir -p "$stub"
# Every vault key the map names exists, so every row would be pushed.
cat > "$stub/secrets" <<'STUB'
#!/usr/bin/env bash
case "$1" in
  unlock) exit 0 ;;
  list)   awk '!/^[ \t]*(#|$)/ {print $3}' "$STUB_MAP" ;;
  *)      exit 2 ;;
esac
STUB
chmod +x "$stub/secrets"
export STUB_MAP="$REPO_ROOT/infra/k8s-secrets/github-secrets.map"
first_name="$(awk '!/^[ \t]*(#|$)/ {print $2; exit}' "$STUB_MAP")"
rows="$(grep -cvE '^[[:space:]]*(#|$)' "$STUB_MAP")"

# push <args...>: the output in $work/out, "<status>" on stdout. /bin/bash is the 3.2 the empty-array bug bites on.
push() {
  PATH="$stub:$PATH" /bin/bash "$REPO_ROOT/infra/bin/push-gh-secrets" "$@" > "$work/out" 2>&1
  echo "$?"
}

check "--all --dry-run with no names would push every row, and succeeds" "0:$rows" \
  "$(status=$(push --all --dry-run); echo "$status:$(grep -c 'WOULD push' "$work/out")")"
check "a named --dry-run would push that secret's rows only" "0:1" \
  "$(status=$(push --dry-run "$first_name"); echo "$status:$(grep -c "$first_name: WOULD push" "$work/out")")"
check "no names and no --all is a usage error" "2" "$(push --dry-run)"
check "a name the map lacks is refused" "2" "$(push --dry-run NOT_A_SECRET)"

spec_summary
