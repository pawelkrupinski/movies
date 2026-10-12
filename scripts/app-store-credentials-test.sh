#!/usr/bin/env bash
#
# app-store-credentials.sh loads the App Store Connect API key out of the Touch ID
# vault and lays the .p8 down where altool looks for it. Exercised against a fake
# `secrets` binary, never the real vault.
#
#   scripts/app-store-credentials-test.sh
#
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
. "$HERE/shell-spec.sh"

printf '\033[36m▸\033[0m app-store-credentials.sh\n'

SCRATCH="$(mktemp -d)"
trap 'rm -rf "$SCRATCH"' EXIT

# A stand-in for ~/.local/bin/secrets: `get movies NAME` prints a canned value, or
# fails the way the real one does while the namespace is locked.
FAKE="$SCRATCH/secrets"
cat > "$FAKE" <<'SH'
#!/usr/bin/env bash
[ -n "${FAKE_LOCKED:-}" ] && { echo "Error: locked — run \`secrets unlock movies\`" >&2; exit 1; }
[ "$1 $2" = "get movies" ] || exit 2
case "$3" in
  APP_STORE_KEY_ID)    printf 'KEY1234567' ;;
  APP_STORE_ISSUER_ID) printf 'issuer-uuid' ;;
  APP_STORE_KEY_P8)    printf -- '-----BEGIN PRIVATE KEY-----\nabc\n-----END PRIVATE KEY-----' ;;
  *) exit 3 ;;
esac
SH
chmod +x "$FAKE"

# ── the happy path ────────────────────────────────────────────────────────────
out=$(SECRETS_BIN="$FAKE" bash -c '
  . "'"$HERE"'/log.sh"; . "'"$HERE"'/app-store-credentials.sh"
  load_app_store_credentials
  p8="$API_PRIVATE_KEYS_DIR/AuthKey_$APP_STORE_KEY_ID.p8"
  printf "%s|%s|%s|%s\n" "$APP_STORE_KEY_ID" "$APP_STORE_ISSUER_ID" "$(head -1 "$p8")" "$(stat -f %Lp "$p8")"
  printf "%s\n" "$API_PRIVATE_KEYS_DIR"' 2>&1)
check "exits 0 with the vault unlocked" "0" "$?"
line=$(printf '%s\n' "$out" | head -1)
check "reads the key id" "KEY1234567" "$(cut -d'|' -f1 <<<"$line")"
check "reads the issuer id" "issuer-uuid" "$(cut -d'|' -f2 <<<"$line")"
check "writes the .p8 under API_PRIVATE_KEYS_DIR as AuthKey_<id>.p8" "-----BEGIN PRIVATE KEY-----" "$(cut -d'|' -f3 <<<"$line")"
check "leaves the .p8 readable by its owner only" "600" "$(cut -d'|' -f4 <<<"$line")"
dir=$(printf '%s\n' "$out" | sed -n 2p)
check "removes the key directory when the caller exits" "gone" "$([ -e "$dir" ] && echo present || echo gone)"

# ── a locked vault ────────────────────────────────────────────────────────────
out=$(FAKE_LOCKED=1 SECRETS_BIN="$FAKE" bash -c '
  . "'"$HERE"'/log.sh"; . "'"$HERE"'/app-store-credentials.sh"
  load_app_store_credentials; echo reached' 2>&1)
check "dies on a locked vault" "1" "$?"
check "tells you how to unlock it" "1" "$(grep -c 'secrets unlock movies' <<<"$out")"
check "stops before anything uses the credentials" "0" "$(grep -c reached <<<"$out")"

spec_summary
