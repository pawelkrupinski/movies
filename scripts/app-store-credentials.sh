#!/usr/bin/env bash
# The App Store Connect API key, out of the Touch ID vault (~/projects/secrets,
# namespace `movies`) — the only place it lives since `.env.local` was retired.
#
# `load_app_store_credentials` sets APP_STORE_KEY_ID and APP_STORE_ISSUER_ID and
# exports API_PRIVATE_KEYS_DIR, a private temp directory holding
# AuthKey_<KEY_ID>.p8: altool reads its key from that variable's directory, so
# the .p8 never has to sit in ~/.appstoreconnect. The directory is removed on the
# caller's EXIT (this sets the EXIT trap, so the caller must not set its own).
#
# Run `secrets unlock movies` first; a locked vault dies here, before anything
# expensive happens. SECRETS_BIN overrides the CLI's path (the test's fake).
#
# Usage:
#   . scripts/log.sh; . scripts/app-store-credentials.sh
#   load_app_store_credentials

load_app_store_credentials() {
  local bin="${SECRETS_BIN:-$HOME/.local/bin/secrets}"
  vault_get() { # $1 name
    # Handed in already (the /mobile dashboard runs the lane under `secrets run`, since the vault
    # pins an unlock to the process that asked and this script is never that one): no vault call.
    if [ -n "${!1:-}" ]; then printf '%s' "${!1}"; return; fi
    "$bin" get movies "$1" 2>/dev/null \
      || die "can't read $1 from the vault — run \`secrets unlock movies\` (Touch ID) and retry"
  }
  APP_STORE_KEY_ID=$(vault_get APP_STORE_KEY_ID) || exit 1
  # shellcheck disable=SC2034 # read by the script that sources this
  APP_STORE_ISSUER_ID=$(vault_get APP_STORE_ISSUER_ID) || exit 1
  API_PRIVATE_KEYS_DIR=$(mktemp -d)
  export API_PRIVATE_KEYS_DIR
  trap 'rm -rf "$API_PRIVATE_KEYS_DIR"' EXIT
  local p8="$API_PRIVATE_KEYS_DIR/AuthKey_$APP_STORE_KEY_ID.p8"
  (umask 077; vault_get APP_STORE_KEY_P8 > "$p8") || exit 1
}
