#!/usr/bin/env bash
#
# Assertions for start-local-mongo.sh's START step — which formula it asks
# `brew services` for, and that it still brings Mongo up when `brew services`
# cannot. No real brew, mongod or mongosh: all three are stubbed on PATH and
# record what they were asked, and the config lands in a temp prefix, so this
# never touches the machine's own Mongo.
#
#   scripts/local-mirror/start-local-mongo-spec.sh
#
# Exits 0 when every case passes, 1 otherwise (the shared scripts/shell-spec.sh harness).
#
# Why this file exists: the script hardcoded `mongodb-community@7.0`, and on a
# machine where `brew services` could not load that service it died under
# `set -e` with nothing started — the local stack and every it/ suite waiting on
# :28017 with it.

set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT="${START_LOCAL_MONGO:-$HERE/start-local-mongo.sh}"
. "$HERE/../shell-spec.sh"

# Every sandbox lives under one temp root, removed however the spec exits.
ROOT="$(mktemp -d)"
trap 'rm -rf "$ROOT"' EXIT

# Runs a case function and reports whether it held, through the shared harness.
holds() { if "$1"; then echo yes; else echo no; fi; }

# A sandbox: stub binaries first on PATH, a temp brew prefix, and a call log.
#   FORMULAS       lines `brew list --formula` prints
#   SERVICES_EXIT  what `brew services restart` exits with
sandbox() {
  local dir; dir="$(mktemp -d "$ROOT/sandbox.XXXXXX")"
  mkdir -p "$dir/bin" "$dir/prefix/etc"
  cat > "$dir/bin/brew" <<STUB
#!/usr/bin/env bash
echo "brew \$*" >> "$dir/calls"
case "\$1" in
  --prefix) echo "$dir/prefix" ;;
  list)     printf '%b' "\${FORMULAS:-}" ;;
  services) exit "\${SERVICES_EXIT:-0}" ;;
esac
STUB
  cat > "$dir/bin/mongod" <<STUB
#!/usr/bin/env bash
echo "mongod \$*" >> "$dir/calls"
STUB
  # Answers every probe the script makes as a healthy single-node primary.
  cat > "$dir/bin/mongosh" <<'STUB'
#!/usr/bin/env bash
case "$*" in
  *ping*)              echo 1 ;;
  *isWritablePrimary*) echo true ;;
  *)                   echo "[local-mongo] replica set already initiated" ;;
esac
STUB
  chmod +x "$dir/bin/"*
  : > "$dir/calls"
  echo "$dir"
}

run_script() {
  local dir="$1"
  PATH="$dir/bin:$PATH" LOCAL_MIRROR_PORT=28999 bash "$SCRIPT" > "$dir/out" 2>&1
}

echo "[spec] start-local-mongo"

# ── The formula brew actually has is the one asked for ───────────────────────
uses_the_installed_formula() {
  local dir; dir="$(sandbox)"
  FORMULAS='mongodb-community@8.0\nmongodb-database-tools\nmongosh\n' run_script "$dir" || return 1
  grep -qx 'brew services restart mongodb-community@8.0' "$dir/calls" && ! grep -q '^mongod ' "$dir/calls"
}
check "asks brew services for the INSTALLED formula, not a hardcoded version" yes "$(holds uses_the_installed_formula)"

# ── brew services failing falls back to mongod --fork on the same config ─────
falls_back_when_brew_services_fails() {
  local dir; dir="$(sandbox)"
  FORMULAS='mongodb-community@7.0\n' SERVICES_EXIT=1 run_script "$dir" || return 1
  grep -qx "mongod --config $dir/prefix/etc/mongod.conf --fork" "$dir/calls" \
    && grep -q 'port: 28999' "$dir/prefix/etc/mongod.conf"
}
check "falls back to mongod --config … --fork when brew services cannot start it" yes "$(holds falls_back_when_brew_services_fails)"

# ── No formula at all still starts mongod ────────────────────────────────────
starts_mongod_without_a_formula() {
  local dir; dir="$(sandbox)"
  FORMULAS='mongosh\n' run_script "$dir" || return 1
  ! grep -q '^brew services' "$dir/calls" && grep -q '^mongod --config .* --fork$' "$dir/calls"
}
check "starts mongod directly when no mongodb-community formula is installed" yes "$(holds starts_mongod_without_a_formula)"

spec_summary
