#!/usr/bin/env bash
# Assertions for scripts/reset-corpus.sh's choice of prod database. Dry runs only, and no vault, ssh
# or Mongo: `secrets`, `ssh`, `nc`, `mongosh` and `kubectl` are fakes on PATH.
#
#   scripts/reset-corpus-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

SCRATCH="$(mktemp -d "${TMPDIR:-/tmp}/kinowo-reset-corpus.XXXXXX")"
trap 'rm -rf "$SCRATCH"' EXIT
FAKE="$SCRATCH/bin"; mkdir -p "$FAKE"
export TUNNEL_UP="$SCRATCH/tunnel-up" ARGV_LOG="$SCRATCH/mongosh-argv"
cat > "$FAKE/secrets" <<'FAKE_EOF'
#!/usr/bin/env bash
shift 2
while [ "$1" != "--" ]; do export "${1%%=*}=mongodb://u:p@10.20.0.13:27017/?authSource=admin"; shift; done
shift
exec "$@"
FAKE_EOF
printf '#!/usr/bin/env bash\ntouch "$TUNNEL_UP"; exec sleep 30\n' > "$FAKE/ssh"
printf '#!/bin/sh\n[ -e "$TUNNEL_UP" ]\n' > "$FAKE/nc"
printf '#!/bin/sh\necho "$*" >> "$ARGV_LOG"\n[ -e "$TUNNEL_UP" ] || exit 1\necho 0\n' > "$FAKE/mongosh"
printf '#!/bin/sh\nexit 0\n' > "$FAKE/kubectl"
chmod +x "$FAKE"/*

reset() { # $1 country, $2 MONGODB_DB ("" = unset)
  rm -f "$TUNNEL_UP"
  ( cd "$REPO_ROOT" && env -u MONGODB_URI -u MONGODB_DB ${2:+MONGODB_DB=$2} KINOWO_RESET_COUNTRY="$1" PATH="$FAKE:$PATH" \
      scripts/reset-corpus.sh --dry-run 2>&1 )
}
target() { sed -n "s/.*target db '\([^']*\)'.*/\1/p" <<<"$1" | head -1; }

echo "[spec] reset-corpus.sh"
check "Poland's corpus is the kinowo database" "kinowo" "$(target "$(reset pl "")")"
check "Germany's corpus is kinowo_de, never Poland's" "kinowo_de" "$(target "$(reset de "")")"
out="$(reset de kinowo)"; status=$?
check "an exported MONGODB_DB naming another country's database is refused" "1" "$status"
check "…before anything is targeted" "" "$(target "$out")"
check "the prod URI's password never reaches mongosh's command line, where ps shows it" \
  "" "$(grep -F 'u:p@' "$ARGV_LOG")"
check "a matching exported MONGODB_DB is accepted" "kinowo_uk" "$(target "$(reset uk kinowo_uk)")"

spec_summary
