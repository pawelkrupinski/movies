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

# A real (not dry) prod reset scales web + worker to ZERO around the wipe. Anything failing on the way must not leave the
# country's web tier down: the tunnel is opened before anything is scaled, and a failed wipe scales both back up.
export KUBECTL_LOG="$SCRATCH/kubectl"
printf '#!/bin/sh\necho "$*" >> "$KUBECTL_LOG"\n' > "$FAKE/kubectl"
printf '#!/usr/bin/env bash\n[ -n "${SSH_FAILS:-}" ] && exit 255\ntouch "$TUNNEL_UP"; exec sleep 30\n' > "$FAKE/ssh"
printf '#!/bin/sh\necho "$*" >> "$ARGV_LOG"\n[ -e "$TUNNEL_UP" ] || exit 1\ncase "$*" in *getSiblingDB*) if [ -n "${DROP_FAILS:-}" ]; then exit 1; fi ;; esac\necho 0\n' > "$FAKE/mongosh"
wipe() { # [VAR=value...]
  rm -f "$TUNNEL_UP" "$KUBECTL_LOG"
  ( cd "$REPO_ROOT" && env -u MONGODB_URI -u MONGODB_DB KINOWO_RESET_COUNTRY=pl PATH="$FAKE:$PATH" "$@" \
      scripts/reset-corpus.sh --yes >/dev/null 2>&1 ); }
scaled() { grep -o 'scale deploy/[a-z-]* --replicas=[0-9]*' "$KUBECTL_LOG" 2>/dev/null | sed 's/scale deploy\///' | tr '\n' ' ' | sed 's/ $//'; }
wipe; status=$?
check "a reset scales both down, wipes, and scales both back up" "0 web-pl --replicas=0 worker-pl --replicas=0 worker-pl --replicas=1 web-pl --replicas=1" \
  "$status $(scaled)"
wipe SSH_FAILS=1; status=$?
check "prod unreachable: refused before anything is scaled" "1 " "$status $(scaled)"
wipe DROP_FAILS=1; status=$?
check "a wipe that fails half-way still scales both back up" "1 web-pl --replicas=0 worker-pl --replicas=0 worker-pl --replicas=1 web-pl --replicas=1" \
  "$status $(scaled)"

spec_summary
