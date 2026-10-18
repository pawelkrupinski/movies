#!/usr/bin/env bash
# prod-tunnel.sh gives every caller a forward of its own on a free local port, so one script closing its tunnel never
# cuts another's. ssh, nc and mongosh are stubs: no network, no prod, no real forward.
# Run: bash scripts/local-mirror/prod-tunnel-test.sh
set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$HERE/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

scratch="$(mktemp -d)"; trap 'rm -rf "$scratch"' EXIT
mkdir -p "$scratch/bin"
# nc: a port "accepts" when $scratch/open/<port> exists; ssh records its -L spec, and opens that port
mkdir -p "$scratch/open"
cat > "$scratch/bin/nc" <<STUB
#!/bin/sh
for a; do port=\$a; done
[ -e "$scratch/open/\$port" ]
STUB
cat > "$scratch/bin/ssh" <<STUB
#!/bin/sh
while [ "\$1" != -L ]; do shift; done
echo "\$2" >> "$scratch/ssh-forwards"
touch "$scratch/open/\${2%%:*}"
exec sleep 30
STUB
printf '#!/bin/sh\nexit 0\n' > "$scratch/bin/mongosh"
chmod +x "$scratch/bin"/*
export PATH="$scratch/bin:$PATH"
unset PROD_TUNNEL_PORT KINOWO_MONGO_SSH

in_tunnel() { bash -c '. "'"$HERE"'/prod-tunnel.sh"; '"$1"; }

# one shell throughout: the pid-derived start is per process
# shellcheck disable=SC2016
out="$(in_tunnel '
  p=$(prod_tunnel_free_port); echo "free=$p"
  echo "pinned=$(PROD_TUNNEL_PORT=27555 prod_tunnel_free_port)"
  touch "'"$scratch"'/open/$p"; echo "skipped=$(prod_tunnel_free_port) taken=$p"; rm -f "'"$scratch"'/open/$p"
  echo "uri=$(prod_tunnel_uri mongodb://u:p@10.20.0.13:27017/kinowo)"')"
val() { printf '%s\n' "$out" | sed -n "s/.*$1=\([^ ]*\).*/\1/p" | head -1; }
port="$(val free)"
check "the default port is a free one in 27100-27899, not 27017" "yes" "$([ "$port" -ge 27100 ] && [ "$port" -lt 27900 ] && echo yes || echo no)"
check "a pinned PROD_TUNNEL_PORT wins" "27555" "$(val pinned)"
check "a port something already accepts on is skipped" "$((port + 1))" "$(val skipped)"
check "the URI names the same port the tunnel will take" "mongodb://u:p@127.0.0.1:$port/kinowo?directConnection=true" "$(val uri)"

# Two callers at once: B opens its own forward while A's is up, and A closing cuts nothing of B's.
two_callers() {
  bash -c '
    . "'"$HERE"'/prod-tunnel.sh"
    uri_a="$(prod_tunnel_uri mongodb://u:p@10.20.0.13:27017/kinowo)"; init_prod_tunnel a "$uri_a"; ensure_prod_tunnel >/dev/null || exit 1
    port_a=$PROD_TUNNEL_PORT; pid_a=$PROD_TUNNEL_PID
    (
      . "'"$HERE"'/prod-tunnel.sh"
      PROD_TUNNEL_PORT=$((port_a + 1))
      uri_b="$(prod_tunnel_uri mongodb://u:p@10.20.0.13:27017/kinowo)"; init_prod_tunnel b "$uri_b"; ensure_prod_tunnel >/dev/null || exit 1
      echo "b=$PROD_TUNNEL_PORT pid_b=$PROD_TUNNEL_PID" > "'"$scratch"'/b"
      sleep 1; kill -0 $PROD_TUNNEL_PID && echo alive >> "'"$scratch"'/b"
      close_prod_tunnel
    ) &
    wait $!
    echo "a=$port_a" > "'"$scratch"'/a"
    kill -0 "$pid_a" && echo a-alive >> "'"$scratch"'/a"
    close_prod_tunnel
  '
}
two_callers
check "each caller's ssh forwards its own port to prod's 27017" "2" "$(grep -c ':127.0.0.1:27017$' "$scratch/ssh-forwards")"
check "the two callers use different ports" "2" "$(cut -d: -f1 "$scratch/ssh-forwards" | sort -u | wc -l | tr -d ' ')"
check "one caller closing its tunnel leaves the other's up" "a-alive" "$(grep a-alive "$scratch/a")"
check "ensure_prod_tunnel exports the port it took" "yes" "$(bash -c '. "'"$HERE"'/prod-tunnel.sh"; ensure_prod_tunnel >/dev/null; bash -c "[ -n \"\$PROD_TUNNEL_PORT\" ]" && echo yes; close_prod_tunnel')"
spec_summary
