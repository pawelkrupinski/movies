#!/usr/bin/env bash
# wait-for-mongo-tunnel.sh against stand-ins for everything outside the runner: an `ssh` that answers the forced
# command's `credential`, a `socat`, a `setsid` that records the supervisor instead of detaching it, and a `python3`
# whose mongo-ping.py answers or not. It refuses without the key or mongo-1's host key, fails on an empty credential,
# writes the relay with the forced `connect` verb, and is ready only once the probe answers -- then exports the URI.
# Run: bash scripts/ci/wait-for-mongo-tunnel-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m wait-for-mongo-tunnel.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin"
real_python=$(command -v python3)

# ssh: `credential` prints $STUB/credential (absent: nothing); anything else is a relay, never run here
cat > "$work/bin/ssh" <<'STUB'
#!/usr/bin/env bash
echo "ssh $*" >> "$STUB/calls"
for last; do :; done
[ "$last" = credential ] && cat "$STUB/credential" 2>/dev/null
exit 0
STUB
# socat and setsid only record their arguments: the supervisor is never started -- it would loop forever
for tool in socat setsid; do
  # shellcheck disable=SC2016 # expanded by the stub, not here
  printf '#!/usr/bin/env bash\necho "%s $*" >> "$STUB/calls"\n' "$tool" > "$work/bin/$tool"
done
# python3: the probe answers when $STUB/answers exists; every other use is the real interpreter
cat > "$work/bin/python3" <<STUB
#!/usr/bin/env bash
case "\${1:-}" in
  */mongo-ping.py) echo "probe \$*" >> "\$STUB/calls"; [ -e "\$STUB/answers" ] ;;
  *) exec "$real_python" "\$@" ;;
esac
STUB
chmod +x "$work/bin/"*

# tunnel <case dir> [env assignments...] -> the script's exit status; its output in <case>/out
tunnel() {
  local dir=$1; shift
  mkdir -p "$dir/tmp"
  env STUB="$dir" PATH="$work/bin:$PATH" RUNNER_TEMP="$dir/tmp" TUNNEL_LOG="$dir/tunnel.log" GITHUB_ENV="$dir/github-env" \
    TUNNEL_ATTEMPTS=1 TUNNEL_PROBE_TRIES=1 MONGO_CI_SSH_KEY="PRIVATE KEY" MONGO_CI_SSH_HOST_KEY="mongo-1 ssh-ed25519 AAAA" \
    MONGO_CI_SSH_TARGET=mongociread@example.test "$@" bash "$REPO_ROOT/scripts/ci/wait-for-mongo-tunnel.sh" 27999 \
    > "$dir/out" 2>&1
}

c="$work/no-key"; tunnel "$c" MONGO_CI_SSH_KEY=
check "no key: fails" "1" "$?"
check "...naming the secret" "1" "$(grep -c 'MONGO_CI_SSH_KEY is not set' "$c/out")"
check "...before any ssh" "0" "$(grep -c '^ssh' "$c/calls" 2>/dev/null || echo 0)"

c="$work/no-host-key"; tunnel "$c" MONGO_CI_SSH_HOST_KEY=
check "no host key: refuses to connect unverified" "1" "$?"
check "...saying so" "1" "$(grep -c 'Refusing to connect unverified' "$c/out")"
check "...before any ssh" "0" "$(grep -c '^ssh' "$c/calls" 2>/dev/null || echo 0)"

c="$work/no-credential"; tunnel "$c"
check "an empty credential: fails" "1" "$?"
check "...saying the endpoint returned none" "1" "$(grep -c 'returned no credential' "$c/out")"
check "...and starts no supervisor" "0" "$(grep -c '^setsid' "$c/calls")"

c="$work/ready"; mkdir -p "$c"; echo "reader:s3cret" > "$c/credential"; touch "$c/answers"
tunnel "$c"
check "the probe answers: ready" "0" "$?"
check "the URI goes to GITHUB_ENV, direct, pool-bounded, on the local port" \
  "KINOWO_CONVERGENCE_SCRAPES_URI=mongodb://reader:s3cret@127.0.0.1:27999/?authSource=admin&directConnection=true&maxPoolSize=8" \
  "$(cat "$c/github-env")"
check "the password is masked" "1" "$(grep -cx '::add-mask::s3cret' "$c/out")"
check "...and the whole userinfo" "1" "$(grep -cx '::add-mask::reader:s3cret' "$c/out")"
check "the credential was asked of the target with the pinned host key, strictly" "1" \
  "$(grep '^ssh ' "$c/calls" | grep -c 'StrictHostKeyChecking=yes.*mongociread@example.test credential$')"
check "the key is written for ssh, private" "PRIVATE KEY" "$(cat "$c/tmp/mongo-ci-read.key")"
check "...owner-only" "-rw-------" "$(find "$c/tmp/mongo-ci-read.key" -perm 600 -exec echo -rw------- \;)"
check "the relay asks the forced command for nothing but \`connect\`" "  mongociread@example.test connect" "$(tail -1 "$c/tmp/mongo-relay.sh")"
check "...through the same pinned key, never prompting" "2" \
  "$(grep -cE "^  ($c/tmp/mongo-ci-read.key|BatchMode=yes) \\\\\$" "$c/tmp/mongo-relay.sh")"
check "the supervisor is detached, not left in the step's group" "1" "$(grep -c "^setsid nohup $c/tmp/mongo-tunnel-supervisor.sh" "$c/calls")"
check "...and listens on the port, a relay per connection" "1" \
  "$(grep -c "socat \"TCP4-LISTEN:27999,bind=127.0.0.1,reuseaddr,fork\" \"EXEC:$c/tmp/mongo-relay.sh\"" "$c/tmp/mongo-tunnel-supervisor.sh")"

c="$work/silent"; mkdir -p "$c"; echo "reader:s3cret" > "$c/credential"; echo "socat: connection refused" > "$c/tunnel.log"
tunnel "$c"
check "the probe never answers: fails" "1" "$?"
check "...saying Mongo never answered" "1" "$(grep -c 'never answered a handshake on :27999' "$c/out")"
check "...with the tunnel log's tail" "1" "$(grep -c 'socat: connection refused' "$c/out")"
check "...and exports no URI" "absent" "$([ -e "$c/github-env" ] && echo present || echo absent)"

spec_summary
