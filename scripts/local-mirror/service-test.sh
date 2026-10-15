#!/usr/bin/env bash
# service.sh, the launchd wrapper around mirror.sh: the plist `install` writes (the PATH it resolves, KeepAlive, the
# shared log), the launchctl calls of install / uninstall / status, an unknown command, the agent's wait when the vault
# says no, and the agent restarting mirror.sh with the secret inherited. No launchd, no vault: fake `launchctl`,
# `secrets` and `sleep` on PATH, HOME a scratch directory, and for the agent loop a copy of service.sh beside a fake
# mirror.sh. (mirror-resilience-test.sh holds the plist's ProgramArguments and the agent's one vault read.)
#
#   scripts/local-mirror/service-test.sh
set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/shell-spec.sh
. "$HERE/../shell-spec.sh"

printf '\033[36m▸\033[0m local-mirror/service.sh\n'
work="$(cd "$(mktemp -d "${TMPDIR:-/tmp}/kinowo-service-test.XXXXXX")" && pwd)" # resolved: install writes resolved dirs
trap 'rm -rf "$work"' EXIT
bin="$work/bin" home="$work/home"
mkdir -p "$bin" "$home"

# Every tool install resolves, faked (`bash` among them: the fakes below name /bin/bash, never `env bash`), so the PATH it writes is this directory's alone; launchctl records its calls
# and answers `print` from $LAUNCHCTL_PRINT (absent: "not loaded", exit 113 as launchd does).
for t in bash ssh brew mongosh nc secrets; do printf '#!/bin/sh\nexit 0\n' > "$bin/$t"; done
cat > "$bin/launchctl" <<'FAKE'
#!/bin/bash
echo "launchctl $*" >> "$CALLS"
if [ "$1" = print ]; then [ -n "${LAUNCHCTL_PRINT:-}" ] || exit 113; printf '%b' "$LAUNCHCTL_PRINT"; fi
FAKE
chmod +x "$bin"/*
export CALLS="$work/calls"
service() { HOME="$home" PATH="$bin:/usr/bin:/bin" "$BASH" "$HERE/service.sh" "$@"; }
plist="$home/Library/LaunchAgents/pl.kinowo.local-mirror.plist"
domain="gui/$(id -u)"
value_after() { tr -d '\n' < "$plist" | sed -E "s|.*<key>$1</key> *<([a-z]+)>([^<]*)</[a-z]+>.*|\\2|; s|.*<key>$1</key> *<([a-z]+)/>.*|\\1|"; }

# ── install ────────────────────────────────────────────────────────────────────────────────────────
: > "$CALLS"
service install > "$work/out" 2>&1
check "install succeeds" "0" "$?"
check "...the agent's PATH: where each tool resolved, de-duplicated, then /usr/bin:/bin" "$bin:/usr/bin:/bin" \
  "$(tr -d '\n' < "$plist" | sed -E 's|.*<key>PATH</key><string>([^<]*)</string>.*|\1|')"
check "...restarted by launchd (KeepAlive) and started at login (RunAtLoad)" "true true" \
  "$(value_after KeepAlive) $(value_after RunAtLoad)"
check "...logging where mirror.sh rotates (log-path.sh)" "$home/Library/Logs/kinowo-local-mirror.log" "$(value_after StandardOutPath)"
check "...run from the checkout's root" "$(cd "$HERE/../.." && pwd)" "$(value_after WorkingDirectory)"
check "...an earlier copy booted out, then this one bootstrapped and enabled" \
  "launchctl bootout $domain/pl.kinowo.local-mirror|launchctl bootstrap $domain $plist|launchctl enable $domain/pl.kinowo.local-mirror" \
  "$(paste -sd'|' - < "$CALLS")"
rm "$bin/secrets"
service install > "$work/out" 2>&1
code=$?
check "a tool it cannot find is warned about, and install still goes on" "1 0" \
  "$(grep -c "WARN: 'secrets' not found" "$work/out") $code"
printf '#!/bin/sh\nexit 0\n' > "$bin/secrets"; chmod +x "$bin/secrets"

# ── status / uninstall / anything else ─────────────────────────────────────────────────────────────
check "status shows the loaded agent's state and pid, nothing else" "state = running|pid = 4242" \
  "$(LAUNCHCTL_PRINT='gui/501/pl.kinowo.local-mirror = {\n\tstate = running\n\tpid = 4242\n\tprogram = /bin/bash\n}\n' service status | sed 's/^[[:space:]]*//' | paste -sd'|' -)"
check "...and says so when it is not loaded" "[service] not loaded" "$(service status)"
: > "$CALLS"
service uninstall > /dev/null 2>&1
check "uninstall boots the agent out and removes its plist" "launchctl bootout $domain/pl.kinowo.local-mirror absent" \
  "$(cat "$CALLS") $([ -e "$plist" ] && echo present || echo absent)"
check "an unknown command prints the usage and exits 1" "1 1" \
  "$(service restart 2> "$work/err" > /dev/null; echo "$? $(grep -c '^usage:' "$work/err")")"
check "...as does none" "1" "$(service > /dev/null 2>&1; echo $?)"

# ── agent ──────────────────────────────────────────────────────────────────────────────────────────
# The vault says no (Touch ID declined): wait ten minutes, then exit for launchd to try again — not re-prompt at once.
printf '#!/bin/sh\nexit 1\n' > "$bin/secrets"
# shellcheck disable=SC2016 # expanded by the fake, not here
printf '#!/bin/sh\necho "sleep $*" >> "$CALLS"\n' > "$bin/sleep"
chmod +x "$bin/secrets" "$bin/sleep"
: > "$CALLS"
env -u MONGODB_URI HOME="$home" PATH="$bin:/usr/bin:/bin" "$BASH" "$HERE/service.sh" agent > /dev/null 2>&1
check "a vault that says no is asked again in ten minutes, by launchd (exit 1)" "1 sleep 600" "$? $(cat "$CALLS")"

# With the secret, mirror.sh is restarted each time it exits, the variable inherited; a TERM ends the agent cleanly.
agent="$work/agent"
mkdir -p "$agent"
cp "$HERE/service.sh" "$HERE/log-path.sh" "$agent/"
cat > "$agent/mirror.sh" <<'FAKE'
#!/bin/bash
echo "mirror.sh with $MONGODB_URI" >> "$CALLS"
FAKE
# the second restart's pause stops the agent the way launchd does
cat > "$bin/sleep" <<'FAKE'
#!/bin/bash
echo "sleep $*" >> "$CALLS"
[ "$(grep -c '^sleep' "$CALLS")" -lt 2 ] || kill -TERM "$PPID"
FAKE
chmod +x "$bin/sleep"
: > "$CALLS"
HOME="$home" PATH="$bin:/usr/bin:/bin" MONGODB_URI="mongodb://secret" "$BASH" "$agent/service.sh" agent > /dev/null 2>&1 &
pid=$! deadline=$((SECONDS + 20))
while kill -0 "$pid" 2>/dev/null && [ "$SECONDS" -lt "$deadline" ]; do command sleep 0.1; done
kill "$pid" 2>/dev/null; wait "$pid"; code=$?
check "the agent restarts mirror.sh after each exit, 10s apart, MONGODB_URI inherited" \
  "mirror.sh with mongodb://secret|sleep 10|mirror.sh with mongodb://secret|sleep 10" "$(paste -sd'|' - < "$CALLS")"
check "...and a TERM ends it cleanly (exit 0)" "0" "$code"

spec_summary
