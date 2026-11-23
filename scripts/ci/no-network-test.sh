#!/usr/bin/env bash
# no-network: a command run through it reaches loopback and nothing else -- on macOS by sandbox-exec, on Linux by a
# network namespace where the runner allows one, else by proxies to a closed port. The address it tries is TEST-NET-1
# (192.0.2.1, never routed), so the test itself never leaves the machine whatever the mode does.
# Run: bash scripts/ci/no-network-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
printf '\033[36m▸\033[0m no-network\n'
tool="$REPO_ROOT/scripts/ci/no-network"
work="$(mktemp -d)"; trap 'rm -rf "$work"' EXIT

# a loopback server and a fetch of it; then an outside address by a raw socket and by an HTTP client
cat > "$work/probe.py" <<'EOF'
import http.server, socket, sys, threading, urllib.request
srv = http.server.HTTPServer(("127.0.0.1", 0), http.server.SimpleHTTPRequestHandler)
threading.Thread(target=srv.serve_forever, daemon=True).start()
print("loopback", urllib.request.urlopen("http://127.0.0.1:%d/" % srv.server_port, timeout=5).status)
try:
    socket.create_connection(("192.0.2.1", 80), timeout=2); print("socket connected")
except socket.timeout:
    print("socket timeout")
except OSError as e:
    print("socket refused", type(e).__name__)
try:
    urllib.request.urlopen("http://192.0.2.1/", timeout=2); print("http fetched")
except Exception as e:
    print("http refused" if "refused" in str(e).lower() or "not permitted" in str(e).lower() or "unreachable" in str(e).lower() else "http " + str(e))
EOF

check "no command is a usage error" "64" "$("$tool" -- > /dev/null 2>&1; echo $?)"
check "an unknown mode is an error" "64" "$(NO_NETWORK_MODE=nope "$tool" -- true > /dev/null 2>&1; echo $?)"

out="$(NO_NETWORK_MODE=proxy "$tool" -- python3 -I -B "$work/probe.py" 2> "$work/err")"
check "proxy: loopback still answers, an HTTP client's outside fetch is refused at once" "loopback 200|http refused" \
  "$(printf '%s\n' "$out" | grep -e '^loopback' -e '^http' | paste -sd'|' -)"
check "...every JVM is sent to the closed port, loopback exempt" "1" \
  "$(NO_NETWORK_MODE=proxy "$tool" -- sh -c 'echo "$JAVA_TOOL_OPTIONS"' 2> /dev/null | grep -c -- '-Dhttps.proxyHost=127.0.0.1 -Dhttps.proxyPort=9 -Dhttp.nonProxyHosts=localhost|127')"
check "...and it warns that a raw socket is not stopped" "1" "$(grep -c '::warning title=no-network::' "$work/err")"

# THE BUILD TOOLS' HOMES STAY THEIRS (Order independence #113: mapped to root, the JVM's user.home became passwd's
# /root and every sbt launcher failed on /root/.sbt): in whatever mode the runner gives, the command runs as the same
# uid with the same passwd home, and writes its tool homes under $HOME
mkdir -p "$work/home"
outside="$(id -u) $(python3 -I -B -c 'import os, pwd; print(pwd.getpwuid(os.getuid()).pw_dir)')"
inside="$(HOME="$work/home" "$tool" -- sh -c 'mkdir -p "$HOME/.sbt/boot" "$HOME/.gradle" && : > "$HOME/.sbt/boot/sbt.boot.lock" && echo "$(id -u) $(python3 -I -B -c "import os, pwd; print(pwd.getpwuid(os.getuid()).pw_dir)")"' 2> /dev/null)"
check "the same uid and passwd home inside (the JVM's user.home), and \$HOME/.sbt writable ($("$tool" --probe | cut -d' ' -f1))" \
  "$outside|1" "$inside|$([ -f "$work/home/.sbt/boot/sbt.boot.lock" ] && echo 1 || echo 0)"

case "$(uname -s)" in
  Darwin)
    check "probe on macOS: sandbox" "sandbox" "$("$tool" --probe | cut -d' ' -f1)"
    out="$("$tool" -- python3 -I -B "$work/probe.py" 2> /dev/null)"
    check "sandbox: loopback answers; a raw socket and an HTTP client to the outside are both refused at connect" \
      "loopback 200|socket refused PermissionError|http refused" "$(printf '%s\n' "$out" | paste -sd'|' -)"
    check "a forced netns on macOS is an error, never a silent weaker mode" "1" "$(NO_NETWORK_MODE=netns "$tool" -- true > /dev/null 2>&1; echo $?)" ;;
  Linux)
    if [ "$("$tool" --probe | cut -d' ' -f1)" = netns ]; then
      out="$("$tool" -- python3 -I -B "$work/probe.py" 2> /dev/null)"
      check "netns: loopback answers; a raw socket and an HTTP client to the outside are both unreachable" \
        "loopback 200|socket refused OSError|http refused" "$(printf '%s\n' "$out" | paste -sd'|' -)"
    else
      check "probe on a Linux with no user namespaces: proxy" "proxy" "$("$tool" --probe | cut -d' ' -f1)"
    fi
    check "a forced sandbox on Linux is an error" "1" "$(NO_NETWORK_MODE=sandbox "$tool" -- true > /dev/null 2>&1; echo $?)" ;;
esac

spec_summary
