#!/usr/bin/env python3
"""scripts/ci/no-network, black-box: a command run through it reaches loopback and nothing else -- on macOS by
sandbox-exec, on Linux by a network namespace where the runner allows one, else by proxies to a closed port. The address
it tries is TEST-NET-1 (192.0.2.1, never routed), so the test itself never leaves the machine whatever the mode does.
Ported from no-network-test.sh; the same cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import platform
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/no_network.py", "scripts/ci/no-network")

PROBE = """\
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
"""
HOME_OF_UID = "import os, pwd; print(pwd.getpwuid(os.getuid()).pw_dir)"


class NoNetworkTest(cli_harness.ToolCase):
    TOOL = TOOL

    def probe_lines(self, *args: str, env=None) -> str:
        self.write("probe.py", PROBE)
        done = self.run_tool(*args, sys.executable, "-I", "-B", self.path("probe.py"), env=env)
        return "|".join(line for line in done.stdout.splitlines() if line.startswith(("loopback", "socket", "http")))

    def mode(self) -> str:
        return self.run_tool("--probe").stdout.split(" ")[0]

    def test_no_command_is_a_usage_error(self) -> None:
        self.assertEqual(64, self.run_tool("--").code)

    def test_an_unknown_mode_is_an_error(self) -> None:
        self.assertEqual(64, self.run_tool("--", "true", env={"NO_NETWORK_MODE": "nope"}).code)

    def test_proxy_mode_keeps_loopback_and_refuses_an_http_client(self) -> None:
        lines = self.probe_lines("--", env={"NO_NETWORK_MODE": "proxy"})
        self.assertEqual("loopback 200|http refused", "|".join(p for p in lines.split("|") if not p.startswith("socket")))

    def test_proxy_mode_sends_every_jvm_to_the_closed_port(self) -> None:
        done = self.run_tool("--", "sh", "-c", 'echo "$JAVA_TOOL_OPTIONS"', env={"NO_NETWORK_MODE": "proxy"})
        self.assertIn("-Dhttps.proxyHost=127.0.0.1 -Dhttps.proxyPort=9 -Dhttp.nonProxyHosts=localhost|127", done.stdout)

    def test_proxy_mode_warns_that_a_raw_socket_is_not_stopped(self) -> None:
        done = self.run_tool("--", "true", env={"NO_NETWORK_MODE": "proxy"})
        self.assertEqual(1, done.stderr.count("::warning title=no-network::"), done.output)

    def test_the_same_uid_and_passwd_home_inside_and_the_tool_home_writable(self) -> None:
        home = self.path("home")
        os.makedirs(home)
        body = ('mkdir -p "$HOME/.sbt/boot" "$HOME/.gradle" && : > "$HOME/.sbt/boot/sbt.boot.lock" && '
                f'echo "$(id -u) $({sys.executable} -I -B -c "{HOME_OF_UID}")"')
        inside = self.run_tool("--", "sh", "-c", body, env={"HOME": home}).stdout.strip()
        import pwd
        self.assertEqual(f"{os.getuid()} {pwd.getpwuid(os.getuid()).pw_dir}", inside)
        self.assertTrue(os.path.isfile(os.path.join(home, ".sbt/boot/sbt.boot.lock")))

    @unittest.skipUnless(platform.system() == "Darwin", "macOS")
    def test_macos_uses_the_sandbox(self) -> None:
        self.assertEqual("sandbox", self.mode())
        self.assertEqual("loopback 200|socket refused PermissionError|http refused", self.probe_lines("--"))
        self.assertEqual(1, self.run_tool("--", "true", env={"NO_NETWORK_MODE": "netns"}).code)

    @unittest.skipUnless(platform.system() == "Linux", "Linux")
    def test_linux_uses_a_namespace_or_proxies(self) -> None:
        if self.mode() == "netns":
            self.assertEqual("loopback 200|socket refused OSError|http refused", self.probe_lines("--"))
        else:
            self.assertEqual("proxy", self.mode())
        self.assertEqual(1, self.run_tool("--", "true", env={"NO_NETWORK_MODE": "sandbox"}).code)


if __name__ == "__main__":
    unittest.main()
