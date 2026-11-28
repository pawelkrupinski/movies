#!/usr/bin/env python3
"""scripts/ci/wait-for-mongo-tunnel.sh, black-box, against stand-ins for everything outside the runner: an `ssh` that
answers the forced command's `credential`, a `socat`, a `setsid` that records the supervisor instead of detaching it,
and a `python3` whose mongo-ping.py answers or not. It refuses without the key or mongo-1's host key, fails on an empty
credential, writes the relay with the forced `connect` verb, and is ready only once the probe answers -- then exports
the URI. Ported from wait-for-mongo-tunnel-test.sh; the same cases hold the shell tool and its Python rewrite
(cli_harness.tool_path)."""
from __future__ import annotations

import os
import stat
import sys
import unittest
from typing import Dict, List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from harness import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/wait_for_mongo_tunnel.py", "scripts/ci/wait-for-mongo-tunnel.sh")
URI = "mongodb://reader:s3cret@127.0.0.1:27999/?authSource=admin&directConnection=true&maxPoolSize=8"

# ssh: `credential` prints $STUB/credential (absent: nothing); anything else is a relay, never run here
SSH = """echo "ssh $*" >> "$STUB/calls"
for last; do :; done
[ "$last" = credential ] && cat "$STUB/credential" 2>/dev/null
exit 0"""
# socat and setsid only record their arguments: the supervisor is never started -- it would loop forever. setsid's record
# is its last write; SETSID_DELAY holds it back, as a loaded runner does (a detached process outliving the tool)
RECORDS = '[ "{name}" = setsid ] && [ -n "${{SETSID_DELAY:-}}" ] && sleep "$SETSID_DELAY"\necho "{name} $*" >> "$STUB/calls"'
# python3: the probe answers when $STUB/answers exists; every other use is the real interpreter (the tool's own too)
PYTHON = """case "${{1:-}}" in
  */mongo-ping.py) echo "probe $*" >> "$STUB/calls"; [ -e "$STUB/answers" ] ;;
  *) exec {python} "$@" ;;
esac"""


class WaitForMongoTunnelTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.fakes.script("ssh", SSH, shell="/usr/bin/env bash")
        for name in ("socat", "setsid"):
            self.fakes.script(name, RECORDS.format(name=name), shell="/usr/bin/env bash")
        # no logging prelude: the fakes' own prelude runs python3, which would be this fake again
        python3 = os.path.join(self.fakes.bin, "python3")
        with open(python3, "w") as f:
            f.write("#!/bin/bash\n" + PYTHON.format(python=sys.executable) + "\n")
        os.chmod(python3, 0o755)
        self.out = ""

    def tunnel(self, case: str, extra: Optional[Dict[str, Optional[str]]] = None) -> int:
        """The tool on port 27999 for `case` (its directory: the stubs' state, RUNNER_TEMP under tmp/); its exit."""
        stub = self.path(case)
        os.makedirs(os.path.join(stub, "tmp"), exist_ok=True)
        env: Dict[str, Optional[str]] = {
            "STUB": stub, "RUNNER_TEMP": os.path.join(stub, "tmp"), "TUNNEL_LOG": os.path.join(stub, "tunnel.log"),
            "GITHUB_ENV": os.path.join(stub, "github-env"), "TUNNEL_ATTEMPTS": "1", "TUNNEL_PROBE_TRIES": "1",
            "MONGO_CI_SSH_KEY": "PRIVATE KEY", "MONGO_CI_SSH_HOST_KEY": "mongo-1 ssh-ed25519 AAAA",
            "MONGO_CI_SSH_TARGET": "mongociread@example.test"}
        env.update(extra or {})
        done = self.run_tool("27999", env=env)
        self.out = done.output
        if "supervisor detached" in self.out:
            # the tool never waits for the supervisor it detaches: wait for its fake's last write, or the case's
            # scratch directory is removed while it writes there (Tool self-tests #637: ENOTEMPTY on .fakes)
            self.assertTrue(cli_harness.wait_until(lambda: any(c.startswith("setsid ") for c in self.calls(case))),
                            self.calls(case))
        return done.code

    def calls(self, case: str) -> List[str]:
        path = self.path(case, "calls")
        return self.read(path).splitlines() if os.path.exists(path) else []

    def ready_case(self, case: str) -> None:
        self.write(f"{case}/credential", "reader:s3cret\n")

    def test_no_key_fails_naming_the_secret_before_any_ssh(self) -> None:
        self.assertEqual(1, self.tunnel("no-key", {"MONGO_CI_SSH_KEY": ""}))
        self.assertIn("MONGO_CI_SSH_KEY is not set", self.out)
        self.assertEqual([], [c for c in self.calls("no-key") if c.startswith("ssh")])

    def test_no_host_key_refuses_to_connect_unverified_before_any_ssh(self) -> None:
        self.assertEqual(1, self.tunnel("no-host-key", {"MONGO_CI_SSH_HOST_KEY": ""}))
        self.assertIn("Refusing to connect unverified", self.out)
        self.assertEqual([], [c for c in self.calls("no-host-key") if c.startswith("ssh")])

    def test_an_empty_credential_fails_and_starts_no_supervisor(self) -> None:
        self.assertEqual(1, self.tunnel("no-credential"))
        self.assertIn("returned no credential", self.out)
        self.assertEqual([], [c for c in self.calls("no-credential") if c.startswith("setsid")])

    def test_the_probe_answering_is_ready_and_exports_the_uri(self) -> None:
        self.ready_case("ready")
        self.write("ready/answers", "")
        self.assertEqual(0, self.tunnel("ready"), self.out)
        self.assertEqual(f"KINOWO_CONVERGENCE_SCRAPES_URI={URI}\n", self.read("ready/github-env"))
        said = self.out.splitlines()
        self.assertEqual(1, said.count("::add-mask::s3cret"), self.out)
        self.assertEqual(1, said.count("::add-mask::reader:s3cret"), self.out)

    def test_the_credential_is_asked_of_the_target_with_the_pinned_host_key_strictly(self) -> None:
        self.ready_case("asked")
        self.write("asked/answers", "")
        self.assertEqual(0, self.tunnel("asked"), self.out)
        asks = [c for c in self.calls("asked") if c.startswith("ssh ")]
        self.assertEqual(1, len(asks), asks)
        self.assertIn("StrictHostKeyChecking=yes", asks[0])
        self.assertTrue(asks[0].endswith("mongociread@example.test credential"), asks[0])

    def test_the_key_is_written_for_ssh_owner_only(self) -> None:
        self.ready_case("key")
        self.write("key/answers", "")
        self.assertEqual(0, self.tunnel("key"), self.out)
        key = self.path("key", "tmp", "mongo-ci-read.key")
        self.assertEqual("PRIVATE KEY\n", self.read(key))
        self.assertEqual(0o600, stat.S_IMODE(os.stat(key).st_mode))

    def test_the_relay_asks_the_forced_command_for_nothing_but_connect_through_the_pinned_key(self) -> None:
        self.ready_case("relay")
        self.write("relay/answers", "")
        self.assertEqual(0, self.tunnel("relay"), self.out)
        relay = self.read(self.path("relay", "tmp", "mongo-relay.sh")).splitlines()
        self.assertEqual("  mongociread@example.test connect", relay[-1])
        key = self.path("relay", "tmp", "mongo-ci-read.key")
        self.assertIn(f"  {key} \\", relay)
        self.assertIn("  BatchMode=yes \\", relay)

    def test_the_supervisor_is_detached_and_listens_on_the_port_a_relay_per_connection(self) -> None:
        self.ready_case("supervised")
        self.write("supervised/answers", "")
        self.assertEqual(0, self.tunnel("supervised"), self.out)
        tmp = self.path("supervised", "tmp")
        self.assertIn(f"setsid nohup {tmp}/mongo-tunnel-supervisor.sh", self.calls("supervised"))
        supervisor = self.read(os.path.join(tmp, "mongo-tunnel-supervisor.sh"))
        self.assertIn(f'socat "TCP4-LISTEN:27999,bind=127.0.0.1,reuseaddr,fork" "EXEC:{tmp}/mongo-relay.sh"',
                      supervisor)

    def test_a_run_ends_only_once_the_supervisor_it_detached_has_done_writing(self) -> None:
        self.ready_case("late")
        self.write("late/answers", "")
        self.assertEqual(0, self.tunnel("late", {"SETSID_DELAY": "1"}), self.out)
        self.assertEqual(1, len([c for c in self.calls("late") if c.startswith("setsid ")]), self.calls("late"))

    def test_the_probe_never_answering_fails_with_the_logs_tail_and_exports_no_uri(self) -> None:
        self.ready_case("silent")
        self.write("silent/tunnel.log", "socat: connection refused\n")
        self.assertEqual(1, self.tunnel("silent"))
        self.assertIn("never answered a handshake on :27999", self.out)
        self.assertIn("socat: connection refused", self.out)
        self.assertFalse(os.path.exists(self.path("silent", "github-env")))


class TunnelLogTest(unittest.TestCase):
    """With no TUNNEL_LOG the log is made per run under a private (0700) directory, never a fixed /tmp name."""

    def tunnel(self) -> object:
        sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__))))
        import wait_for_mongo_tunnel  # noqa: E402
        return wait_for_mongo_tunnel.Tunnel("27999", {"RUNNER_TEMP": "/nonexistent"})

    def test_the_default_log_is_in_a_private_directory_made_per_run(self) -> None:
        first, second = self.tunnel().log, self.tunnel().log  # type: ignore[attr-defined]
        self.addCleanup(lambda: [os.rmdir(os.path.dirname(p)) for p in (first, second)])
        self.assertNotEqual("/tmp/mongo-tunnel.log", first)
        self.assertNotEqual(os.path.dirname(first), os.path.dirname(second))
        self.assertEqual(0o700, os.stat(os.path.dirname(first)).st_mode & 0o777)

if __name__ == "__main__":
    unittest.main()
