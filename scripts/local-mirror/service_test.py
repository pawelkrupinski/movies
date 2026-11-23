#!/usr/bin/env python3
# script-test-reads: scripts/local-mirror/service* scripts/local-mirror/log-path.sh
"""scripts/local-mirror/service.sh (the launchd wrapper around mirror.sh), black-box: the plist `install` writes (the
PATH it resolves, KeepAlive, the shared log, what launchd runs), the launchctl calls of install / uninstall / status,
an unknown command, the agent's wait when the vault says no, and the agent restarting mirror.sh with the secret
inherited. No launchd, no vault: fake `launchctl`, `secrets` and `sleep` first on the PATH, HOME a scratch directory,
and for the agent loop a copy of the tool beside a fake mirror.sh. Ported from service-test.sh case for case; the same
cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import re
import shutil
import sys
import unittest
from typing import Dict, List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/local-mirror/service.py", "scripts/local-mirror/service.sh")
HERE = os.path.join(cli_harness.ROOT, "scripts", "local-mirror")
LABEL = "pl.kinowo.local-mirror"
DOMAIN = f"gui/{os.getuid()}"
RESOLVED = ("ssh", "brew", "mongosh", "node", "nc", "secrets")
# launchctl answers `print` from $LAUNCHCTL_PRINT (absent: "not loaded", exit 113 as launchd does)
LAUNCHCTL = '''
if argv[:1] == ["print"]:
    if not env.get("LAUNCHCTL_PRINT"):
        exit(113)
    out(env["LAUNCHCTL_PRINT"], end="")
'''
# appends its call to $CALLS, the one ordered log the agent loop's mirror.sh shares
SLEEP_LOGGED = 'echo "sleep $*" >> "$CALLS"'


class ServiceTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.home = self.path("home")
        os.makedirs(self.home)
        self.plist = os.path.join(self.home, "Library", "LaunchAgents", f"{LABEL}.plist")
        self.ordered = self.path("calls")
        open(self.ordered, "w").close()
        # Every tool install resolves, faked, so the PATH it writes is the fakes' directory alone; bash runs the real
        # one (the tool itself may be a bash script run through env)
        self.fakes.script("bash", 'exec /bin/bash "$@"')
        for name in RESOLVED:
            self.fakes.script(name, "exit 0")
        self.fakes.program("launchctl", LAUNCHCTL)

    def service(self, *args: str, env: Optional[Dict[str, Optional[str]]] = None,
                tool: Optional[str] = None) -> cli_harness.Done:
        return self.run_tool(*args, tool=tool, env={"HOME": self.home, "CALLS": self.ordered, "MONGODB_URI": None,
                                                    **(env or {})})

    def plist_text(self) -> str:
        with open(self.plist) as f:
            return re.sub(r"\s*\n\s*", "", f.read())

    def value_after(self, key: str) -> str:
        found = re.search(rf"<key>{key}</key>\s*<([a-z]+)(?:>([^<]*)</[a-z]+>|/>)", self.plist_text())
        assert found, f"no {key} in the plist"
        return found.group(2) if found.group(2) is not None else found.group(1)

    def launchctl_calls(self) -> List[str]:
        return [" ".join(argv) for argv in self.fakes.calls("launchctl")]

    def ordered_calls(self) -> List[str]:
        with open(self.ordered) as f:
            return f.read().splitlines()

    # ── install ──────────────────────────────────────────────────────────────────────────────────────

    def test_install_writes_the_agent_and_loads_it(self) -> None:
        done = self.service("install")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(f"{self.fakes.bin}:/usr/bin:/bin", self.value_after("PATH"),
                         "the agent's PATH: where each tool resolved, de-duplicated, then /usr/bin:/bin")
        self.assertEqual(("true", "true"), (self.value_after("KeepAlive"), self.value_after("RunAtLoad")),
                         "restarted by launchd (KeepAlive) and started at login (RunAtLoad)")
        log = os.path.join(self.home, "Library", "Logs", "kinowo-local-mirror.log")
        self.assertEqual(log, self.value_after("StandardOutPath"), "logging where mirror.sh rotates (log-path.sh)")
        self.assertEqual(log, self.value_after("StandardErrorPath"))
        self.assertTrue(os.path.isdir(os.path.dirname(log)), "the log's directory made")
        # Under launchd there is no terminal, and mirror.sh deliberately EXITS to be restarted: were each restart to go
        # back to the vault, each would raise a fresh Touch ID dialog, so launchd runs `service.sh agent`, which asks
        # the vault once.
        arguments = re.search(r"<key>ProgramArguments</key><array>(.*)</array><key>WorkingDirectory",
                              self.plist_text())
        assert arguments
        self.assertEqual(f"<string>/bin/bash</string><string>{HERE}/service.sh</string><string>agent</string>",
                         arguments.group(1), "running `service.sh agent` under /bin/bash")
        self.assertEqual(cli_harness.ROOT, self.value_after("WorkingDirectory"), "run from the checkout's root")
        self.assertEqual(f"{LABEL}", self.value_after("Label"))
        self.assertEqual([f"bootout {DOMAIN}/{LABEL}", f"bootstrap {DOMAIN} {self.plist}", f"enable {DOMAIN}/{LABEL}"],
                         self.launchctl_calls(), "an earlier copy booted out, then this one bootstrapped and enabled")
        self.assertIn(f"[service] installed + started: {LABEL}", done.stdout)
        self.assertIn(f"[service] plist: {self.plist}", done.stdout)

    def test_a_tool_it_cannot_find_is_warned_about_and_install_goes_on(self) -> None:
        os.remove(os.path.join(self.fakes.bin, "secrets"))
        done = self.service("install", env={"PATH": f"{self.fakes.bin}:/usr/bin:/bin"})
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(1, done.stderr.count("WARN: 'secrets' not found"), done.output)

    def test_a_failing_bootstrap_fails_the_install(self) -> None:
        self.fakes.program("launchctl", 'exit(5 if argv[:1] == ["bootstrap"] else 0)')
        done = self.service("install")
        self.assertEqual(5, done.code, done.output)
        self.assertNotIn("installed + started", done.stdout)

    # ── status / uninstall / anything else ───────────────────────────────────────────────────────────

    def test_status_shows_the_loaded_agents_state_and_pid_nothing_else(self) -> None:
        printed = f"{DOMAIN}/{LABEL} = {{\n\tstate = running\n\tpid = 4242\n\tprogram = /bin/bash\n}}\n"
        done = self.service("status", env={"LAUNCHCTL_PRINT": printed})
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(["state = running", "pid = 4242"], [line.strip() for line in done.stdout.splitlines()])
        self.assertEqual([f"print {DOMAIN}/{LABEL}"], self.launchctl_calls())

    def test_status_says_so_when_it_is_not_loaded(self) -> None:
        done = self.service("status")
        self.assertEqual((0, "[service] not loaded\n"), (done.code, done.stdout), done.output)

    def test_uninstall_boots_the_agent_out_and_removes_its_plist(self) -> None:
        self.assertEqual(0, self.service("install").code)
        self.fakes.clear()
        done = self.service("uninstall")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([f"bootout {DOMAIN}/{LABEL}"], self.launchctl_calls())
        self.assertFalse(os.path.exists(self.plist))
        self.assertIn(f"[service] uninstalled: {LABEL}", done.stdout)

    def test_uninstall_without_an_agent_still_succeeds(self) -> None:
        self.fakes.program("launchctl", "exit(3)")
        done = self.service("uninstall")
        self.assertEqual(0, done.code, done.output)

    def test_an_unknown_command_prints_the_usage_and_exits_1(self) -> None:
        done = self.service("restart")
        self.assertEqual((1, 1), (done.code, len(re.findall(r"(?m)^usage:", done.stderr))), done.output)
        self.assertEqual("", done.stdout)

    def test_no_command_prints_the_usage_and_exits_1(self) -> None:
        done = self.service()
        self.assertEqual((1, 1), (done.code, len(re.findall(r"(?m)^usage:", done.stderr))), done.output)

    # ── agent ────────────────────────────────────────────────────────────────────────────────────────

    def test_the_agent_asks_the_vault_once_rerunning_itself_under_it(self) -> None:
        # No MONGODB_URI: the agent re-runs ITSELF under the vault, once, for that key alone.
        done = self.service("agent")
        self.assertEqual(0, done.code, done.output)
        calls = self.fakes.calls("secrets")
        self.assertEqual(1, len(calls), calls)
        self.assertEqual(["run", "movies", "MONGODB_URI", "--"], calls[0][:4])
        self.assertEqual("agent", calls[0][-1])
        self.assertIn(os.path.basename(calls[0][-2]), ("service.sh", "service.py"), calls[0])
        self.assertEqual([], self.fakes.calls("sleep"))

    def test_a_vault_that_says_no_is_asked_again_in_ten_minutes_by_launchd(self) -> None:
        # The vault says no (Touch ID declined): wait ten minutes, then exit for launchd to try again, not re-prompt.
        self.fakes.script("secrets", "exit 1")
        self.fakes.script("sleep", "exit 0")
        done = self.service("agent")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual([["600"]], self.fakes.calls("sleep"))
        self.assertIn("could not read MONGODB_URI from the Touch ID vault", done.stderr)

    def test_the_agent_restarts_mirror_after_each_exit_and_a_term_ends_it(self) -> None:
        # With the secret, mirror.sh is restarted each time it exits, the variable inherited; a TERM ends the agent
        # cleanly. The second restart's pause stops the agent the way launchd does.
        agent = self.path("agent")
        os.makedirs(agent)
        for name in os.listdir(HERE):
            if name.startswith("service") and not name.startswith("service_test") and "-test" not in name:
                shutil.copy2(os.path.join(HERE, name), agent)
        shutil.copy2(os.path.join(HERE, "log-path.sh"), agent)
        mirror = self.write("agent/mirror.sh", '#!/bin/sh\necho "mirror.sh with $MONGODB_URI" >> "$CALLS"\n')
        os.chmod(mirror, 0o755)
        self.fakes.script("sleep", SLEEP_LOGGED + '\n[ "$(grep -c "^sleep" "$CALLS")" -lt 2 ] || kill -TERM "$PPID"')
        done = self.service("agent", tool=os.path.join(agent, "service.sh"),
                            env={"MONGODB_URI": "mongodb://secret"})
        self.assertEqual(["mirror.sh with mongodb://secret", "sleep 10"] * 2, self.ordered_calls(),
                         "the agent restarts mirror.sh after each exit, 10s apart, MONGODB_URI inherited")
        self.assertEqual(0, done.code, done.output)
        self.assertRegex(done.stdout, r"(?m)^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}[+-]\d{4} \[service\] mirror\.sh exited"
                                      r" — restarting it in 10s$")


if __name__ == "__main__":
    unittest.main()
