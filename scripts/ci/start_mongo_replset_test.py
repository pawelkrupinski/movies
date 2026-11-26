#!/usr/bin/env python3
"""scripts/ci/start-mongo-replset.sh, black-box, against a stub `docker` whose mongod comes up (or never does) on cue,
and a job image's native mongod and mongosh (stubs too); with mongo-exec.sh, which runs a command beside what it
started. Ported from start-mongo-replset-test.sh; the same cases hold the shell tool and its Python rewrite
(cli_harness.tool_path)."""
from __future__ import annotations

import os
import re
import subprocess
import sys
import unittest
from typing import List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/start_mongo_replset.py", "scripts/ci/start-mongo-replset.sh")
MONGO_EXEC = os.path.join(cli_harness.ROOT, "scripts/ci/mongo-exec.sh")
PULL = os.path.join(cli_harness.ROOT, "scripts/ci/pull-pinned-image.sh")

# STUB_MONGO: `healthy` answers ping and reports PRIMARY after rs.initiate; `down` never answers a ping (and its
# container cannot be inspected); `exited` is a container whose mongod died at start, its log's fatal line saying why;
# `secondary` answers but never becomes PRIMARY. A pull succeeds and `image inspect` then names the pulled ref (its
# digest with it); where the pull's sources fall back is scripts/ci/pull_pinned_image_test.py's.
DOCKER = r"""#!/usr/bin/env bash
printf '%s\n' "$*" >> "$STUB_LOG"
case "$1" in
  run) exit 0 ;;
  logs) [ "$STUB_MONGO" = exited ] && printf '%s\n' '{"t":{},"s":"I","msg":"starting"}' \
          '{"t":{},"s":"F","c":"CONTROL","msg":"MongoDB cannot start: Linux kernel versions 6.19 and newer has a known incompatibility"}'
        exit 0 ;;
  inspect) case "$STUB_MONGO" in exited) echo "false 14" ;; healthy|secondary) echo "true 0" ;; *) exit 1 ;; esac; exit 0 ;;
  pull) echo "${*: -1}" >> "$STUB_LOG.pulled"; exit 0 ;;
  image) grep -qxF -- "${*: -1}" "$STUB_LOG.pulled" 2>/dev/null && echo "${*: -1}"; exit 0 ;;
  info) exit 0 ;;
esac
eval_arg="${*: -1}"
case "$STUB_MONGO:$eval_arg" in
  down:*) exit 1 ;;
  *:"rs.status().myState") [ "$STUB_MONGO" = healthy ] && echo 1 || echo 2 ;;
esac
exit 0
"""
# The job image's own mongod and mongosh, answering as the container's do; `sudo` runs its command, logging it.
MONGOD = r"""#!/usr/bin/env bash
[ "$1" = --version ] && { echo "db version v8.3.11"; exit 0; }
printf 'mongod %s\n' "$*" >> "$STUB_LOG"
[ "$STUB_MONGO" = down ] && echo "mongod: the log says why" >> "$MONGO_NATIVE_LOG"
exit 0
"""
MONGOSH = r"""#!/usr/bin/env bash
printf 'mongosh HOME=%s %s\n' "$HOME" "$*" >> "$STUB_LOG"
eval_arg="${*: -1}"
case "$STUB_MONGO:$eval_arg" in
  down:*) exit 1 ;;
  *:"rs.status().myState") [ "$STUB_MONGO" = healthy ] && echo 1 || echo 2 ;;
esac
exit 0
"""
SUDO = r"""#!/usr/bin/env bash
printf 'sudo %s\n' "$*" >> "$STUB_LOG"
[ "$1" = mount ] && exit "${STUB_MOUNT:-0}"
exec "$@"
"""


def pinned() -> str:
    """The pin's GHCR mirror by digest: what the tool pulls and runs."""
    def field(name: str) -> str:
        return subprocess.run(["bash", PULL, "--pin", "mongo", name], check=True, capture_output=True,
                              text=True).stdout.strip()
    return f"{field('mirror')}@{field('digest')}"


class StartMongoReplsetTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.executable(os.path.join(self.fakes.bin, "docker"), DOCKER)
        self.executable(os.path.join(self.fakes.bin, "sudo"), SUDO)
        self.native = self.path("native")
        self.executable(os.path.join(self.native, "bin", "mongod"), MONGOD)
        self.executable(os.path.join(self.native, "bin", "mongosh"), MONGOSH)
        self.log = self.path("log")
        self.out = ""

    def executable(self, path: str, text: str) -> None:
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "w") as f:
            f.write(text)
        os.chmod(path, 0o755)

    def base_env(self, state: str) -> dict[str, Optional[str]]:
        return {"STUB_LOG": self.log, "STUB_MONGO": state, "MONGO_NATIVE_MARKER": self.path("marker"),
                "MONGO_NATIVE_LOG": self.path("mongod.log"), "MONGO_NATIVE_DBPATH": self.path("db")}

    def start(self, state: str, *args: str, native: bool = False, **env: str) -> int:
        """The tool with the stub in `state`; its exit status. Output in self.out, the stubs' calls in self.log."""
        self.write("log", "")
        self.write("log.pulled", "")
        extra = {**self.base_env(state), "MONGO_START_TIMEOUT_SECONDS": "2",
                 "MONGO_NATIVE_HOME": self.native if native else self.path("no-native"), **env}
        done = self.run_tool(*args, env=extra)
        self.out = done.output
        return done.code

    def mongo_exec(self, marker: str, *args: str) -> int:
        return self.run_tool(*args, tool=MONGO_EXEC,
                             env={**self.base_env("healthy"), "MONGO_NATIVE_MARKER": self.path(marker)}).code

    def logged(self) -> List[str]:
        return self.read("log").splitlines()

    def count(self, pattern: str, lines: Optional[List[str]] = None) -> int:
        return sum(1 for line in (self.logged() if lines is None else lines) if re.search(pattern, line))

    def said(self, pattern: str) -> int:
        return self.count(pattern, self.out.splitlines())

    # the container

    def test_a_healthy_mongod_becomes_primary_having_initiated_the_set_once(self) -> None:
        self.assertEqual(0, self.start("healthy"), self.out)
        self.assertEqual(1, self.count("rs.initiate"))
        self.assertEqual(2, self.said(r"^MongoDB (reachable|PRIMARY) after [0-9]*s"), self.out)

    def test_extra_mongod_args_reach_docker_run(self) -> None:
        self.start("healthy", "--wiredTigerCacheSizeGB", "2")
        self.assertEqual(1, self.count(r"^run .*--replSet rs0 --wiredTigerCacheSizeGB 2$"))

    def test_majority_writes_wait_for_the_journal_unless_asked_not_to(self) -> None:
        self.start("healthy")
        self.assertEqual(1, self.count("writeConcernMajorityJournalDefault:true"))
        self.start("healthy", MONGO_MAJORITY_JOURNAL="false")
        self.assertEqual(1, self.count("writeConcernMajorityJournalDefault:false"))

    def test_mongod_runs_the_pinned_image_on_the_hosts_network_pulled_from_ghcr_first(self) -> None:
        image = pinned()
        self.start("healthy")
        self.assertIn(f"run -d --name mongo --network host {image} --replSet rs0", self.logged())
        self.assertEqual(0, self.count(" -p "))
        self.assertEqual(f"pull -q {image}", [line for line in self.logged() if line.startswith("pull")][0])

    def test_an_image_the_job_already_pulled_runs_as_it_is(self) -> None:
        self.assertEqual(0, self.start("healthy", PINNED_IMAGE="pulled@sha256:x"), self.out)
        self.assertEqual(0, self.count("^pull"))
        self.assertEqual(["run -d --name mongo --network host pulled@sha256:x --replSet rs0"],
                         [line for line in self.logged() if line.startswith("run")])

    def test_the_data_directory_is_on_disk_unless_a_tmpfs_is_asked_for(self) -> None:
        self.start("healthy")
        self.assertEqual(0, self.count("--tmpfs"))
        self.start("healthy", MONGO_TMPFS="6g")
        self.assertEqual(1, self.count("^run -d --name mongo --network host --tmpfs /data/db:rw,size=6g ghcr.io/"))

    def test_every_probe_keeps_mongoshs_state_out_of_the_data_directory(self) -> None:
        self.start("healthy")
        probes = [line for line in self.logged() if line.startswith("exec ")]
        self.assertTrue(probes)
        self.assertEqual([], [line for line in probes if not line.startswith("exec -e HOME=/tmp mongo mongosh ")])

    def test_a_mongod_that_never_answers_fails_bounded_naming_its_logs_last_word(self) -> None:
        self.assertEqual(1, self.start("down"))
        self.assertEqual(1, self.said("did not become reachable"))
        self.assertEqual(1, self.said("^::error::MongoDB did not become reachable within 2s: "), self.out)

    # Convergence #523 (run 41199223125): every leg waited out its 180 s for a mongod that died in its first second.
    def test_a_container_whose_mongod_exited_fails_at_once_naming_mongods_fatal_line(self) -> None:
        self.assertEqual(1, self.start("exited", MONGO_START_TIMEOUT_SECONDS="60"))
        self.assertEqual(1, self.count(r"ping:1"))
        self.assertEqual(1, self.said(r'^::error::MongoDB.s container exited \(status 14\) .*"msg":"MongoDB cannot '
                                      r"start: Linux kernel versions 6\.19"), self.out)

    def test_a_mongod_that_never_becomes_primary_fails_naming_what_it_waited_for(self) -> None:
        self.assertEqual(1, self.start("secondary"))
        self.assertEqual(1, self.said("did not become PRIMARY"))

    # NATIVE: the job image's own mongod and mongosh, started on the job's localhost with the container's flags

    def test_a_job_image_that_bakes_mongod_starts_it_natively_with_the_containers_flags(self) -> None:
        self.assertEqual(0, self.start("healthy", "--wiredTigerCacheSizeGB", "2", native=True), self.out)
        db, log = self.path("db"), self.path("mongod.log")
        self.assertEqual(1, self.count(f"^mongod --replSet rs0 --bind_ip 127.0.0.1 --port 27017 --dbpath {re.escape(db)} "
                                       f"--fork --logpath {re.escape(log)} --wiredTigerCacheSizeGB 2$"))
        self.assertEqual(0, self.count("^(pull|run|info) "))
        self.assertEqual(1, self.count("^mongosh HOME=/tmp --quiet --eval rs.initiate"))
        self.assertEqual(self.native + "\n", self.read("marker"))

    def test_mongo_exec_runs_beside_a_native_mongod_else_in_the_container(self) -> None:
        self.write("marker", self.native + "\n")
        self.write("log", "")
        self.assertEqual(0, self.mongo_exec("marker", "mongosh", "--quiet", "--eval", "x"))
        self.assertEqual(["mongosh HOME=/tmp --quiet --eval x"], self.logged())
        self.assertEqual(0, self.mongo_exec("marker", "-d", "true"))
        self.write("log", "")
        self.assertEqual(0, self.mongo_exec("none", "-d", "du", "-sh", "/data/db"))
        self.assertEqual(["exec -d -e HOME=/tmp mongo du -sh /data/db"], self.logged())

    def test_the_native_data_directory_on_a_tmpfs_of_the_size_asked_for(self) -> None:
        self.start("healthy", native=True, MONGO_TMPFS="6g")
        uid, gid = os.getuid(), os.getgid()
        self.assertEqual(1, self.count(f"^sudo mount -t tmpfs -o size=6g,uid={uid},gid={gid},mode=0700 tmpfs "
                                       f"{re.escape(self.path('db'))}$"))

    def test_a_tmpfs_that_will_not_mount_leaves_the_data_on_disk_and_says_so(self) -> None:
        self.assertEqual(0, self.start("healthy", native=True, STUB_MOUNT="1", MONGO_TMPFS="6g"), self.out)
        self.assertEqual(1, self.said("could not mount a 6g tmpfs"))

    def test_a_native_mongod_that_never_answers_fails_bounded_with_its_logs_tail(self) -> None:
        self.assertEqual(1, self.start("down", native=True))
        self.assertEqual(1, self.said("^mongod: the log says why"), self.out)

    def test_mongo_no_native_runs_the_container_regardless_leaving_no_marker(self) -> None:
        self.start("healthy", native=True, MONGO_NO_NATIVE="1")
        self.assertEqual(1, self.count("^run -d --name mongo "))
        self.assertFalse(os.path.exists(self.path("marker")))


class StarterLogTest(unittest.TestCase):
    """With no MONGO_NATIVE_LOG the log is made per run under a private (0700) directory, never a fixed /tmp name."""

    def starter(self) -> object:
        sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
        import start_mongo_replset  # noqa: E402
        env = {"MONGO_NATIVE_HOME": "/nonexistent", "PATH": os.environ.get("PATH", "")}
        return start_mongo_replset.Starter(env, lambda: 0.0, lambda _seconds: None)

    def test_the_default_log_is_in_a_private_directory_made_per_run(self) -> None:
        first, second = self.starter().log, self.starter().log  # type: ignore[attr-defined]
        self.addCleanup(lambda: [os.rmdir(os.path.dirname(p)) for p in (first, second)])
        self.assertNotEqual("/tmp/mongod.log", first)
        self.assertNotEqual(os.path.dirname(first), os.path.dirname(second))
        self.assertEqual(0o700, os.stat(os.path.dirname(first)).st_mode & 0o777)

if __name__ == "__main__":
    unittest.main()
