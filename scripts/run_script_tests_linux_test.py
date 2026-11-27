"""scripts/run-script-tests --linux, black-box (ported from run-script-tests-test.sh): the selection made here, the tests
run in an ubuntu container (a stub `docker` notes what it was asked) over a copy of the checkout in the first free of a
few fixed slots, mounted read-only at the checkout's path, as this user; its git metadata a snapshot unpacked into a
tmpfs, only the objects (and those it borrows) mounted live; the image built once, from Docker Hub when the mirror lost
a blob; and with no names the tests --changed origin/main selects.
Run: python3 scripts/run_script_tests_linux_test.py"""
from __future__ import annotations

import fcntl
import os
import re
import tarfile
import unittest
from typing import Dict, List, Optional

from run_script_tests_fixture import ROOT, ScriptTestsCase
import cli_harness

DOCKER = r'''#!/usr/bin/env bash
echo "$*" >> "$DOCKER_CALLS"
case "$1" in
  info) [ -z "${DOCKER_DOWN:-}" ] || { echo "Cannot connect to the Docker daemon" >&2; exit 1; } ;;
  image) [ -z "${NO_IMAGE:-}" ] || exit 1 ;;
  build) from=$(head -1); cat > /dev/null; echo "$from" >> "$DOCKER_CALLS"
    case "$from" in *mirror.gcr.io*) [ -z "${MIRROR_LOST:-}" ] || { echo "ERROR: $MIRROR_LOST" >&2; exit 1; } ;; esac ;;
  run) cat > "$DOCKER_STDIN"; echo "PASS       0s  in the container" ;;
esac
'''
LOST = ("failed to copy: could not fetch content descriptor sha256:edaf (application/vnd.oci.image.layer.v1.tar+gzip) "
        "from remote: not found")


class LinuxTest(ScriptTestsCase):
    def setUp(self) -> None:
        super().setUp()
        self.docker = self.write("docker-bin/docker", DOCKER)
        os.chmod(self.docker, 0o755)
        self.slots = self.path("home/.cache/movies-linux-tree")
        self.git_dir = os.path.realpath(os.path.join(self.tree, ".git"))

    def linux(self, *args: str, root: Optional[str] = None, env: Optional[Dict[str, str]] = None) -> cli_harness.Done:
        self.write("docker-calls", "")
        return self.runner("--linux", *args, root=root, env={
            "HOME": self.path("home"), "PATH": self.path("docker-bin") + os.pathsep + self.env()["PATH"],
            "DOCKER_CALLS": self.path("docker-calls"), "DOCKER_STDIN": self.path("docker-stdin"), **(env or {})})

    def calls(self, pattern: str) -> int:
        return len(re.findall(pattern, self.read("docker-calls"), re.M))

    def snapshot(self) -> List[str]:
        with tarfile.open(self.path("docker-stdin")) as tar:
            return tar.getnames()

    def test_the_selected_tests_run_in_a_container_over_a_slot_copy(self) -> None:
        done = self.linux("scripts/ci/b_test.py")
        self.assertEqual(0, done.code, done.output)
        slot0 = re.escape(f"-v {self.slots}/slot-0:{self.tree}:ro")
        self.assertEqual((1, 0), (self.calls(f"^run .*{slot0}"), self.calls(re.escape(f"-v {self.tree}:"))))
        self.assertEqual(1, self.calls(re.escape(f"--user {os.getuid()}:{os.getgid()}")))
        with open(f"{self.slots}/slot-0/scripts/a.sh") as f:
            self.assertEqual("echo tool a", f.readline().strip())
        self.assertTrue(os.path.isdir(f"{self.slots}/slot-0/.git"))
        self.assertEqual(1, self.calls(r"run-script-tests --jobs 8 --timeout 0 --stall 300 scripts/ci/b_test\.py$"))
        self.assertEqual(1, min(1, self.calls("kinowo-script-tests:")))
        git = re.escape(self.git_dir)
        self.assertEqual((1, 1, 0), (self.calls(f"^run .*--mount type=tmpfs,destination={git},tmpfs-mode=1777"),
                                     self.calls(f"^run .*-v {git}/objects:{git}/objects:ro"),
                                     self.calls(f"-v {git}:")))
        self.assertEqual(1, self.calls(r'^run .* bash -c .*tar -xf - -C "\$0".* %s .*scripts/run-script-tests ' % git))
        names = self.snapshot()
        self.assertEqual((True, True, True, []), ("config" in names, "HEAD" in names, "refs/heads/main" in names,
                                                  [n for n in names if n.startswith("objects")]))

    def test_a_worktree_takes_the_same_slot_a_held_slot_is_passed_over(self) -> None:
        self.put("main-only.txt", "scratch")
        self.linux("scripts/ci/b_test.py")
        worktree = self.path("wt")
        self.git(self.tree, "worktree", "add", "-q", worktree, "-b", "wt")
        self.linux("scripts/ci/b_test.py", root=worktree)
        self.assertEqual(1, self.calls(re.escape(f"-v {self.slots}/slot-0:{worktree}:ro")))
        self.assertTrue(os.path.isfile(f"{self.slots}/slot-0/.git"))
        self.assertFalse(os.path.exists(f"{self.slots}/slot-0/main-only.txt"))
        with open(f"{self.slots}/slot-0.lock", "w") as held:
            fcntl.flock(held, fcntl.LOCK_EX)
            self.linux("scripts/ci/b_test.py", root=worktree)
            self.assertEqual(1, self.calls(re.escape(f"-v {self.slots}/slot-1:{worktree}:ro")))
            done = self.linux("scripts/ci/b_test.py", root=worktree,
                              env={"RUN_SCRIPT_TESTS_LINUX_SLOTS": "1", "RUN_SCRIPT_TESTS_LINUX_WAIT": "0"})
            self.assertEqual(2, done.code, done.output)
            self.assertIn("every Linux tree slot is busy", done.stderr)
            self.assertEqual(0, self.calls("^run "))
        self.linux("scripts/ci/b_test.py", root=worktree)
        self.assertLessEqual({"config", "worktrees/wt/HEAD", "worktrees/wt/commondir", "worktrees/wt/gitdir"},
                             set(self.snapshot()))

    def test_the_objects_it_borrows_are_mounted_too(self) -> None:
        mirror = self.repo("mirror.git", "--bare")
        objects = os.path.realpath(os.path.join(mirror, "objects"))
        with open(os.path.join(self.tree, ".git/objects/info/alternates"), "w") as f:
            f.write(objects + "\n")
        self.linux("scripts/ci/b_test.py")
        self.assertEqual(1, self.calls(re.escape(f"-v {objects}:{objects}:ro")))

    def test_the_image_is_built_once_from_docker_hub_when_the_mirror_lost_a_blob(self) -> None:
        self.linux("scripts/ci", env={"NO_IMAGE": "1"})
        self.assertEqual(1, self.calls("^build "))
        self.linux("scripts/ci", env={"NO_IMAGE": "1", "MIRROR_LOST": LOST})
        with open(os.path.join(ROOT, "scripts/run-script-tests")) as f:
            recipe = re.search(r"FROM mirror\.gcr\.io/(\S+)", f.read())
        self.assertIsNotNone(recipe)
        froms = [line for line in self.read("docker-calls").splitlines() if line.startswith("FROM")]
        self.assertEqual((2, f"FROM docker.io/{recipe.group(1) if recipe else ''}"), (self.calls("^build "), froms[-1]))
        done = self.linux("scripts/ci", env={"NO_IMAGE": "1", "MIRROR_LOST": "no space left on device"})
        self.assertEqual((2, 1), (done.code, self.calls("^build ")))

    def test_with_no_names_the_tests_changed_origin_main_selects_and_no_docker_fails(self) -> None:
        self.git(self.tree, "update-ref", "refs/remotes/origin/main", self.base)
        self.append("scripts/a.sh", "echo edited")
        want = len(self.listed("--changed", "origin/main").split())
        done = self.linux()
        self.assertEqual(0, done.code, done.output)
        self.assertIn(f"run-script-tests: {want} test(s) on Linux", done.stderr)
        done = self.linux("scripts/ci", env={"DOCKER_DOWN": "1"})
        self.assertEqual(2, done.code)
        self.assertIn("--linux needs a running Docker", done.stderr)


if __name__ == "__main__":
    unittest.main()
