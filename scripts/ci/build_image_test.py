#!/usr/bin/env python3
# script-test-reads: Dockerfile
"""scripts/ci/build-image.sh, black-box, against a stub `docker` whose builds succeed, or fail through the mirror, on
cue. Ported from build-image-test.sh; the same cases hold the shell tool and its Python rewrite
(cli_harness.tool_path)."""
from __future__ import annotations

import os
import re
import shutil
import sys
import unittest
from typing import List

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/build_image.py", "scripts/ci/build-image.sh")
DOCKERFILE = os.path.join(cli_harness.ROOT, "Dockerfile")

# STUB_BUILD: `ok` builds; `mirror-blob` fails every build whose bases come through mirror.gcr.io the way Main #2264's
# did; `mirror-buildkit` fails the same way with BuildKit's "unknown blob"; `broken` fails every build on its own
# Dockerfile; `registry-down` fails the first build as the image registry dropping away mid-build did (Main #2361,
# 2026-11-09 23:33Z: a transfer cut short, then refused), and `registry-pruned` as Main #2662's push into the 04:00
# registry-prune did (a reset, then the proxy's 502) -- each builds the next. A build's -f Dockerfile (else ./Dockerfile)
# is copied to $STUB_DOCKERFILE.
DOCKER = r"""#!/usr/bin/env bash
printf '%s\n' "$*" >> "$STUB_LOG"
dockerfile=Dockerfile
while [ "$#" -gt 0 ]; do [ "$1" = -f ] && dockerfile="$2"; shift; done
cp "$dockerfile" "$STUB_DOCKERFILE"
from_mirror=$(grep -c '^FROM mirror.gcr.io/' "$dockerfile")
case "$STUB_BUILD" in
  ok) exit 0 ;;
  mirror-blob) [ "$from_mirror" -gt 0 ] || exit 0
    echo "ERROR: failed to build: failed to solve: failed to compute cache key: failed to copy: httpReadSeeker: failed open: could not fetch content descriptor sha256:edaf (application/vnd.oci.image.layer.v1.tar+gzip) from remote: not found"
    exit 1 ;;
  mirror-buildkit) [ "$from_mirror" -gt 0 ] || exit 0; echo "ERROR: unknown blob"; exit 1 ;;
  broken) echo "ERROR: failed to solve: process \"/bin/sh -c apt-get install nope\" did not complete successfully: exit code: 100"; exit 1 ;;
  registry-down)
    [ -e "$STUB_LOG.pruned" ] && exit 0
    : > "$STUB_LOG.pruned"
    echo "#13 2.036 error: short read: expected 42139920 bytes but got 13714392: unexpected EOF"
    echo "ERROR: failed to build: failed to solve: failed to compute cache key: failed to copy: httpReadSeeker: failed open: failed to do request: Get \"https://registry.example/v2/movies-web/blobs/sha256:86bc\": dial tcp 2.28.47.31:443: connect: connection refused"
    exit 1 ;;
  registry-pruned)
    [ -e "$STUB_LOG.pruned" ] && exit 0
    : > "$STUB_LOG.pruned"
    echo "#17 1.311 error: failed to copy: failed to do request: Put \"https://registry.example/v2/movies-web/blobs/uploads/01a1\": write tcp 172.17.0.2:58052->2.28.47.31:443: write: connection reset by peer"
    echo "ERROR: failed to build: failed to solve: failed to push registry.example/movies-web:abc123: unexpected status from HEAD request to https://registry.example/v2/movies-web/blobs/sha256:46e1: 502 Bad Gateway"
    exit 1 ;;
  cut-short)
    echo "#13 2.036 error: short read: expected 42139920 bytes but got 13714392: unexpected EOF from registry.example"
    exit 1 ;;
  exit-7) exit 7 ;;
esac
"""
# The registry's /v2/ answers, one per probe from $STUB_ANSWERS (the last repeats); every URL probed to $STUB_LOG.curl.
CURL = r"""#!/usr/bin/env bash
printf '%s\n' "${!#}" >> "$STUB_LOG.curl"
set -- $STUB_ANSWERS
n=$(($(wc -l < "$STUB_LOG.curl")))
[ "$n" -gt "$#" ] && n=$#
printf '%s' "${!n}"
"""
DATE = """#!/usr/bin/env bash
echo "$STUB_HHMM"
"""
V2 = "https://registry.example/v2/"


def lines(text: str) -> List[str]:
    return text.splitlines()


class BuildImageTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        for name, stub in (("docker", DOCKER), ("curl", CURL), ("date", DATE)):
            os.chmod(self.write(f".fakes/bin/{name}", stub), 0o755)
        shutil.copy(DOCKERFILE, self.path("Dockerfile"))
        with open(DOCKERFILE) as f:
            self.dockerfile = f.read()
        self.out = ""

    def build(self, state: str, *args: str, answers: str = "200", hhmm: str = "1200") -> int:
        """The tool building `web` with the stub in `state`, the registry's /v2/ answering `answers` in turn, at UTC
        `hhmm`, from the scratch context; its exit status."""
        self.write("log", "")
        self.write("log.curl", "")
        if os.path.exists(self.path("log.pruned")):
            os.remove(self.path("log.pruned"))
        argv = args or ("web", "registry.example/movies-web", "abc123")
        done = self.run_tool(*argv, env={"STUB_BUILD": state, "STUB_LOG": self.path("log"),
                                         "STUB_DOCKERFILE": self.path("used-dockerfile"), "STUB_ANSWERS": answers,
                                         "STUB_HHMM": hhmm, "IMAGE_PUSH_WAIT_SECONDS": "5",
                                         "IMAGE_PUSH_POLL_SECONDS": "0"})
        self.out = done.output
        return done.code

    def builds(self) -> List[str]:
        return [line for line in lines(self.read("log")) if line.startswith("buildx build")]

    def probes(self) -> List[str]:
        return lines(self.read("log.curl"))

    def said(self, pattern: str) -> int:
        return sum(1 for line in lines(self.out) if re.search(pattern, line))

    def test_a_build_that_succeeds_builds_once_for_amd64_under_the_sha_tag_only(self) -> None:
        self.assertEqual(0, self.build("ok"), self.out)
        self.assertEqual(["buildx build --platform linux/amd64 --push --build-arg BIN=web --build-arg COMMIT_SHA=abc123 "
                          "--tag registry.example/movies-web:abc123 --cache-to type=inline "
                          "--cache-from type=registry,ref=registry.example/movies-web:latest ."], self.builds())
        self.assertEqual(self.dockerfile, self.read("used-dockerfile"))

    def test_a_build_the_mirror_fails_a_blob_of_is_built_again_from_docker_hub(self) -> None:
        self.assertEqual(0, self.build("mirror-blob"), self.out)
        builds = self.builds()
        self.assertEqual(2, len(builds))
        self.assertNotIn("--cache-from", builds[1])
        used = self.read("used-dockerfile")
        self.assertEqual(0, sum(1 for line in lines(used) if line.startswith("FROM mirror.gcr.io/")))
        expected = [line.replace("mirror.gcr.io/library/", "docker.io/library/") if line.startswith("FROM ") else line
                    for line in lines(self.dockerfile)]
        self.assertEqual(expected, lines(used))
        self.assertEqual(1, self.said(r"^::warning::the web image.s build could not fetch a blob through mirror.gcr.io"))

    def test_buildkits_unknown_blob_from_the_mirror_falls_back_the_same_way(self) -> None:
        self.assertEqual(0, self.build("mirror-buildkit"), self.out)
        self.assertEqual(2, len(self.builds()))

    def test_a_build_failing_on_its_own_dockerfile_fails_once_its_error_on_the_log(self) -> None:
        self.assertEqual(1, self.build("broken"))
        self.assertEqual(1, len(self.builds()))
        self.assertEqual(1, self.said("did not complete successfully"), self.out)
        self.assertEqual(0, self.said(r"^::error::.*lost registry"))

    def test_a_builds_own_exit_status_is_the_tools(self) -> None:
        self.assertEqual(7, self.build("exit-7"))

    def test_a_build_into_the_0400_registry_prune_waits_for_the_registry_and_builds_once_more(self) -> None:
        self.assertEqual(0, self.build("registry-pruned", answers="502 502 401", hhmm="0400"), self.out)
        self.assertEqual([V2, V2, V2], self.probes())
        builds = self.builds()
        self.assertEqual(2, len(builds))
        self.assertIn("--cache-from type=registry,ref=registry.example/movies-web:latest", builds[1])
        self.assertEqual(1, self.said(r"^::warning::the web image.s build lost registry.example: connection reset by "
                                      r"peer -- the image registry was stopped by its daily 04:00 UTC registry-prune"),
                         self.out)

    def test_a_registry_back_at_once_outside_the_prune_window_is_built_again_with_the_generic_cause(self) -> None:
        self.assertEqual(0, self.build("registry-down"), self.out)
        self.assertEqual(2, len(self.builds()))
        self.assertEqual(1, self.said(r"^::warning::the web image.s build lost registry.example: connection refused -- "
                                      r"the image registry was down or restarting"), self.out)

    def test_a_build_the_image_registry_stays_gone_from_fails_after_waiting_naming_it(self) -> None:
        self.assertEqual(1, self.build("registry-down", answers="000"))
        self.assertGreaterEqual(self.said("answers 000, waiting for it"), 1, self.out)
        self.assertEqual(1, len(self.builds()))
        self.assertEqual(1, self.said(r"^::error::the web image.s build lost registry.example: connection refused, and "
                                      r"it still answered 000 after 5 s"), self.out)

    def test_a_transfer_from_the_registry_cut_short_is_the_registry_lost_too(self) -> None:
        self.assertEqual(1, self.build("cut-short"))
        self.assertEqual(2, len(self.builds()))
        self.assertEqual(1, self.said(r"^::warning::the web image.s build lost registry.example: a transfer cut short"),
                         self.out)

    def test_a_build_failing_on_its_own_dockerfile_never_probes_the_registry(self) -> None:
        self.assertEqual(1, self.build("broken"))
        self.assertEqual([], self.probes())

    def test_a_missing_argument_is_refused_before_any_build(self) -> None:
        self.assertNotEqual(0, self.build("ok", "web", "registry.example/movies-web"))
        self.assertEqual([], self.builds())
        self.assertIn("usage:", self.out)


if __name__ == "__main__":
    unittest.main()
