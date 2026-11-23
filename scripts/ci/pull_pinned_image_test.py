#!/usr/bin/env python3
"""scripts/ci/pull-pinned-image.sh, black-box, against a stub `docker` whose registries serve, refuse, stall or hand
over the wrong bytes on cue. Ported from pull-pinned-image-test.sh; the same cases hold the shell tool and its Python
rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import sys
import unittest
from typing import List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/pull_pinned_image.py", "scripts/ci/pull-pinned-image.sh")
DIGEST = "sha256:" + "a" * 64
GHCR = f"ghcr.io/owner/mongo@{DIGEST}"
GCR = f"mirror.gcr.io/library/mongo@{DIGEST}"
HUB = f"docker.io/library/mongo@{DIGEST}"

# STUB_GHCR / STUB_GCR / STUB_HUB: `up` serves the pull, `down` refuses it, `stalls` never ends it, `wrong` serves an
# image whose RepoDigests name another digest; STUB_GHCR=private refuses a logged-in pull and serves an anonymous one.
# STUB_DAEMON_MIRROR is the daemon's own Docker Hub mirror. A `login` records what it read on stdin in $STUB_LOGIN.
DOCKER = r"""#!/usr/bin/env bash
printf '%s\n' "$*" >> "$STUB_LOG"
ref="${*: -1}"
state() { case "$1" in ghcr.io/*) echo "${STUB_GHCR:-up}" ;; mirror.gcr.io/*) echo "${STUB_GCR:-up}" ;; *) echo "${STUB_HUB:-up}" ;; esac; }
case "$1" in
  pull)
    case "$(state "$ref")" in
      down) exit 1 ;;
      stalls) exec sleep 600 ;;
      private) [ -e "$STUB_LOG.loggedin" ] && exit 1 ;;
    esac
    echo "$ref" >> "$STUB_LOG.pulled"; exit 0 ;;
  image)
    grep -qxF -- "$ref" "$STUB_LOG.pulled" 2>/dev/null || exit 1
    [ "$(state "$ref")" = wrong ] && { echo "${ref%@*}@sha256:$(printf 'b%.0s' $(seq 64))"; exit 0; }
    echo "$ref"; exit 0 ;;
  info) [ -n "${STUB_DAEMON_MIRROR:-}" ] && echo "$STUB_DAEMON_MIRROR"; exit 0 ;;
  login) cat >> "$STUB_LOGIN"; [ "$2" = ghcr.io ] && : > "$STUB_LOG.loggedin"; exit 0 ;;
  logout) rm -f "$STUB_LOG.loggedin"; exit 0 ;;
esac
exit 0
"""


class PullPinnedImageTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        docker = self.write(".fakes/bin/docker", DOCKER)
        os.chmod(docker, 0o755)
        pins = self.write("pins.tsv", f"# a comment\nmongo\tdocker.io/library/mongo:8.3.17@{DIGEST}\tghcr.io/owner/mongo\n")
        self.log = self.path("log")
        self.login = self.path("login")
        self.base = {"PINNED_IMAGES": pins, "STUB_LOG": self.log, "STUB_LOGIN": self.login}
        self.err = ""

    def pull(self, *args: str, quick: bool = True, **env: str) -> str:
        """The tool pulling `mongo`: "<exit status> <stdout>"; its stderr in self.err, docker's calls in self.log."""
        self.write("log", "")
        self.write("login", "")
        for leftover in (self.log + ".pulled", self.log + ".loggedin"):
            if os.path.exists(leftover):
                os.remove(leftover)
        bounds = {"PINNED_PULL_SECONDS": "1", "PINNED_GHCR_PULL_SECONDS": "1"} if quick else {}
        done = self.run_tool("mongo", *args, env={**self.base, **bounds, **env})
        self.err = done.stderr
        return f"{done.code} {done.stdout.strip()}".strip()

    def logged(self) -> List[str]:
        return self.read("log").splitlines()

    def pulls(self) -> str:
        return " ".join(line[len("pull -q "):] for line in self.logged() if line.startswith("pull"))

    def count(self, prefix: str, lines: Optional[List[str]] = None) -> int:
        return sum(1 for line in (self.logged() if lines is None else lines) if line.startswith(prefix))

    def errors(self, prefix: str) -> int:
        return self.count(prefix, self.err.splitlines())

    def mentions(self, text: str) -> int:
        return sum(1 for line in self.logged() if text in line)

    # the pin

    def test_the_pins_fields_read_without_pulling(self) -> None:
        fields = [self.run_tool("--pin", "mongo", f, env=self.base).stdout.strip()
                  for f in ("repository", "tag", "digest", "mirror", "image")]
        self.assertEqual(["docker.io/library/mongo", "8.3.17", DIGEST, "ghcr.io/owner/mongo",
                          f"docker.io/library/mongo:8.3.17@{DIGEST}"], fields)

    def test_an_unknown_pin_name_is_refused(self) -> None:
        done = self.run_tool("--pin", "redis", "tag", env=self.base)
        self.assertEqual(1, done.code, done.output)
        self.assertIn("pins no image named redis", done.stderr)

    def test_an_unknown_field_and_a_short_pin_line_are_usage_errors(self) -> None:
        self.assertEqual(64, self.run_tool("--pin", "mongo", "colour", env=self.base).code)
        self.assertEqual(64, self.run_tool("--pin", "mongo", env=self.base).code)

    def test_no_name_or_a_command_without_its_separator_is_a_usage_error(self) -> None:
        self.assertEqual(64, self.run_tool(env=self.base).code)
        self.assertEqual(64, self.run_tool("mongo", "echo", env=self.base).code)
        self.assertEqual(64, self.run_tool("mongo", "--", env=self.base).code)

    def test_a_pin_naming_no_sha256_digest_is_refused(self) -> None:
        self.base["PINNED_IMAGES"] = self.write("bad.tsv", "mongo\tdocker.io/library/mongo:8.3.17@md5:abc\tghcr.io/o/m\n")
        self.assertEqual("1", self.pull())
        self.assertIn("::error::mongo's pin names no sha256 digest (md5:abc)", self.err)

    # GHCR first

    def test_the_ghcr_mirror_serves_the_pinned_digest_first_pulling_nothing_else(self) -> None:
        self.assertEqual(f"0 {GHCR}", self.pull())
        self.assertEqual(GHCR, self.pulls())
        self.assertIn(f"mongo: {GHCR}", self.err)

    def test_logged_in_to_ghcr_with_the_jobs_token_as_the_actor_on_stdin_only(self) -> None:
        self.assertEqual(f"0 {GHCR}", self.pull(GHCR_TOKEN="tok", GITHUB_ACTOR="me"))
        self.assertEqual("tok", self.read("login"))
        self.assertEqual(1, self.count("login ghcr.io -u me --password-stdin"))
        self.assertEqual(0, self.mentions("tok"))

    def test_without_a_token_ghcr_is_pulled_anonymously(self) -> None:
        self.pull()
        self.assertEqual(0, self.count("login"))

    def test_a_token_ghcr_refuses_leaves_the_last_try_anonymous_after_a_logout(self) -> None:
        self.assertEqual(f"0 {GHCR}", self.pull(STUB_GHCR="private", GHCR_TOKEN="tok"))
        self.assertEqual(1, self.count("logout ghcr.io"))

    # the fallbacks

    def test_a_ghcr_that_refuses_falls_back_to_mirror_gcr_io_after_its_tries_saying_so(self) -> None:
        self.assertEqual(f"0 {GCR}", self.pull(STUB_GHCR="down"))
        self.assertEqual(f"{GHCR} {GHCR} {GCR}", self.pulls())
        self.assertEqual(1, self.errors("::warning::mongo: the GHCR mirror (ghcr.io/owner/mongo) did not serve"))

    def test_a_ghcr_pull_that_stalls_is_bounded_and_falls_back_too(self) -> None:
        self.assertEqual(f"0 {GCR}", self.pull(STUB_GHCR="stalls"))
        self.assertEqual(2, sum(1 for line in self.err.splitlines() if "stalled past 1s" in line), self.err)

    def test_ghcr_and_mirror_gcr_io_both_failing_falls_back_to_docker_hub(self) -> None:
        # run 44526198751
        self.assertEqual(f"0 {HUB}", self.pull(STUB_GHCR="down", STUB_GCR="stalls"))

    def test_docker_hub_logged_in_as_the_ci_account_its_token_on_stdin_only(self) -> None:
        self.assertEqual(f"0 {HUB}", self.pull(STUB_GHCR="down", STUB_GCR="down", DOCKERHUB_USERNAME="ci-user",
                                               DOCKERHUB_TOKEN="s3cret"))
        self.assertEqual("s3cret", self.read("login"))
        self.assertEqual(1, self.count("login docker.io -u ci-user --password-stdin"))
        self.assertEqual(0, self.mentions("s3cret"))

    def test_docker_hub_without_its_token_is_pulled_anonymously(self) -> None:
        self.pull(STUB_GHCR="down", STUB_GCR="down")
        self.assertEqual(0, self.count("login"))

    def test_no_source_serving_it_fails_naming_the_pin_as_an_error(self) -> None:
        self.assertEqual("1", self.pull(STUB_GHCR="down", STUB_GCR="down", STUB_HUB="down"))
        self.assertEqual(1, self.errors(f"::error::no source served mongo (docker.io/library/mongo@{DIGEST})"))

    def test_an_image_whose_digest_is_not_the_pin_is_refused_and_the_next_source_tried(self) -> None:
        self.assertEqual(f"0 {GCR}", self.pull(STUB_GHCR="wrong", PINNED_GHCR_ATTEMPTS="1"))
        self.assertEqual(1, self.errors(f"::error::{GHCR} was pulled but does not carry {DIGEST}"))

    # already here, the daemon's mirror

    def test_an_image_already_here_is_used_as_it_is_nothing_pulled(self) -> None:
        self.write("log", "")
        self.write("log.pulled", GCR + "\n")
        done = self.run_tool("mongo", env=self.base)
        self.assertEqual((0, GCR), (done.code, done.stdout.strip()), done.output)
        self.assertEqual(0, self.count("pull"))

    def test_a_daemon_with_a_docker_hub_mirror_pulls_docker_hubs_name_first_through_it(self) -> None:
        self.assertEqual(f"0 {HUB}", self.pull(STUB_DAEMON_MIRROR="http://mirror.internal:3012/"))
        self.assertEqual(HUB, self.pulls())

    # -- command

    def test_a_command_runs_after_the_pull_with_the_ref_in_pinned_image(self) -> None:
        self.assertEqual(f"0 ran {GHCR}", self.pull("--", "bash", "-c", 'echo ran "$PINNED_IMAGE"'))

    def test_a_command_does_not_run_when_the_pull_failed(self) -> None:
        self.assertEqual("1", self.pull("--", "echo", "ran", STUB_GHCR="down", STUB_GCR="down", STUB_HUB="down"))

    def test_the_commands_exit_status_is_the_tools(self) -> None:
        self.assertEqual("3", self.pull("--", "sh", "-c", "exit 3"))


if __name__ == "__main__":
    unittest.main()
