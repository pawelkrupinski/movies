#!/usr/bin/env python3
"""Pull one of .github/pinned-images.tsv's images BY DIGEST, from the first source that serves it, and refuse any other.

  pull-pinned-image.sh <name>                  # pull; the local ref (<source>@sha256:...) on stdout, progress on stderr
  pull-pinned-image.sh <name> -- <command...>  # pull, then exec <command> with PINNED_IMAGE=<that ref>
  pull-pinned-image.sh --pin <name> <field>    # print the pin's image|repository|tag|digest|mirror; pulls nothing

In CI it is run by ./.github/actions/pull-pinned-image, the one way a workflow or action pulls a pinned image
(CiPinnedImageSpec); scripts/ci/start-mongo-replset.sh runs it itself only where nothing pulled first (locally).

THE SOURCES, in order, each pull bounded and a stalled pull treated as a refused one:
  1. the daemon's own Docker Hub mirror, when it has one (`docker info`): Docker Hub's name, served from its disk;
  2. the GHCR mirror (this repo's package, filled by .github/workflows/mirror-pinned-images.yml), logged in with
     GHCR_TOKEN (the job's GITHUB_TOKEN) when set -- PINNED_GHCR_PULL_SECONDS (60) a try, PINNED_GHCR_ATTEMPTS (2)
     tries, the last one anonymous (a public package needs no token; a token without `packages: read` is refused);
  3. mirror.gcr.io, Google's pull-through cache of Docker Hub;
  4. Docker Hub itself, logged in as DOCKERHUB_USERNAME/DOCKERHUB_TOKEN when both are set.
Sources 3 and 4 get PINNED_PULL_SECONDS (120) each. Every token goes to `docker login` on stdin, never argv.

WHY GHCR FIRST: Identity model convergence #1286 (run 44526198751, 2026-11-19) lost a leg before any build ran --
mirror.gcr.io stalled past 120 s, Docker Hub refused the CI account's expired token, then stalled anonymously.
GitHub's own registry, reached from GitHub's own runners with the job's own token, depends on neither.

BY DIGEST, ALWAYS: every pull names the pin's sha256, so each source can only hand over the pinned bytes, and the
pulled image's RepoDigests must name it too before its ref is printed. A tag that moved upstream changes nothing here.

Stdlib only: the action runs it in whatever checkout its job has. Workflows and actions run it by the name
scripts/ci/pull-pinned-image.sh; tested by pull_pinned_image_test.py beside it."""
from __future__ import annotations

import os
import re
import subprocess
import sys
import time
from dataclasses import dataclass
from typing import List, Mapping, Optional, Sequence
import ci_machine  # noqa: E402

HERE = os.path.dirname(os.path.abspath(__file__))
FIELDS = "image|repository|tag|digest|mirror"
SHA256 = re.compile(r"^sha256:[0-9a-f]{64}$")
TIMED_OUT = 124


class Refused(Exception):
    """A pin that cannot be read: its message on stderr, and `code` the exit status."""

    def __init__(self, message: str, code: int = 1) -> None:
        super().__init__(message)
        self.code = code


def after(text: str, sep: str) -> str:
    """`${text#*sep}`: what follows the first `sep`, or all of `text` without one."""
    return text.split(sep, 1)[1] if sep in text else text


@dataclass(frozen=True)
class Pin:
    image: str
    mirror: str

    @property
    def repository(self) -> str:
        return self.image.split(":", 1)[0]

    @property
    def hub(self) -> str:
        """The repository as Docker Hub names it, without its `docker.io/`."""
        return self.repository[len("docker.io/"):] if self.repository.startswith("docker.io/") else self.repository

    @property
    def tag(self) -> str:
        return after(self.image, ":").split("@", 1)[0]

    @property
    def digest(self) -> str:
        return after(self.image, "@")

    def field(self, name: str) -> str:
        if name not in FIELDS.split("|"):
            raise Refused(f"unknown field {name}", 64)
        return str(getattr(self, name))


def read_pin(pins: str, name: str) -> Pin:
    with open(pins) as f:
        for line in f.read().split("\n"):
            cells = line.split("\t")
            if cells[0] == name:
                return Pin(cells[1] if len(cells) > 1 else "", cells[2] if len(cells) > 2 else "")
    raise Refused(f"::error::{pins} pins no image named {name}")


def say(line: str) -> None:
    print(line, file=sys.stderr, flush=True)


class Docker:
    """`docker`, each call bounded as `timeout -k` bounds it, its output on stderr."""

    def __init__(self, env: Mapping[str, str]) -> None:
        self.env = dict(env)

    def call(self, argv: Sequence[str], seconds: Optional[float] = None, stdin: Optional[str] = None) -> int:
        try:
            done = subprocess.run(["docker", *argv], env=self.env, input=stdin, stdout=sys.stderr,
                                  stdin=None if stdin is not None else subprocess.DEVNULL, text=True, timeout=seconds)
        except subprocess.TimeoutExpired:
            return TIMED_OUT
        except OSError:
            return 127
        return done.returncode

    def output(self, argv: Sequence[str]) -> str:
        try:
            return subprocess.run(["docker", *argv], env=self.env, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL,
                                  stdin=subprocess.DEVNULL, text=True).stdout
        except OSError:
            return ""


class Puller:
    def __init__(self, name: str, pin: Pin, env: Mapping[str, str], docker: Docker) -> None:
        self.name = name
        self.pin = pin
        self.env = env
        self.docker = docker
        self.hub_ref = f"docker.io/{pin.hub}@{pin.digest}"
        self.ghcr_ref = f"{pin.mirror}@{pin.digest}"
        self.mirror_gcr_ref = f"mirror.gcr.io/{pin.hub}@{pin.digest}"
        self.pull_seconds = int(env.get("PINNED_PULL_SECONDS") or 120)

    def verified(self, ref: str) -> bool:
        """The pulled image carries the pin's digest, or it is not the pinned image."""
        lines = self.docker.output(["image", "inspect", "--format", "{{range .RepoDigests}}{{println .}}{{end}}", ref])
        return any(line.endswith("@" + self.pin.digest) for line in lines.split("\n"))

    def pull(self, ref: str, seconds: int) -> bool:
        say(f"pulling {ref} (at most {seconds}s)")
        started = time.monotonic()
        status = self.docker.call(["pull", "-q", ref], seconds=seconds)
        say(f"the pull of {ref} ended (status {status}) after {int(time.monotonic() - started)}s")
        if status == TIMED_OUT:
            say(f"::warning::the pull of {ref} stalled past {seconds}s")
        if status != 0:
            return False
        if self.verified(ref):
            return True
        say(f"::error::{ref} was pulled but does not carry {self.pin.digest}")
        return False

    def login(self, registry: str, user: str, token: str) -> None:
        if self.docker.call(["login", registry, "-u", user, "--password-stdin"], seconds=30, stdin=token) != 0:
            say(f"::warning::could not log in to {registry}; pulling from it anonymously")

    def already_here(self) -> Optional[str]:
        """An earlier step pulled it: nothing to fetch."""
        return next((ref for ref in (self.ghcr_ref, self.mirror_gcr_ref, self.hub_ref) if self.verified(ref)), None)

    def through_daemon_mirror(self) -> Optional[str]:
        mirrors = self.docker.output(["info", "--format", "{{range .RegistryConfig.Mirrors}}{{.}} {{end}}"])
        if mirrors.strip() and self.pull(self.hub_ref, self.pull_seconds):
            return self.hub_ref
        return None

    def from_ghcr(self) -> Optional[str]:
        registry = self.pin.mirror.split("/", 1)[0]
        token = self.env.get("GHCR_TOKEN", "")
        if token:
            self.login(registry, self.env.get("GITHUB_ACTOR") or "github-actions", token)
        attempts = int(self.env.get("PINNED_GHCR_ATTEMPTS") or 2)
        seconds = int(self.env.get("PINNED_GHCR_PULL_SECONDS") or 60)
        for attempt in range(1, attempts + 1):
            if attempt == attempts and attempt > 1 and token:
                self.docker.call(["logout", registry])  # the last try anonymous: a public package needs no token
            if self.pull(self.ghcr_ref, seconds):
                return self.ghcr_ref
        return None

    def from_mirror_gcr(self) -> Optional[str]:
        say(f"::warning::{self.name}: the GHCR mirror ({self.pin.mirror}) did not serve {self.pin.digest}; "
            "trying mirror.gcr.io")
        return self.mirror_gcr_ref if self.pull(self.mirror_gcr_ref, self.pull_seconds) else None

    def from_docker_hub(self) -> Optional[str]:
        say(f"::warning::{self.name}: mirror.gcr.io refused or stalled the pull; falling back to Docker Hub")
        user, token = self.env.get("DOCKERHUB_USERNAME", ""), self.env.get("DOCKERHUB_TOKEN", "")
        if user and token:
            self.login("docker.io", user, token)
        return self.hub_ref if self.pull(self.hub_ref, self.pull_seconds) else None

    def ref(self) -> Optional[str]:
        """The first source's ref that served the pinned digest, in the order the header gives."""
        for source in (self.already_here, self.through_daemon_mirror, self.from_ghcr, self.from_mirror_gcr,
                       self.from_docker_hub):
            found = source()
            if found:
                return found
        return None


def usage(text: str) -> int:
    print(f"usage: pull-pinned-image.sh {text}", file=sys.stderr)
    return 64


def pinned(pins: str, name: str) -> Pin:
    pin = read_pin(pins, name)
    if not SHA256.match(pin.digest):
        raise Refused(f"::error::{name}'s pin names no sha256 digest ({pin.digest})")
    return pin


def run_command(command: List[str], ref: str, env: Mapping[str, str]) -> int:
    try:
        os.execvpe(command[0], command, {**env, "PINNED_IMAGE": ref})
    except OSError as error:
        say(f"pull-pinned-image.sh: {command[0]}: {error.strerror}")
    return 127


def pull_and_run(pins: str, name: str, command: List[str], env: Mapping[str, str]) -> int:
    pin = pinned(pins, name)
    ref = Puller(name, pin, env, Docker(env)).ref()
    if not ref:
        say(f"::error::no source served {name} ({pin.repository}@{pin.digest})")
        return 1
    say(f"{name}: {ref}")
    if command:
        return run_command(command, ref, env)
    print(ref, flush=True)
    return 0


def main(argv: List[str], env: Mapping[str, str]) -> int:
    pins = env.get("PINNED_IMAGES") or os.path.join(HERE, "..", "..", ".github", "pinned-images.tsv")
    try:
        if argv[:1] == ["--pin"]:
            if len(argv) != 3:
                return usage(f"--pin <name> {FIELDS}")
            print(read_pin(pins, argv[1]).field(argv[2]))
            return 0
        if not argv or not argv[0]:
            return usage("<name> [-- command...] | --pin <name> <field>")
        if len(argv) > 1 and (argv[1] != "--" or len(argv) < 3):
            return usage("<name> [-- command...]")
        return pull_and_run(pins, argv[0], argv[2:], env)
    except Refused as refused:
        say(str(refused))
        return refused.code


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], ci_machine.environment()))
