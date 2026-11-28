#!/usr/bin/env python3
"""Build one of the Dockerfile's images for linux/amd64 and push it under the commit's SHA tag ONLY (main.yml's publish
jobs give it the tags that ship). Run from the repository root, the build context, with buildx's default builder set up
by .github/actions/setup-buildx.

  build-image.sh <bin: web|worker> <image repository> <commit sha>

amd64 ONLY: the cluster is Hetzner x86_64. INLINE cache, restored from the image last shipped (`latest`): the layers
worth reusing (base OS, JRE, libvips) are all in it, and inline metadata rides the push itself.

THE MIRROR'S FALLBACK. The Dockerfile's bases come through mirror.gcr.io (CiImageMirrorSpec), which at times answers for
blobs it served minutes before as if they were gone: run 38788714642 (2026-10-15, "could not fetch content descriptor
... not found") and Main #2264 (2026-10-28, BuildKit's pull "unknown blob", then the ubuntu base's 42 MB layer "could
not fetch content descriptor ... not found", served again a few minutes later). A build that fails THAT way is built
once more from the same digests on Docker Hub, and without the cache import, whose lazily fetched layers can name the
same blobs. A build that lost the image registry itself waits for it to come back and builds once more
(`registry_lost`, `RegistryWait`). Any other failure is the build's verdict as it stands.

ci.yml runs it by the name scripts/ci/build-image.sh (the worker's from its own staged context); tested by
build_image_test.py beside it."""
from __future__ import annotations

import os
import re
import sys
import tempfile
import time
from typing import Callable, List, Mapping, Optional, Sequence

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from processes import machine  # noqa: E402
from tool_process import Command, Runner, SubprocessRunner  # noqa: E402

USAGE = "usage: build-image.sh <bin> <image repository> <commit sha>"
MIRROR_LOST_BLOB = re.compile(r"could not fetch content descriptor|unknown blob")
REGISTRY_GONE = re.compile(r"connection refused|connection reset by peer|i/o timeout|no such host|502 Bad Gateway|"
                           r"503 Service Unavailable")
CUT_SHORT = re.compile(r"short read: expected [0-9]* bytes")


def registry_lost(log: str, registry: str) -> Optional[str]:
    """THE REGISTRY GONE. A build that lost the image registry itself -- a refused connection, a reset, a 502 from the
    proxy in front of it, a transfer cut short -- is no verdict on the build: Main #2361 (2026-11-09 23:33Z) lost
    registry.kinowo.net mid-pull while a host switch rolled out ("short read ... unexpected EOF", then "connect:
    connection refused"); Main #2662 (2026-11-19 04:00:22Z) lost it mid-push to the daily 04:00 UTC `registry-prune`
    (infra/nix/modules/roles/registry.nix), which stops the registry for its garbage-collect ("write: connection reset
    by peer", then "502 Bad Gateway"). Both outages are minutes long and END: RegistryWait waits for it."""
    for line in log.split("\n"):
        found = REGISTRY_GONE.search(line) if registry in line else None
        if found:
            return found.group(0)
    if CUT_SHORT.search(log) and registry in log:
        return "a transfer cut short"
    return None


PRUNE_WHY = "the image registry was stopped by its daily 04:00 UTC registry-prune (infra/nix/modules/roles/registry.nix)"
OUTAGE_WHY = "the image registry was down or restarting (a host switch, or its daily prune's garbage collect)"


class RegistryWait:
    """A lost registry waited for, bounded (IMAGE_PUSH_WAIT_SECONDS, 600; polled every IMAGE_PUSH_POLL_SECONDS, 15),
    until its /v2/ answers 200, or 401 asking for the login docker/login-action already did; the build then runs once
    more (the builder's own cache makes that the push), saying on the run what it lost and how long it waited -- never a
    silent retry. A registry still gone at the deadline fails naming it."""

    def __init__(self, image: str, registry: str, runner: Runner, env: Mapping[str, str],
                 clock: Callable[[], float] = time.monotonic, sleep: Callable[[float], None] = time.sleep) -> None:
        self.image = image
        self.registry = registry
        self.runner = runner
        self.env = env
        self.clock = clock
        self.sleep = sleep
        self.limit = int(env.get("IMAGE_PUSH_WAIT_SECONDS", "600"))
        self.poll = float(env.get("IMAGE_PUSH_POLL_SECONDS", "15"))

    def why(self) -> str:
        """The prune by name in its 04:00-04:20 UTC window (the time read as `date -u`, as the run's log reads it)."""
        hhmm = self.runner.capture(Command(["date", "-u", "+%H%M"], self.env)).stdout.strip()
        return PRUNE_WHY if hhmm.isdigit() and 400 <= int(hhmm) < 420 else OUTAGE_WHY

    def answer(self) -> str:
        done = self.runner.capture(Command(["curl", "-s", "-o", "/dev/null", "-w", "%{http_code}", "--max-time", "10",
                                            f"https://{self.registry}/v2/"], self.env))
        return done.stdout.strip() if done.code == 0 and done.stdout.strip() else "000"

    def back(self, lost: str) -> bool:
        """Whether the registry answered again within the limit; the wait and its verdict said on the run."""
        why = self.why()
        start = self.clock()
        answer = self.answer()
        while answer not in ("200", "401") and self.clock() - start < self.limit:
            print(f"the {self.image} image's build lost {self.registry} ({lost}): it answers {answer}, waiting for it "
                  f"({int(self.clock() - start)} s of {self.limit} s)", flush=True)
            self.sleep(self.poll)
            answer = self.answer()
        if answer not in ("200", "401"):
            print(f"::error::the {self.image} image's build lost {self.registry}: {lost}, and it still answered "
                  f"{answer} after {self.limit} s -- {why}, not the build", flush=True)
            return False
        print(f"::warning::the {self.image} image's build lost {self.registry}: {lost} -- {why}; it answered again "
              f"after {int(self.clock() - start)} s: building once more", flush=True)
        return True


def docker_hub_bases(dockerfile: str) -> str:
    """The same pinned digests under Docker Hub's names."""
    return re.sub(r"(?m)^FROM mirror\.gcr\.io/library/", "FROM docker.io/library/", dockerfile)


class ImageBuild:
    def __init__(self, image: str, repository: str, sha: str, runner: Runner, env: Mapping[str, str]) -> None:
        self.image = image
        self.repository = repository
        self.sha = sha
        self.runner = runner
        self.env = env

    def argv(self, extra: Sequence[str]) -> List[str]:
        return ["docker", "buildx", "build", "--platform", "linux/amd64", "--push", "--build-arg", f"BIN={self.image}",
                "--build-arg", f"COMMIT_SHA={self.sha}", "--tag", f"{self.repository}:{self.sha}",
                "--cache-to", "type=inline", *extra, "."]

    def run(self, work: str) -> int:
        log = os.path.join(work, "mirror.log")
        cache = ["--cache-from", f"type=registry,ref={self.repository}:latest"]
        status = self.runner.run(Command(self.argv(cache), self.env, log=log, tee=True))
        if status == 0:
            return 0
        with open(log, errors="replace") as f:
            said = f.read()
        registry = self.repository.split("/", 1)[0]
        lost = registry_lost(said, registry)
        if lost:
            if not RegistryWait(self.image, registry, self.runner, self.env).back(lost):
                return status
            return self.runner.run(Command(self.argv(cache), self.env))
        if not MIRROR_LOST_BLOB.search(said):
            return status
        print(f"::warning::the {self.image} image's build could not fetch a blob through mirror.gcr.io: building it "
              "again from Docker Hub", flush=True)
        # the context's .dockerignore still applies (there is no Dockerfile.dockerignore beside this copy)
        dockerfile = os.path.join(work, "Dockerfile")
        with open("Dockerfile") as source, open(dockerfile, "w") as copy:
            copy.write(docker_hub_bases(source.read()))
        return self.runner.run(Command(self.argv(["-f", dockerfile]), self.env))


def main(argv: List[str], env: Mapping[str, str]) -> int:
    image, repository, sha = (argv + [""] * 3)[:3]
    if not (image and repository and sha):
        print(USAGE, file=sys.stderr)
        return 1
    with tempfile.TemporaryDirectory() as work:
        return ImageBuild(image, repository, sha, SubprocessRunner(), env).run(work)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], machine.environment()))
