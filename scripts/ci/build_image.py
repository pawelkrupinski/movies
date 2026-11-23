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
same blobs. A build that lost the image registry itself fails naming it (`registry_lost`). Any other failure is the
build's verdict as it stands.

ci.yml runs it by the name scripts/ci/build-image.sh (the worker's from its own staged context); tested by
build_image_test.py beside it."""
from __future__ import annotations

import os
import re
import sys
import tempfile
from typing import List, Mapping, Optional, Sequence

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from tool_process import Command, Runner, SubprocessRunner  # noqa: E402

USAGE = "usage: build-image.sh <bin> <image repository> <commit sha>"
MIRROR_LOST_BLOB = re.compile(r"could not fetch content descriptor|unknown blob")
REGISTRY_GONE = re.compile(r"connection refused|connection reset by peer|i/o timeout|no such host")
CUT_SHORT = re.compile(r"short read: expected [0-9]* bytes")


def registry_lost(log: str, registry: str) -> Optional[str]:
    """THE REGISTRY GONE. A build that lost the image registry itself -- a refused connection, a reset, a transfer cut
    short -- is no verdict on the build: Main #2361 (2026-11-09 23:33Z) lost registry.kinowo.net mid-pull while a host
    switch rolled out ("short read ... unexpected EOF", then "connect: connection refused"). Said by name, once, never
    retried: a retry inside the outage cannot help, and one that passed would hide it."""
    for line in log.split("\n"):
        found = REGISTRY_GONE.search(line) if registry in line else None
        if found:
            return found.group(0)
    if CUT_SHORT.search(log) and registry in log:
        return "a transfer cut short"
    return None


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
            print(f"::error::the {self.image} image's build lost {registry}: {lost} -- the image registry was down or "
                  "restarting (a host switch, or its daily prune's garbage collect), not the build", flush=True)
            return status
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
    if len(argv) < 3 or not all(argv[:3]):
        print(USAGE, file=sys.stderr)
        return 1
    with tempfile.TemporaryDirectory() as work:
        return ImageBuild(argv[0], argv[1], argv[2], SubprocessRunner(), env).run(work)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], os.environ))
