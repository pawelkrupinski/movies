#!/usr/bin/env python3
"""Copy every image .github/pinned-images.tsv pins to its GHCR mirror BY DIGEST, and check that GHCR serves exactly the
pin. Run by .github/workflows/mirror-pinned-images.yml (weekly, on dispatch, on a push to main changing the pins).

  mirror-pinned-images.sh

For each pin: GHCR's `<mirror>:<tag>` is read (its raw manifest's sha256). The pin's digest: nothing to do. Missing, or
another digest (DRIFT: the pin was bumped and GHCR not refreshed, or someone pushed over the tag): the pinned digest is
copied over it with `skopeo copy --all --preserve-digests` -- every platform, the index byte for byte, so the GHCR
image's digest IS the upstream digest -- from Docker Hub, else mirror.gcr.io. Then GHCR is read again, and anything but
the pin fails the run, loudly: CI's pulls (scripts/ci/pull-pinned-image.sh) would skip GHCR for that image. The
upstream TAG pointing elsewhere than the pin (an official image rebuilt under the same tag) is only a notice: the pin is
a digest, so nothing CI runs moved; bumping it is infra/bin/nix-update's (mongo) or a deliberate edit.

GHCR_TOKEN (the job's GITHUB_TOKEN, `packages: write`) logs in to ghcr.io as GITHUB_ACTOR; DOCKERHUB_USERNAME and
DOCKERHUB_TOKEN, when both set, log in to Docker Hub for the copy's reads (anonymous otherwise, or when refused). Tokens
go to `skopeo login` on stdin, never argv. Every registry call is bounded (MIRROR_COPY_SECONDS, default 1800, for a
copy; 120 s for a read).

Stdlib only, beside pull_pinned_image.py (whose Pin reads a row). mirror-pinned-images.yml runs it by the name
scripts/ci/mirror-pinned-images.sh; tested by mirror_pinned_images_test.py beside it."""
from __future__ import annotations

import hashlib
import os
import subprocess
import sys
from typing import List, Mapping, Optional, Sequence, Tuple

from pull_pinned_image import SHA256, Pin
import ci_machine  # noqa: E402

HERE = os.path.dirname(os.path.abspath(__file__))
READ_SECONDS = 120


def say(line: str) -> None:
    print(line, flush=True)


class Skopeo:
    """`skopeo`, each call bounded as `timeout -k` bounds it."""

    def __init__(self, env: Mapping[str, str]) -> None:
        self.env = dict(env)

    def call(self, argv: Sequence[str], seconds: float, stdin: Optional[str] = None) -> int:
        try:
            return subprocess.run(["skopeo", *argv], env=self.env, input=stdin, text=True, timeout=seconds,
                                  stdin=None if stdin is not None else subprocess.DEVNULL).returncode
        except subprocess.TimeoutExpired:
            return 124
        except OSError:
            return 127

    def served(self, ref: str) -> str:
        """The sha256 of the manifest the registry serves for `ref`, "" when it serves none."""
        try:
            done = subprocess.run(["skopeo", "inspect", "--raw", "--retry-times", "2", ref], env=self.env,
                                  stdin=subprocess.DEVNULL, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL,
                                  timeout=READ_SECONDS)
        except (subprocess.TimeoutExpired, OSError):
            return ""
        return "sha256:" + hashlib.sha256(done.stdout).hexdigest() if done.returncode == 0 else ""

    def login(self, registry: str, user: str, token: str) -> None:
        if self.call(["login", registry, "-u", user, "--password-stdin"], 60, stdin=token) != 0:
            say(f"::warning::could not log in to {registry} as {user}")


def rows(pins: str) -> List[Tuple[str, str, str]]:
    """Each pin row's name, image and mirror (comments and blank names left out), as `read -r name image mirror`."""
    out: List[Tuple[str, str, str]] = []
    with open(pins) as f:
        for line in f.read().split("\n")[:-1]:
            cells = line.split("\t", 2) + ["", ""]
            if cells[0] and not cells[0].startswith("#"):
                out.append((cells[0], cells[1], cells[2]))
    return out


class Mirror:
    def __init__(self, skopeo: Skopeo, copy_seconds: float) -> None:
        self.skopeo = skopeo
        self.copy_seconds = copy_seconds

    def copy(self, name: str, pin: Pin, target: str) -> str:
        """The pinned digest copied over `target` from the first source that serves it; that source, or ""."""
        for source in (f"docker://{pin.repository}@{pin.digest}", f"docker://mirror.gcr.io/{pin.hub}@{pin.digest}"):
            say(f"{name}: copying {source} to {target[len('docker://'):]}")
            argv = ["copy", "--all", "--preserve-digests", "--retry-times", "3", source, target]
            if self.skopeo.call(argv, self.copy_seconds) == 0:
                return source
            say(f"::warning::{name}: the copy from {source} failed")
        return ""

    def refreshed(self, name: str, pin: Pin, mirror: str) -> bool:
        """GHCR's `<mirror>:<tag>` serving the pin, copied there first when it did not."""
        target = f"docker://{mirror}:{pin.tag}"
        now = self.skopeo.served(target)
        if now == pin.digest:
            say(f"{name}: {mirror}:{pin.tag} is the pin, {pin.digest}")
            return True
        if now:
            say(f"::warning::{name}: DRIFT -- GHCR's {mirror}:{pin.tag} is {now}, the pin is {pin.digest}; "
                "copying the pin over it")
        else:
            say(f"{name}: {mirror}:{pin.tag} is not on GHCR yet; copying {pin.digest}")
        copied = self.copy(name, pin, target)
        now = self.skopeo.served(target)
        if now != pin.digest:
            source = f" from {copied}" if copied else ""
            say(f"::error::{name}: GHCR's {mirror}:{pin.tag} is {now or 'missing'} after the copy{source}, not the pin "
                f"{pin.digest} -- CI pulls of {name} skip GHCR until it is")
            return False
        say(f"{name}: {mirror}:{pin.tag} is now the pin, {pin.digest} (from {copied})")
        return True

    def row(self, name: str, image: str, mirror: str) -> bool:
        pin = Pin(image, mirror)
        if not SHA256.match(pin.digest) or not mirror:
            say(f"::error::{name}: the pin is not <repository>:<tag>@sha256:<digest> TAB <mirror> ({image} {mirror})")
            return False
        if not self.refreshed(name, pin, mirror):
            return False
        upstream = self.skopeo.served(f"docker://{pin.repository}:{pin.tag}")
        if upstream and upstream != pin.digest:
            say(f"::notice::{name}: {pin.repository}:{pin.tag} now points at {upstream}, not the pinned {pin.digest} "
                "(CI keeps the pin)")
        return True


def log_in(skopeo: Skopeo, env: Mapping[str, str]) -> None:
    if env.get("GHCR_TOKEN"):
        skopeo.login("ghcr.io", env.get("GITHUB_ACTOR") or "github-actions", env["GHCR_TOKEN"])
    if env.get("DOCKERHUB_USERNAME") and env.get("DOCKERHUB_TOKEN"):
        skopeo.login("docker.io", env["DOCKERHUB_USERNAME"], env["DOCKERHUB_TOKEN"])


def main(env: Mapping[str, str]) -> int:
    pins = env.get("PINNED_IMAGES") or os.path.join(HERE, "..", "..", ".github", "pinned-images.tsv")
    skopeo = Skopeo(env)
    log_in(skopeo, env)
    mirror = Mirror(skopeo, float(env.get("MIRROR_COPY_SECONDS") or 1800))
    table = rows(pins)
    results = [mirror.row(*row) for row in table]
    if not table:
        say(f"::error::{pins} pins no image")
        return 1
    return 0 if all(results) else 1


if __name__ == "__main__":
    sys.exit(main(ci_machine.environment()))
