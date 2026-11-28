"""`nix-update --self-test` (infra/bin/nix-update-test.sh runs it): offline checks of the version choice, the file
rewrites and every pin's shape, against the real files of this tree."""

from __future__ import annotations

import pathlib
import tempfile
from typing import Dict, List, Optional, Tuple
from unittest import mock

import nix_update_mongodb
from nix_update_body import ANSI, DIFF_LINE, PinnedUpgrades, Upgrade, changed, pull_request_body
from nix_update_mongodb import (MONGODB_FILE, MONGODB_TAG_FILES, MONGODB_VERSION_LINE, PINNED_IMAGES, REPO, ROOT,
                                MongodbBump, bump_mongodb, choose, header_comment, mongodb_shape, rewrite_mongodb,
                                rewrite_pinned_digest, rewrite_tags)

def expect(failures: List[str], what: str, got: object, want: object) -> None:
    if got != want:
        failures.append(f"{what}: got {got!r}, want {want!r}")


def check_choice(failures: List[str]) -> None:
    expect(failures, "same major only", choose("8.3.11", ["8.2.9", "8.3.11", "8.3.13", "8.3.12", "8.4.0", "9.0.4", "9.0.2"]),
           (["8.4.0", "8.3.13", "8.3.12"], "9.0.4"))
    expect(failures, "nothing newer", choose("8.3.11", ["8.3.11", "8.2.9"]), ([], None))
    expect(failures, "prereleases ignored", choose("8.3.11", ["8.3.12-rc0", "8.3.12"]), (["8.3.12"], None))
    expect(failures, "major only newer", choose("8.3.13", ["8.3.13", "9.0.0"]), ([], "9.0.0"))


def check_mongodb_rewrites(failures: List[str]) -> None:
    sample = ('  # Pinned ahead of nixpkgs.\n  mongodbVersion = "8.3.11";\n  x = {\n'
              + "".join(f'    "{p}" = pkgs.fetchurl {{\n      url = "https://h/{p}-${{mongodbVersion}}.tgz";\n'
                        f'      hash = "sha256-{p}=";\n    }};\n' for p in ("a", "b", "c", "d")) + "  };\n")
    expect(failures, "shape", mongodb_shape(sample)[0], "8.3.11")
    new = rewrite_mongodb(sample, "8.3.13", {f"https://h/{p}-${{mongodbVersion}}.tgz": f"sha256-N{p}=" for p in "abcd"})
    expect(failures, "rewrite", new, sample.replace('"8.3.11"', '"8.3.13"').replace("sha256-a=", "sha256-Na=")
           .replace("sha256-b=", "sha256-Nb=").replace("sha256-c=", "sha256-Nc=").replace("sha256-d=", "sha256-Nd="))
    for broken, why in ((sample.replace("  mongodbVersion", "  # mongodbVersion"), "no version line"),
                        (sample.replace("${mongodbVersion}.tgz\";\n      hash = \"sha256-d=", "8.3.11.tgz\";\n      hash = \"sha256-d="),
                         "a url not built from the version")):
        try:
            mongodb_shape(broken)
            failures.append(f"mongodb_shape accepted a file with {why}")
        except ValueError:
            pass
    expect(failures, "header", header_comment("x\n  # Why one,\n  # why two.\n  mongodbVersion = \"1\";\n", MONGODB_VERSION_LINE),
           "Why one, why two.")


def check_pins_and_diffs(failures: List[str]) -> None:
    expect(failures, "tags", rewrite_tags("image=mirror.gcr.io/library/mongo:8.3.11\nrun mongo:8.3.11 --x\nmongo:8.3.111\n",
                                          "8.3.11", "8.3.13"),
           "image=mirror.gcr.io/library/mongo:8.3.13\nrun mongo:8.3.13 --x\nmongo:8.3.111\n")
    pin = "mongo\tdocker.io/library/mongo:8.3.13@sha256:" + "a" * 64 + "\tghcr.io/o/mongo\n"
    expect(failures, "pinned digest", rewrite_pinned_digest(pin, "8.3.13", "sha256:" + "b" * 64), pin.replace("a" * 64, "b" * 64))
    expect(failures, "diff line", changed("openssl: 3.5.1 → 3.5.2, +4.1 KiB"), "3.5.1 → 3.5.2")
    expect(failures, "coloured size dropped", changed(ANSI.sub("", "libgcrypt: 1.12.2 → 1.12.4, \x1b[31;1m+9.6 KiB\x1b[0m")),
           "1.12.2 → 1.12.4")
    expect(failures, "size-only line ignored", DIFF_LINE.match("openssl: +4.1 KiB"), None)


def check_body(failures: List[str]) -> None:
    """A taken bump reaches the pull request's body, with the reason the package is pinned."""
    bump = MongodbBump("mongodb", "infra/mongodb.nix", "8.3.11", "8.3.13", None, "Pinned ahead.", [], "checked")
    upgrade = Upgrade(PinnedUpgrades(bumped=[bump]), {}, {}, [], [], [], ["mongo-1"], {}, {}, "")
    body = pull_request_body(upgrade)
    expect(failures, "body names the pinned package", "Why <code>mongodb</code> is pinned" in body and "> Pinned ahead." in body, True)


RELEASES: Dict[str, Dict[str, Optional[str]]] = {"8.3.13": {f"https://h/{p}-8.3.13.tgz": f"{ord(p):064x}" for p in "abcd"}}
DOCKER_DIGEST = "sha256:" + "d" * 64


def _lay_out(root: pathlib.Path, files: Dict[str, str]) -> None:
    for name, text in files.items():
        (root / name).parent.mkdir(parents=True, exist_ok=True)
        (root / name).write_text(text)


def _published_hash(url: str) -> str:
    return RELEASES["8.3.13"][url] or ""


def _bump_in(files: Dict[str, str]) -> Tuple[MongodbBump, str]:
    """bump_mongodb over a scratch checkout of `files`, MongoDB and Docker Hub answering from RELEASES and
    DOCKER_DIGEST: (what it found, the pins table afterwards)."""
    with tempfile.TemporaryDirectory() as tmp:
        root = pathlib.Path(tmp)
        _lay_out(root, files)
        with mock.patch.object(nix_update_mongodb, "REPO", root / "infra"), mock.patch.object(nix_update_mongodb, "ROOT", root), \
                mock.patch.object(nix_update_mongodb, "mongodb_releases", lambda: RELEASES), \
                mock.patch.object(nix_update_mongodb, "docker_hub_digest", lambda version: DOCKER_DIGEST), \
                mock.patch.object(nix_update_mongodb, "sha256_of", _published_hash):
            return bump_mongodb(), (root / PINNED_IMAGES).read_text()


def check_bump(failures: List[str]) -> None:
    """A taken bump writes Docker Hub's digest, not a tarball's hash, into the pins table CI pulls `mongo:<tag>` by."""
    sample = ('  mongodbVersion = "8.3.11";\n'
              + "".join(f'  x{p} = {{\n    url = "https://h/{p}-${{mongodbVersion}}.tgz";\n    hash = "sha256-{p}=";\n  }};\n'
                        for p in "abcd"))
    bump, pins = _bump_in({"infra/" + nix_update_mongodb.MONGODB_FILE: sample,
                           PINNED_IMAGES: "mongo\tdocker.io/library/mongo:8.3.11@sha256:" + "a" * 64 + "\tghcr.io/o/mongo\n",
                           "scripts/convergence_local.py": "MONGO = 'mongo:8.3.11'\n"})
    expect(failures, "bump takes", bump.take, "8.3.13")
    expect(failures, "pins table digest", pins, f"mongo\tdocker.io/library/mongo:8.3.13@{DOCKER_DIGEST}\tghcr.io/o/mongo\n")


def check_real_files(failures: List[str]) -> None:
    """The real files keep the shape the weekly run rewrites, so a file reshaped by hand fails here (on every push, via
    scripts/run-script-tests) rather than on next Monday's run."""
    try:
        current, _ = mongodb_shape((REPO / MONGODB_FILE).read_text())
        header_comment((REPO / MONGODB_FILE).read_text(), MONGODB_VERSION_LINE)
        for p in MONGODB_TAG_FILES:
            try:
                moved = rewrite_tags((ROOT / p).read_text(), current, "0.0.0")
                if p == PINNED_IMAGES:
                    rewrite_pinned_digest(moved, "0.0.0", "sha256:" + "0" * 64)
            except ValueError as e:
                failures.append(f"{p}: {e}")
    except ValueError as e:
        failures.append(f"infra/{MONGODB_FILE}: {e}")


def self_test() -> int:
    failures: List[str] = []
    for check in (check_choice, check_mongodb_rewrites, check_pins_and_diffs, check_body, check_bump, check_real_files):
        check(failures)
    for f in failures:
        print(f"FAIL {f}")
    print(f"{'FAILED' if failures else 'ok'}: self-test, {len(failures)} failure(s)")
    return 1 if failures else 0
