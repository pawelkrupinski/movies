"""The pinned package nix-update bumps itself: mongod (infra/bin/nix-update; its tests are nix-update's `--self-test`).

roles/mongodb.nix names the release once (`mongodbVersion = "x.y.z";`) and four tarballs, one
`url = "...${mongodbVersion}...";` + `hash = "sha256-...";` pair per platform. Every url is the
file's own template with the new version put in, so the file and this script cannot disagree
about where the bytes come from."""

from __future__ import annotations

import base64
import dataclasses
import hashlib
import json
import pathlib
import re
import subprocess
import urllib.error
import urllib.request
from typing import Dict, List, Optional, Pattern, Sequence, Tuple

REPO = pathlib.Path(__file__).resolve().parents[1]  # infra/, the flake root
ROOT = REPO.parent  # the git checkout
USER_AGENT = "kinowo-infra-nix-update"


def request(url: str) -> urllib.request.Request:
    return urllib.request.Request(url, headers={"User-Agent": USER_AGENT})


def fetch(url: str) -> bytes:
    with urllib.request.urlopen(request(url), timeout=300) as response:
        return response.read()


def sha256_of(url: str) -> str:
    """The sha256 of url's bytes, streamed: a mongod tarball is a few hundred MB."""
    digest = hashlib.sha256()
    with urllib.request.urlopen(request(url), timeout=300) as response:
        for chunk in iter(lambda: response.read(1 << 20), b""):
            digest.update(chunk)
    return digest.hexdigest()


def run(argv: Sequence[str], cwd: pathlib.Path) -> Tuple[int, str, str]:
    result = subprocess.run(argv, cwd=cwd, capture_output=True, text=True)
    return result.returncode, result.stdout, result.stderr


def version_key(v: str) -> Tuple[int, ...]:
    return tuple(int(p) for p in v.split("."))


def is_release(v: str) -> bool:
    return re.fullmatch(r"\d+(\.\d+)*", v) is not None


def choose(current: str, available: Sequence[str]) -> Tuple[List[str], Optional[str]]:
    """([every release in current's major newer than it, newest first], newest of a later major or None)."""
    releases = sorted({v for v in available if is_release(v)}, key=version_key, reverse=True)
    major = version_key(current)[0]
    same = [v for v in releases if version_key(v)[0] == major and version_key(v) > version_key(current)]
    later = [v for v in releases if version_key(v)[0] > major]
    return same, (later[0] if later else None)


MONGODB_FILE = "nix/modules/roles/mongodb.nix"
# The containers MongoServerVersionParitySpec holds to the same release (paths from the checkout).
# CI's is the `mongo` row of .github/pinned-images.tsv (tag AND digest; CiPinnedImageSpec), which
# start_mongo_replset.py reads, its native /opt/mongodb/<tag> home included.
MONGODB_TAG_FILES = (".github/pinned-images.tsv", "scripts/convergence_local.py")
PINNED_IMAGES = ".github/pinned-images.tsv"

MONGODB_VERSION_LINE = re.compile(r'^(\s*mongodbVersion = ")([^"]+)(";)', re.M)
URL_HASH = re.compile(r'(url = ")([^"]+)(";\s*\n\s*hash = ")(sha256-[A-Za-z0-9+/=]+)(";)')


def mongo_tag(version: str) -> Pattern[str]:
    # The spec's own pattern: bare or registry-qualified `mongo:<tag>`.
    return re.compile(r"(?<![\w.-])((?:[\w.-]+/)*mongo:)" + re.escape(version) + r"(?![\w.])")


def mongodb_shape(text: str) -> Tuple[str, List[Tuple[str, str]]]:
    """(current version, [(url template, hash)]). Refuses any other shape."""
    versions = MONGODB_VERSION_LINE.findall(text)
    if len(versions) != 1:
        raise ValueError(f"expected exactly one `mongodbVersion = \"...\";` line, found {len(versions)}")
    pairs = [(m.group(2), m.group(4)) for m in URL_HASH.finditer(text)]
    if len(pairs) != 4 or any("${mongodbVersion}" not in url for url, _ in pairs):
        raise ValueError("expected four `url = \"...${mongodbVersion}...\"; hash = \"sha256-...\";` pairs")
    return versions[0][1], pairs


def rewrite_mongodb(text: str, version: str, hashes: Dict[str, str]) -> str:
    """The file with its version and each url's hash replaced; hashes is {url template: sri}."""
    mongodb_shape(text)
    text = MONGODB_VERSION_LINE.sub(lambda m: m.group(1) + version + m.group(3), text)
    return URL_HASH.sub(lambda m: m.group(1) + m.group(2) + m.group(3) + hashes[m.group(2)] + m.group(5), text)


def rewrite_tags(text: str, current: str, version: str) -> str:
    """Every `mongo:<current>` made `<version>`. Refuses a file naming no tag."""
    pattern = mongo_tag(current)
    if not pattern.search(text):
        raise ValueError(f"names no mongo:{current}")
    return pattern.sub(lambda m: m.group(1) + version, text)


def mongodb_releases() -> Dict[str, Dict[str, Optional[str]]]:
    """{version: {archive url: published sha256}} for every production release."""
    listing = json.loads(fetch("https://downloads.mongodb.org/full.json"))
    out: Dict[str, Dict[str, Optional[str]]] = {}
    for release in listing["versions"]:
        if not release.get("production_release"):
            continue
        out[release["version"]] = {d["archive"]["url"]: d["archive"].get("sha256")
                                   for d in release.get("downloads", []) if "archive" in d}
    return out


def _get(url: str, timeout: float) -> bytes:
    with urllib.request.urlopen(request(url), timeout=timeout) as response:
        return response.read()


def _tag_digest(version: str) -> Optional[str]:
    """The digest field of Docker Hub's tag document, None for a tag it does not have."""
    try:
        document = _get(f"https://hub.docker.com/v2/repositories/library/mongo/tags/{version}", 60)
    except urllib.error.HTTPError as e:
        if e.code == 404:
            return None
        raise
    return json.loads(document).get("digest") or ""


def docker_hub_digest(version: str) -> Optional[str]:
    """The digest Docker Hub's `mongo:<version>` points at (its index), None when there is no such tag."""
    digest = _tag_digest(version)
    if digest is None:
        return None
    if not re.fullmatch(r"sha256:[0-9a-f]{64}", digest):
        raise ValueError(f"Docker Hub's mongo:{version} names no digest ({digest!r})")
    return digest


def rewrite_pinned_digest(text: str, version: str, digest: str) -> str:
    """The pins table with `mongo:<version>`'s digest made `digest` (after rewrite_tags moved the tag)."""
    pattern = re.compile(r"((?:[\w.-]+/)*mongo:" + re.escape(version) + r")@sha256:[0-9a-f]{64}")
    if not pattern.search(text):
        raise ValueError(f"pins no mongo:{version}@sha256:<digest>")
    return pattern.sub(lambda m: m.group(1) + "@" + digest, text)


def header_comment(text: str, line_pattern: Pattern[str]) -> str:
    """The comment block right above the pinned line: why it is pinned."""
    found = line_pattern.search(text)
    assert found is not None  # the pinned line is there: mongodb_shape found it first
    lines = text[:found.start()].rstrip("\n").splitlines()
    out: List[str] = []
    for line in reversed(lines):
        if not line.strip().startswith("#"):
            break
        out.append(line.strip().lstrip("#").strip())
    return " ".join(reversed(out))


@dataclasses.dataclass
class MongodbBump:
    """What bump_mongodb found: the pin, the version to take (None when nothing newer is proposable), the newer major
    it holds back, and the PR's wording."""
    name: str
    path: str
    current: str
    take: Optional[str]
    later: Optional[str]
    header: str
    notes: List[str]
    checked: str = ""


class Refused(Exception):
    """A pinned package's new bytes did not match upstream's checksum."""


def _newest_on_docker_hub(newer: Sequence[str], notes: List[str]) -> Tuple[Optional[str], Optional[str]]:
    """(the newest release Docker Hub also has, its digest); a release it lacks is said in `notes`."""
    for v in newer:
        digest = docker_hub_digest(v)
        if digest:
            return v, digest
        notes.append(f"mongodb {v}: Docker Hub has no `mongo:{v}` yet, and CI's mongod must run "
                     "the same release (MongoServerVersionParitySpec) -- not proposed")
    return None, None


def _verified_hash(template: str, take: str, releases: Dict[str, Dict[str, Optional[str]]]) -> str:
    """The sri hash of the tarball a url template names for `take`, after checking it against MongoDB's own listing."""
    url = template.replace("${mongodbVersion}", take)
    want = releases[take].get(url)
    if not want:
        raise Refused(f"MongoDB's listing for {take} has no {url} (the file's url template no longer matches upstream)")
    digest = sha256_of(url)
    if digest != want:
        raise Refused(f"downloaded {url} hashes to {digest} but MongoDB's listing says {want}")
    return "sha256-" + base64.b64encode(bytes.fromhex(digest)).decode()


def _write_bump(text: str, current: str, take: str, digest: str, hashes: Dict[str, str]) -> None:
    tag_files = {p: (ROOT / p).read_text() for p in MONGODB_TAG_FILES}
    rewritten = {p: rewrite_tags(t, current, take) for p, t in tag_files.items()}
    rewritten[PINNED_IMAGES] = rewrite_pinned_digest(rewritten[PINNED_IMAGES], take, digest)
    (REPO / MONGODB_FILE).write_text(rewrite_mongodb(text, take, hashes))
    for p, t in rewritten.items():
        (ROOT / p).write_text(t)


def bump_mongodb() -> MongodbBump:
    """Bump mongod in place when its major has a newer release Docker Hub also has."""
    text = (REPO / MONGODB_FILE).read_text()
    current, pairs = mongodb_shape(text)
    releases = mongodb_releases()
    if not releases:
        raise ValueError("upstream listed no releases at all, which is not the same as none being newer")
    newer, later = choose(current, list(releases))
    found = MongodbBump("mongodb", f"infra/{MONGODB_FILE}", current, None, later,
                        header_comment(text, MONGODB_VERSION_LINE), [])
    take, digest = _newest_on_docker_hub(newer, found.notes)
    if not take or not digest:
        return found
    found.take = take
    hashes = {template: _verified_hash(template, take, releases) for template, _ in pairs}
    _write_bump(text, current, take, digest, hashes)
    found.checked = "each tarball matches MongoDB's published sha256; `mongo:` tags in " + \
        ", ".join(f"`{p}`" for p in MONGODB_TAG_FILES) + f" moved with it (CI's pin to Docker Hub's {digest}; " \
        "mirror-pinned-images.yml copies it to GHCR once this lands, CI pulling it from mirror.gcr.io until then)"
    # start_mongo_replset.py looks for a job image's native mongod at /opt/mongodb/<the pin's tag>
    found.notes.append(f"mongodb {take}: a CI job image baking its native mongod at `/opt/mongodb/{current}` "
                       "(bitcashier/infra `nix/files/movies-job-image/Dockerfile`) must bake "
                       f"`/opt/mongodb/{take}` too; until then CI starts the `mongo:{take}` container (slower, same release)")
    return found
