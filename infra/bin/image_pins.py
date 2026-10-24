#!/usr/bin/env python3
"""Every container image pin no other bumper owns, checked against its registry each week.

WHY THIS EXISTS. infra/bin/nix-update moves the Nix side (flake inputs, mongod), scala-steward.yml the sbt side (Gradle
has no bumper since Renovate was retired, 2026-10-24). Nothing looked at the container images: the Dockerfile's two
bases, the buildx BuildKit image, the Swift CI container, and the cluster's own components in movies-gitops (Flux,
kube-state-metrics, Reloader, Headlamp) -- which were found months behind on 2026-10-23 (docs/proposals/2026-10-23-dependency-and-flag-audit.md).

WHAT IT DOES. For each pin in PINS it reads the tag (and digest) from the file, lists the registry's tags over the
OCI distribution API (anonymous bearer token, paginated) and decides by the pin's policy:

  digest   the tag is a floating one (`27-jdk`): refresh the @sha256 to what the tag points at now.
  minor    take the newest tag within the current major.
  report   never rewrite; list what is newer (a new major, an OS release, a tag other files must move with).

Only pins in THIS checkout are rewritten; nix-update puts the rewrite in its one weekly pull request (nix-update/all,
the PR CLAUDE.md sanctions). Pins in the movies-gitops checkout (infra/kubernetes, gitignored here, put there by
infra/bin/fetch-gitops) are REPORTED in that PR's body and never rewritten: that repository gets no bot PRs (yet).

  infra/bin/image_pins.py                # the table, live; nothing written
  infra/bin/image_pins_test.py           # offline: tag choice, rewrites, the registry protocol, every real pin's shape

Imported by infra/bin/nix-update (`update(apply=True)`). Python 3.9 (scripts/run-script-tests' floor).
"""

import json
import pathlib
import re
import sys
import urllib.error
import urllib.parse
import urllib.request

ROOT = pathlib.Path(__file__).resolve().parents[2]  # the git checkout
GITOPS = ROOT / "infra" / "kubernetes"  # infra/bin/fetch-gitops' checkout of movies-gitops
USER_AGENT = "kinowo-infra-image-pins"

DOCKER_HUB = "https://registry-1.docker.io"
GHCR = "https://ghcr.io"
K8S = "https://registry.k8s.io"

SEMVER = r"v?(\d+)\.(\d+)\.(\d+)"


def image_ref(path):
    """`[registry/][namespace/]path:<tag>[@sha256:<digest>]`, bare or registry-qualified, as named groups."""
    return (r"(?<![\w.-])(?:[\w.-]+/)*" + re.escape(path)
            + r":(?P<tag>[\w][\w.-]*)(?:@(?P<digest>sha256:[0-9a-f]{64}))?(?![\w.-])")


class Pin:
    def __init__(self, name, files, registry, repository, policy, tag_key=SEMVER, pattern=None, root="repo", note=""):
        self.name, self.files, self.registry, self.repository = name, files, registry, repository
        self.policy, self.tag_key, self.root, self.note = policy, tag_key, root, note
        self.pattern = re.compile(pattern or image_ref(repository.split("/", 1)[1] if repository.startswith("library/")
                                                       else repository), re.M)

    def base(self, root, gitops):
        return root if self.root == "repo" else gitops


PINS = [
    Pin("eclipse-temurin", ["Dockerfile"], DOCKER_HUB, "library/eclipse-temurin", "digest", tag_key=r"(\d+)-jdk",
        note="JdkVersionParitySpec holds the major to CI's JDK: a new major is listed, never taken"),
    Pin("ubuntu", ["Dockerfile", "scripts/run-script-tests"], DOCKER_HUB, "library/ubuntu", "report",
        tag_key=r"(\d\d)\.(04)", note="LTS releases only; the runtime base, and CI's script-test container"),
    Pin("moby/buildkit", [".github/actions/setup-buildx/action.yml"], DOCKER_HUB, "moby/buildkit", "minor"),
    Pin("swift", [".github/workflows/ios.yml", "scripts/ios-compile-check.sh"], DOCKER_HUB, "library/swift", "report",
        tag_key=r"(\d+)\.(\d+)(?:\.(\d+))?", note="moves with ci.yml's Xcode/Swift pins (ActionPinningSpec)"),
    Pin("flux", ["flux/gotk-components.yaml"], GHCR, "fluxcd/flux-cli", "report", root="gitops",
        pattern=r"^# Flux Version: (?P<tag>v[\d.]+)$",
        note="regenerate flux/gotk-components.yaml with `flux install --export` of the new CLI; every controller moves with it"),
    Pin("kube-state-metrics", ["kube-state-metrics/deployment.yaml"], K8S, "kube-state-metrics/kube-state-metrics",
        "report", root="gitops", note="k3s.rules reads its metric names: read the release notes for renamed series"),
    Pin("reloader", ["reloader/deployment.yaml"], GHCR, "stakater/reloader", "report", root="gitops"),
    Pin("headlamp", ["headlamp/deployment.yaml"], GHCR, "headlamp-k8s/headlamp", "report", root="gitops"),
]


# ---- tags ----------------------------------------------------------------------------------------

def key(tag, tag_key):
    m = re.fullmatch(tag_key, tag)
    return tuple(int(g or 0) for g in m.groups()) if m else None


def choose(current, tags, tag_key):
    """(newest tag within current's major or None, newest tag of a later major or None)."""
    now = key(current, tag_key)
    if now is None:
        raise ValueError(f"the pinned tag {current!r} does not read as {tag_key}")
    keyed = sorted(((key(t, tag_key), t) for t in set(tags)), key=lambda kt: kt[0] or ())
    keyed = [(k, t) for k, t in keyed if k is not None]
    same = [t for k, t in keyed if k[0] == now[0] and k > now]
    later = [t for k, t in keyed if k[0] > now[0]]
    return (same[-1] if same else None), (later[-1] if later else None)


# ---- the files -----------------------------------------------------------------------------------

def read_pin(text, pattern):
    """(tag, digest or None) -- every occurrence in the file must agree. Refuses a file that names none."""
    found = {(m["tag"], m.groupdict().get("digest")) for m in pattern.finditer(text)}
    if not found:
        raise ValueError(f"names no {pattern.pattern}")
    if len(found) > 1:
        raise ValueError(f"names it more than one way: {sorted(found, key=str)}")
    return found.pop()


def rewrite(text, pattern, tag, digest=None):
    """Every occurrence's tag (and digest, when it has one) replaced."""
    def sub(m):  # a match ends at its tag, or at its digest when it has one
        out = m.group(0)[:m.start("tag") - m.start()] + tag
        return out + ("@" + (digest or m["digest"]) if m.groupdict().get("digest") else "")
    return pattern.sub(sub, text)


# ---- the registry (OCI distribution API) ---------------------------------------------------------

AUTH_PARAM = re.compile(r'(\w+)="([^"]*)"')
NEXT = re.compile(r'<([^>]+)>\s*;\s*rel="?next"?')
MANIFEST_TYPES = ", ".join([
    "application/vnd.oci.image.index.v1+json", "application/vnd.docker.distribution.manifest.list.v2+json",
    "application/vnd.oci.image.manifest.v1+json", "application/vnd.docker.distribution.manifest.v2+json"])


class Registry:
    """Anonymous pull access: a 401's `WWW-Authenticate: Bearer realm=..,service=..,scope=..` names the token."""

    def __init__(self, base):
        self.base, self.tokens = base.rstrip("/"), {}

    def _open(self, url, repository, method="GET", accept=None):
        for attempt in (0, 1):
            headers = {"User-Agent": USER_AGENT}
            if accept:
                headers["Accept"] = accept
            if repository in self.tokens:
                headers["Authorization"] = f"Bearer {self.tokens[repository]}"
            try:
                return urllib.request.urlopen(urllib.request.Request(url, headers=headers, method=method), timeout=60)
            except urllib.error.HTTPError as e:
                challenge = e.headers.get("WWW-Authenticate", "")
                e.close()
                if e.code != 401 or attempt or not challenge.lower().startswith("bearer"):
                    raise
                params = dict(AUTH_PARAM.findall(challenge))
                realm = params.pop("realm")
                with urllib.request.urlopen(urllib.request.Request(
                        realm + "?" + urllib.parse.urlencode(params), headers={"User-Agent": USER_AGENT}),
                        timeout=60) as r:
                    body = json.loads(r.read())
                self.tokens[repository] = body.get("token") or body["access_token"]

    def tags(self, repository):
        url, out = f"{self.base}/v2/{repository}/tags/list?n=1000", []
        while url:
            with self._open(url, repository) as r:
                out += json.loads(r.read()).get("tags") or []
                link = NEXT.search(r.headers.get("Link", ""))
            url = urllib.parse.urljoin(self.base + "/", link.group(1)) if link else None
        return out

    def digest(self, repository, tag):
        with self._open(f"{self.base}/v2/{repository}/manifests/{tag}", repository, "HEAD", MANIFEST_TYPES) as r:
            got = r.headers.get("Docker-Content-Digest")
        if not got:
            raise ValueError(f"{repository}:{tag} answered without a Docker-Content-Digest")
        return got


# ---- one pass ------------------------------------------------------------------------------------

def check(pin, root=ROOT, gitops=GITOPS, apply=False, registry=None):
    """{name, files, current, latest, later, action, note}; action is current, bumped, behind or held."""
    base = pin.base(root, gitops)
    texts = {f: (base / f).read_text() for f in pin.files}
    pins = {f: read_pin(t, pin.pattern) for f, t in texts.items()}
    if len(set(pins.values())) > 1:
        raise ValueError(f"the files disagree: {pins}")
    tag, digest = next(iter(pins.values()))
    registry = registry or Registry(pin.registry)
    same, later = choose(tag, registry.tags(pin.repository), pin.tag_key)
    row = {"name": pin.name, "files": [str(base.relative_to(root) / f) if base != root else f for f in pin.files],
           "current": tag + (f"@{digest[:19]}" if digest else ""), "latest": None, "later": later,
           "action": "current", "note": pin.note, "gitops": pin.root == "gitops"}
    new_tag, new_digest = tag, digest
    if pin.policy == "digest":
        fresh = registry.digest(pin.repository, tag)
        if fresh != digest:
            new_digest, row["latest"] = fresh, f"{tag}@{fresh[:19]}"
    elif same:
        new_tag, row["latest"] = same, same
    if row["latest"]:
        if pin.policy == "report" or pin.root != "repo":
            row["action"] = "behind"
        else:
            row["action"] = "bumped"
            if apply:
                for f, t in texts.items():
                    (base / f).write_text(rewrite(t, pin.pattern, new_tag, new_digest))
    elif later:
        row["action"] = "held"
    return row


def update(root=ROOT, gitops=GITOPS, apply=False):
    """(rows, failures): every pin checked; one that could not be read is a failure, never `current`."""
    rows, failures = [], []
    for pin in PINS:
        if pin.root == "gitops" and not (gitops / ".git").exists():
            failures.append(f"**{pin.name}**: no movies-gitops checkout at {gitops} (infra/bin/fetch-gitops) -- not checked")
            continue
        try:
            rows.append(check(pin, root, gitops, apply))
        except Exception as e:  # network or format: "could not look", never "up to date"
            failures.append(f"**{pin.name}**: could not check -- {e}")
    return rows, failures


def markdown(rows, failures):
    """The pull request body's section."""
    def latest(r):
        return (f"**{r['latest']}**" if r["latest"] else "=") + (f" (major: {r['later']})" if r["later"] else "")
    out = ["| pin | file | current | newest | |", "|---|---|---|---|---|"]
    for r in rows:
        what = {"bumped": "bumped here", "behind": "**bump by hand**" + (" in movies-gitops" if r["gitops"] else ""),
                "held": "newer major only", "current": "current"}[r["action"]]
        out.append(f"| {r['name']} | {', '.join(f'`{f}`' for f in r['files'])} | {r['current']} | {latest(r)} | {what} |")
    notes = [f"- {r['name']}: {r['note']}" for r in rows if r["note"] and r["action"] in ("behind", "held", "bumped")]
    return "\n".join(out + [""] + notes + [f"- {f}" for f in failures]) + "\n"


def main():
    rows, failures = update(apply=False)
    print(markdown(rows, failures))
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
