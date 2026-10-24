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

TWO PULL REQUESTS, ONE PER REPOSITORY (both CLAUDE.md's sanctioned bot PRs). This checkout's pins are rewritten by
infra/bin/nix-update into its weekly PR (nix-update/all). The pins in the movies-gitops checkout (infra/kubernetes,
gitignored here, put there by infra/bin/fetch-gitops) are rewritten by `--gitops-out` into that repository's own PR
(branch GITOPS_BRANCH, opened by infra/bin/propose-pr): a merge there IS the deploy (Flux reconciles every add-on
directory), so the reviewer is the gate. Each body lists the other repository's pins too, saying where they move.
Only a plain image-tag bump is made by machine: the upstream release's RBAC, args or CRDs are NOT carried, so each
body says to read the release notes. Flux stays `report`: its pin is a generated manifest (`flux install --export`),
not a tag, and its controllers are applied by hand (movies-gitops' README).

  infra/bin/image_pins.py                    # the table, live; nothing written
  infra/bin/image_pins.py --gitops-out DIR   # rewrite movies-gitops' pins in place; DIR gets title, body.md, changes.patch
  infra/bin/image_pins_test.py               # offline: tag choice, rewrites, the registry protocol, every real pin's shape

Imported by infra/bin/nix-update (`update(write={"repo"})`). Python 3.9 (scripts/run-script-tests' floor).
"""

import argparse
import difflib
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
GITOPS_BRANCH = "image-pins/all"  # the movies-gitops PR's branch, rebuilt from its main every week
K3S_RULES = "infra/nix/files/monitoring/rules/k3s.rules"  # the alert rules that read kube-state-metrics' series

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
        "minor", root="gitops", note="k3s.rules reads its metric names: read the release notes for renamed series"),
    Pin("reloader", ["reloader/deployment.yaml"], GHCR, "stakater/reloader", "minor", root="gitops",
        note="only the image moves: compare the release's RBAC and flags with reloader/deployment.yaml"),
    Pin("headlamp", ["headlamp/deployment.yaml"], GHCR, "headlamp-k8s/headlamp", "minor", root="gitops",
        note="only the image moves: compare the release's RBAC and args with headlamp/"),
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

def check(pin, root=ROOT, gitops=GITOPS, write=(), registry=None):
    """{name, files, own_files, current, latest, later, action, note, gitops}; action is current, bumped, behind or held.

    `bumped` is the pin's own repository's pull request moving it; the files are rewritten only when that repository
    ("repo" or "gitops") is in `write` -- nix-update writes this checkout, `--gitops-out` movies-gitops."""
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
           "own_files": pin.files, "action": "current", "note": pin.note, "gitops": pin.root == "gitops"}
    new_tag, new_digest = tag, digest
    if pin.policy == "digest":
        fresh = registry.digest(pin.repository, tag)
        if fresh != digest:
            new_digest, row["latest"] = fresh, f"{tag}@{fresh[:19]}"
    elif same:
        new_tag, row["latest"] = same, same
    if row["latest"]:
        if pin.policy == "report":
            row["action"] = "behind"
        else:
            row["action"] = "bumped"
            if pin.root in write:
                for f, t in texts.items():
                    (base / f).write_text(rewrite(t, pin.pattern, new_tag, new_digest))
    elif later:
        row["action"] = "held"
    return row


def update(root=ROOT, gitops=GITOPS, write=(), roots=("repo", "gitops"), pins=None):
    """(rows, failures): every pin of `roots` checked; one that could not be read is a failure, never `current`."""
    rows, failures = [], []
    for pin in (p for p in (PINS if pins is None else pins) if p.root in roots):
        if pin.root == "gitops" and not (gitops / ".git").exists():
            failures.append(f"**{pin.name}**: no movies-gitops checkout at {gitops} (infra/bin/fetch-gitops) -- not checked")
            continue
        try:
            rows.append(check(pin, root, gitops, write))
        except Exception as e:  # network or format: "could not look", never "up to date"
            failures.append(f"**{pin.name}**: could not check -- {e}")
    return rows, failures


def markdown(rows, failures, here="repo"):
    """A pull request body's section; `here` is the repository the PR is in ("repo" or "gitops")."""
    def latest(r):
        return (f"**{r['latest']}**" if r["latest"] else "=") + (f" (major: {r['later']})" if r["later"] else "")

    def ours(r):
        return r["gitops"] == (here == "gitops")

    def bumped(r):
        if ours(r):
            return "bumped here"
        return f"bumped in movies-gitops' PR (`{GITOPS_BRANCH}`)" if r["gitops"] else "bumped in movies' nix-update PR"
    out = ["| pin | file | current | newest | |", "|---|---|---|---|---|"]
    for r in rows:
        what = {"bumped": bumped(r), "behind": "**bump by hand**" + (" in movies-gitops" if r["gitops"] else ""),
                "held": "newer major only", "current": "current"}[r["action"]]
        files = r["own_files"] if ours(r) else r["files"]  # a path in the PR's own repository reads from its root
        out.append(f"| {r['name']} | {', '.join(f'`{f}`' for f in files)} | {r['current']} | {latest(r)} | {what} |")
    notes = [f"- {r['name']}: {r['note']}" for r in rows if r["note"] and r["action"] in ("behind", "held", "bumped")]
    return "\n".join(out + [""] + notes + [f"- {f}" for f in failures]) + "\n"


# ---- movies-gitops' pull request ---------------------------------------------------------------

def k3s_series(root=ROOT):
    """Every kube-state-metrics series (`kube_*`) the alert rules read, sorted."""
    return sorted(set(re.findall(r"\bkube_[a-z_]+\b", (root / K3S_RULES).read_text())))


def gitops_body(rows, failures, root=ROOT):
    """The movies-gitops PR's body: the table, and what a reviewer must check before merging."""
    bumped = [r for r in rows if r["gitops"] and r["action"] == "bumped"]
    out = ["Weekly container image pins (`infra/bin/image_pins.py` in pawelkrupinski/movies, run by its "
           "`.github/workflows/nix-update.yml`): the newest tag within each component's current major.", "",
           "**Merging this deploys it**: Flux reconciles each of these directories. Only the `image:` tag moved -- "
           "an upstream release's RBAC, flags or CRDs are not carried, so read each release's notes first. To leave "
           f"one out, push a commit to `{GITOPS_BRANCH}` reverting it: the weekly run will not overwrite a branch with "
           "commits by anyone but ci-bot.", ""]
    if any(r["name"] == "kube-state-metrics" for r in bumped):
        out += ["> [!WARNING]",
                f"> **kube-state-metrics moved: check `k3s.rules`' series names** (`{K3S_RULES}` in movies) against the "
                "release notes before merging. A renamed or removed series makes its alert silent, not red. It reads: "
                + ", ".join(f"`{s}`" for s in k3s_series(root)), ""]
    return "\n".join(out) + "\n" + markdown(rows, failures, here="gitops")


def propose_gitops(out, root=ROOT, gitops=GITOPS, pins=None):
    """Rewrite movies-gitops' pins in its checkout and write out/{title, body.md, changes.patch} for propose-pr.

    No bump, no patch (propose-pr then proposes nothing). Returns the failures."""
    files = sorted({f for p in (PINS if pins is None else pins) if p.root == "gitops" for f in p.files})
    before = {f: (gitops / f).read_text() for f in files if (gitops / f).exists()}
    rows, failures = update(root, gitops, write={"gitops"}, roots=("gitops",), pins=pins)
    bumped = [r for r in rows if r["action"] == "bumped"]
    out.mkdir(parents=True, exist_ok=True)
    # A plain unified diff, `git apply`'s -p1 form, made here rather than by `git diff`: no git environment (a hook's
    # GIT_DIR) can point it at another repository.
    patch = "".join("".join(difflib.unified_diff(text.splitlines(True), (gitops / f).read_text().splitlines(True),
                                                 f"a/{f}", f"b/{f}")) for f, text in before.items())
    (out / "changes.patch").write_text(patch)
    (out / "title").write_text("Image pins: " + ", ".join(f"{r['name']} {r['latest']}" for r in bumped) + "\n"
                               if bumped else "Image pins: nothing to bump\n")
    (out / "body.md").write_text(gitops_body(rows, failures, root))
    return failures


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("--gitops-out", type=pathlib.Path, metavar="DIR",
                        help="rewrite movies-gitops' pins in place and write its PR's title, body.md and changes.patch")
    args = parser.parse_args()
    if args.gitops_out:
        failures = propose_gitops(args.gitops_out)
        print((args.gitops_out / "title").read_text() + (args.gitops_out / "body.md").read_text())
    else:
        rows, failures = update()
        print(markdown(rows, failures))
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
