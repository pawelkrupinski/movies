#!/usr/bin/env python3
"""rotate the kinowo cluster Flux's GitHub deploy keys (read and write) for movies-gitops, keys first, old keys last.

Flux's GitRepositories `movies` (flux/gotk-sync.yaml) and `movies-write` (image-automation/automation.yaml) read
ssh://git@github.com/pawelkrupinski/movies-gitops.git with secretRef flux-git-read / flux-git-write. This mints a
fresh pair, hands it to Flux and only then removes the old one, so a run that dies half way leaves Flux's current key
working. (It began as the 2026-10-31 cut-over to GitHub, flux-cutover-github; with the cut-over done, what is left
is the rotation.)

  0. preflight (read-only): gh is logged in, GitHub main's two manifests name the GitHub URL and secretRefs, and
     (--apply) both GitRepositories already read that URL with those Secrets -- this rotates keys, it moves no source;
  1. builds known_hosts with ssh-keyscan of github.com run on k3s-worker-1 (the cluster's own view),
     and refuses keys whose fingerprints GitHub's /meta does not publish;
  2. generates a read and a write ed25519 deploy key in a temp dir (removed on exit);
  3. registers them on pawelkrupinski/movies-gitops (`gh repo deploy-key add`, the write one with
     --allow-write), titles stamped with the run's UTC time, beside the current flux keys;
  4. creates/updates Secrets flux-system/flux-git-read and flux-git-write (identity, identity.pub,
     known_hosts) by `kubectl apply -f -` on the k3s server, values over stdin;
  5. removes every earlier flux- deploy key, now that the Secrets hold the new ones;
  6. requests a reconcile and prints the state.

DRY RUN BY DEFAULT: without --apply it runs step 0's GitHub reads (nothing on the cluster) and prints what
steps 1-6 would do, including the deploy keys it would remove. --apply does it; re-running it rotates again.

Needs: gh logged in as the owner (repo admin on movies-gitops), ssh to monitoring-1 and k3s-worker-1
(--apply only), ssh-keygen. Tested by infra/bin/flux_rotate_deploy_keys_test.py (fake gh, ssh and ssh-keygen).

  infra/bin/flux-rotate-deploy-keys            # dry run
  infra/bin/flux-rotate-deploy-keys --apply"""
from __future__ import annotations

import base64
import json
import os
import shutil
import subprocess
import sys
import tempfile
import time
from typing import List, Mapping, Optional, Sequence

SLUG = "pawelkrupinski/movies-gitops"
GIT_URL = f"ssh://git@github.com/{SLUG}.git"
READ_TITLE = "flux-git-read (kinowo k3s)"
WRITE_TITLE = "flux-git-write (kinowo k3s)"
KUBECONFIG = "export KUBECONFIG=/etc/rancher/k3s/k3s.yaml; "
MY_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)), "flux-rotate-deploy-keys")


class Stop(Exception):
    """The run ends here: `line` on stderr (when there is one), then this exit status."""

    def __init__(self, line: str = "", code: int = 1) -> None:
        super().__init__(line)
        self.code = code


def say(line: str = "") -> None:
    print(line, flush=True)


def indented(text: str) -> None:
    for line in text.splitlines():
        say("  " + line)


def would(text: str) -> None:
    say(f"  would: {text}")


def run(argv: Sequence[str], *, stdin: Optional[str] = None, quiet: bool = False) -> str:
    """Stdout of the command; a failure ends the run with its status (the shell tool's `set -e`)."""
    done = subprocess.run(list(argv), input=stdin, stdin=None if stdin is not None else subprocess.DEVNULL,
                          stdout=subprocess.PIPE, text=True)
    if done.returncode != 0 and not quiet:
        raise Stop("", done.returncode)
    return done.stdout


class Rotation:
    def __init__(self, env: Mapping[str, str], apply: bool, work: str) -> None:
        self.apply, self.work = apply, work
        self.server = env.get("K8S_SERVER_HOST") or "monitoring-1"  # k3s server; kubectl runs there
        self.scanner = env.get("KEYSCAN_HOST") or "k3s-worker-1"  # a cluster node: known_hosts as the cluster sees github.com
        self.known_hosts = ""
        self.old_keys = ""

    def kube(self, command: str, stdin: Optional[str] = None) -> str:
        return run(["ssh", "-o", "BatchMode=yes", self.server, KUBECONFIG + command], stdin=stdin)

    def raw(self, path: str) -> str:
        return run(["gh", "api", "-H", "Accept: application/vnd.github.raw", f"repos/{SLUG}/contents/{path}?ref=main"])

    def preflight(self) -> None:
        say(f"== 0. preflight ({'APPLY' if self.apply else 'dry run: nothing changes'})")
        if subprocess.run(["gh", "auth", "status", "-h", "github.com"], stdout=subprocess.DEVNULL,
                          stderr=subprocess.DEVNULL).returncode != 0:
            raise Stop("gh is not logged in to github.com: gh auth login")
        say(run(["gh", "api", f"repos/{SLUG}", "--jq",
                 '"  repo \\(.full_name) (private: \\(.private)), default branch \\(.default_branch)"']).rstrip("\n"))
        manifests = {"gotk_sync": self.raw("flux/gotk-sync.yaml"), "automation": self.raw("image-automation/automation.yaml")}
        for name, text in manifests.items():
            if f"url: {GIT_URL}" not in text:
                raise Stop(f"GitHub main's {name} manifest does not point at {GIT_URL} -- fix movies-gitops' main first")
        for text, secret, where in ((manifests["gotk_sync"], "flux-git-read", "flux/gotk-sync.yaml"),
                                    (manifests["automation"], "flux-git-write", "automation.yaml")):
            if f"name: {secret}" not in text:
                raise Stop(f"GitHub main's {where} has no secretRef {secret}")
        say(f"  GitHub main's manifests point at {GIT_URL}")
        if self.apply:
            self.sources_are_on_github()
        self.list_old_keys()

    def sources_are_on_github(self) -> None:
        for repo, secret in (("movies", "flux-git-read"), ("movies-write", "flux-git-write")):
            current = self.kube(f"kubectl -n flux-system get gitrepository {repo} -o jsonpath='{{.spec.url}} {{.spec.secretRef.name}}'")
            current = current.rstrip("\n")
            if current != f"{GIT_URL} {secret}":
                raise Stop(f"gitrepository {repo} is '{current}', not '{GIT_URL} {secret}': this rotates keys, it moves no source")
        say(f"  both GitRepositories read {GIT_URL}")

    def list_old_keys(self) -> None:
        """Every flux key there now: removed after step 4 hands Flux the new ones."""
        self.old_keys = run(["gh", "repo", "deploy-key", "list", "-R", SLUG, "--json", "id,title,readOnly", "--jq",
                             '.[] | select(.title | startswith("flux-")) | "\\(.id)\\t\\(.title)\\t(read_only \\(.readOnly // "unknown"))"']).rstrip("\n")
        if self.old_keys:
            say("  deploy keys removed after the Secrets carry the new ones:")
            for line in self.old_keys.splitlines():
                say("    " + line)
        else:
            say("  no earlier flux deploy keys")

    def host_keys(self) -> None:
        say(f"== 1. known_hosts (ssh-keyscan github.com on {self.scanner})")
        if not self.apply:
            would(f"ssh {self.scanner} ssh-keyscan github.com, each key checked against gh api meta's ssh_key_fingerprints")
            return
        scanned = run(["ssh", "-o", "BatchMode=yes", self.scanner, "ssh-keyscan -t ed25519,ecdsa,rsa github.com 2>/dev/null"],
                      quiet=True)
        lines = [line for line in scanned.splitlines() if not line.startswith("#")]
        if not any(line.startswith("github.com ") for line in lines):
            raise Stop(f"ssh-keyscan from {self.scanner} returned no host keys for github.com")
        published = run(["gh", "api", "meta", "--jq", ".ssh_key_fingerprints | .[]"]).split()
        for line in lines:
            shown = run(["ssh-keygen", "-lf", "-"], stdin=line + "\n").split()
            fingerprint = shown[1].removeprefix("SHA256:") if len(shown) > 1 else ""
            if fingerprint not in published:
                raise Stop(f"github.com host key {fingerprint} (from {self.scanner}) is not one GitHub publishes")
            say(f"  {line.split()[1]} SHA256:{fingerprint} (matches api.github.com/meta)")
        self.known_hosts = "\n".join(lines) + "\n"

    def generate(self) -> None:
        say("== 2. generate deploy keys")
        for kind in ("read", "write"):
            run(["ssh-keygen", "-q", "-t", "ed25519", "-N", "", "-C", f"flux-git-{kind}@kinowo-k3s", "-f",
                 os.path.join(self.work, kind)])
            shown = run(["ssh-keygen", "-lf", os.path.join(self.work, kind + ".pub")]).split()
            say(f"  {kind}: {shown[1] if len(shown) > 1 else ''}")

    def register(self, stamp: str) -> None:
        say(f"== 3. register deploy keys on {SLUG}")
        for kind, title, extra in (("read", READ_TITLE, []), ("write", WRITE_TITLE, ["--allow-write"])):
            if self.apply:
                run(["gh", "repo", "deploy-key", "add", os.path.join(self.work, kind + ".pub"), "-R", SLUG,
                     "-t", f"{title} {stamp}", *extra])
            else:
                would(f"gh repo deploy-key add {kind}.pub{' ' if kind == 'read' else ''} -R {SLUG} -t '{title} {stamp}'"
                      + (" --allow-write" if extra else ""))

    def secret(self, kind: str) -> str:
        def b64(name: str) -> str:
            with open(os.path.join(self.work, name), "rb") as f:
                return base64.b64encode(f.read()).decode()
        return json.dumps({"apiVersion": "v1", "kind": "Secret", "type": "Opaque",
                           "metadata": {"name": f"flux-git-{kind}", "namespace": "flux-system"},
                           "data": {"identity": b64(kind), "identity.pub": b64(kind + ".pub"),
                                    "known_hosts": base64.b64encode(self.known_hosts.encode()).decode()}})

    def secrets(self) -> None:
        say("== 4. Secrets flux-system/flux-git-read and flux-git-write")
        for kind in ("read", "write"):
            if self.apply:
                indented(self.kube("kubectl apply -f -", stdin=self.secret(kind)))
            else:
                would(f"kubectl apply Secret flux-system/flux-git-{kind} (identity, identity.pub, known_hosts) on {self.server}, over stdin")

    def remove_old_keys(self) -> None:
        say("== 5. remove the deploy keys the Secrets no longer hold")
        for line in self.old_keys.splitlines():
            key_id, title = (line.split("\t") + ["", ""])[:2]
            if not key_id:
                continue
            if self.apply:
                run(["gh", "repo", "deploy-key", "delete", key_id, "-R", SLUG])
                say(f"  removed {title} (id {key_id})")
            else:
                would(f"gh repo deploy-key delete {key_id} -R {SLUG}   # {title}")

    def reconcile(self) -> None:
        say("== 6. reconcile")
        if not self.apply:
            would("annotate movies, movies-write, every Kustomization and imageupdateautomation/movies for a reconcile; wait 120 s for Ready")
            say()
            say("Dry run: nothing changed. Run again with --apply.")
            return
        now = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
        stamp = f"reconcile.fluxcd.io/requestedAt={now}"
        indented(self.kube(f"kubectl -n flux-system annotate --overwrite gitrepository/movies gitrepository/movies-write {stamp}"))
        try:
            indented(self.kube("kubectl -n flux-system wait gitrepository/movies gitrepository/movies-write --for=condition=Ready --timeout=120s"))
        except Stop:
            say("  sources not Ready within 120s -- see the status below")
        self.kube(f"kubectl -n flux-system annotate --overwrite kustomization --all {stamp}")
        self.kube(f"kubectl -n flux-system annotate --overwrite imageupdateautomation/movies {stamp}")
        say()
        say(self.kube("kubectl -n flux-system get gitrepositories,kustomizations").rstrip("\n"))
        say()
        say(self.kube("kubectl -n flux-system get imagerepositories,imagepolicies,imageupdateautomations").rstrip("\n"))
        say()
        say(f"Re-check: ssh {self.server} 'KUBECONFIG=/etc/rancher/k3s/k3s.yaml kubectl -n flux-system get gitrepositories,kustomizations,imageupdateautomations'")

    def run(self) -> None:
        self.preflight()
        self.host_keys()
        self.generate()
        self.register(time.strftime("%Y%m%dT%H%M%SZ", time.gmtime()))
        self.secrets()
        self.remove_old_keys()
        self.reconcile()


def main(argv: List[str], env: Mapping[str, str]) -> int:
    if argv[:1] in (["-h"], ["--help"]):
        print(__doc__)
        return 0
    if argv not in ([], ["--dry-run"], ["--apply"]):
        print(f"usage: {MY_PATH} [--dry-run | --apply]", file=sys.stderr)
        return 2
    for tool in ("gh", "ssh", "ssh-keygen"):
        if not shutil.which(tool):
            print(f"missing: {tool}", file=sys.stderr)
            return 1
    with tempfile.TemporaryDirectory() as work:
        try:
            Rotation(env, argv == ["--apply"], work).run()
        except Stop as e:
            if str(e):
                print(str(e), file=sys.stderr)
            return e.code
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], os.environ))
