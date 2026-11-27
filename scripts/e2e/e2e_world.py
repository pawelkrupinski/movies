"""THE TOOLS' END-TO-END HARNESS (the owner, 2026-11-27: "for the important Python scripts, also add end-to-end tests").
The black-box tests (scripts/lib/cli_harness.py) drive ONE tool with fakes around it; this runs the REAL tools
TOGETHER, through their real chain -- `git push` running the installed pre-push hook running run-script-tests running a
self-test -- each as a subprocess, with stubs only at the machine's edge. Modelled on agentctl's tests/e2e/.

ONE temp root per scenario (`World`, removed at its end) holds every piece:
- HOME, TMPDIR and the caches (XDG_CACHE_HOME, the tools' ~/.cache/kinowo-*) inside it, so no cache, store or log of
  the laptop is read or written;
- REAL git repositories: a bare "origin" and a clone with worktrees (`toy_repo`), or a clone of this very checkout
  (`movies_clone`: the tools under test as they are in the working tree, the big fixtures left out);
- SHIMS first on PATH, each logging its argv to one calls file: `gh` (scripts/e2e/gh_shim.py -> the stub GitHub, an
  HTTP server, scripts/e2e/gh_stub.py), `ssh` / `nix` / `kubectl` / `sbt` (a scenario's canned host, never a real
  one: unless a scenario says otherwise each refuses), and `agentctl` (a stub at ~/bin/agentctl and on PATH: the live
  agentctl store is never reached);
- the isolation of cli_harness.isolated_env: no GIT_*, GITHUB_*, AGENTCTL_* or PREPUSH_* of the run around it, a fixed
  git identity, unsigned commits, the governor and the JVM gate off inside (the scenario itself runs under
  run-script-tests' ticket, one core of it).
Every step is bounded (STEP_BOUND): its whole process group is ended past it and the step fails, never hangs.

THE MUTATION PROOF (scripts/e2e/mutations): with $E2E_MUTATION set ({"file", "old", "new"}: one exact replacement,
which must match once), every tool a scenario runs comes from a copy of the tool trees with it applied -- `tool()`,
and the tree `movies_clone()` commits -- and that scenario must then FAIL."""
from __future__ import annotations

import json
import os
import shutil
import signal
import subprocess
import sys
import tempfile
import unittest
from typing import Any, Dict, List, Mapping, Optional, Sequence

HERE = os.path.dirname(os.path.realpath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
sys.path.insert(0, os.path.join(ROOT, "scripts", "lib"))
sys.path.insert(0, HERE)
import cli_harness  # noqa: E402

STEP_BOUND = float(os.environ.get("TOOLS_E2E_STEP_BOUND") or 240)
# The trees a tool runs from: what a mutation's copy holds, and what movies_clone lays over its checkout
TOOL_TREES = ("scripts", "infra/bin", ".github/scripts", ".github/actions", "docs/tools.md")
# Left out of movies_clone's checkout: ~1.1 GB of recordings no tool under test reads
SPARSE = ("/*", "!/test/resources/fixtures/")
REFUSING_SHIM = "err(f'{NAME}: the e2e world has no {NAME} here (argv {argv})'); sys.exit(97)"
# agentctl's laptop CLI as the hook meets it (~/bin/agentctl prepush-check: the push returns no file to an older main):
# it agrees, and the call is logged; any other command is refused -- the live store is never reached
AGENTCTL_SHIM = "sys.exit(0) if argv[:1] == ['prepush-check'] else None\n" + REFUSING_SHIM
SHIM_PRELUDE = """#!{python} -B
import json, os, sys
NAME = {name!r}
argv = sys.argv[1:]
stdin = "" if sys.stdin is None or sys.stdin.isatty() else sys.stdin.read()
with open(os.environ["E2E_CALLS"], "a") as _log:
    _log.write(json.dumps({{"name": NAME, "argv": argv, "cwd": os.getcwd(), "stdin": stdin[:4000]}}) + "\\n")
def out(text, end="\\n"):
    sys.stdout.write(text + end)
def err(text, end="\\n"):
    sys.stderr.write(text + end)
"""


# Set in every world's tools: a self-test run inside a world (the pre-push scenario's run-script-tests) is never an e2e
# scenario again -- a world inside a world, each clone pushing the next
INSIDE = "TOOLS_E2E_INSIDE"


class StepFailed(AssertionError):
    """A step that could not be made (a setup git command, a step past its bound)."""


def mutation() -> Optional[Dict[str, str]]:
    raw = os.environ.get("E2E_MUTATION")
    return json.loads(raw) if raw else None


def mutate(tree: str, m: Mapping[str, str]) -> None:
    path = os.path.join(tree, m["file"])
    with open(path) as f:
        text = f.read()
    if text.count(m["old"]) != 1:
        raise StepFailed(f"the mutation of {m['file']} matches {text.count(m['old'])} times, not once "
                         "(scripts/e2e/mutations is stale)")
    with open(path, "w") as f:
        f.write(text.replace(m["old"], m["new"]))


def working_files(source: str, prefixes: Sequence[str]) -> List[str]:
    """The files of `prefixes` in `source`'s working tree, tracked or new (ignored ones left out)."""
    done = subprocess.run(["git", "-C", source, "ls-files", "-co", "--exclude-standard", "-z", "--", *prefixes],
                          capture_output=True, text=True, check=True, env=cli_harness.isolated_env(os.environ))
    return sorted(p for p in set(done.stdout.split("\0")) if p and os.path.isfile(os.path.join(source, p)))


def read(path: str) -> str:
    with open(path) as f:
        return f.read()


def copy_files(source: str, target: str, files: Sequence[str]) -> None:
    for rel in files:
        os.makedirs(os.path.dirname(os.path.join(target, rel)), exist_ok=True)
        shutil.copy2(os.path.join(source, rel), os.path.join(target, rel))


class World:
    """One scenario's temp root and everything in it (the module's doc). `close()` (an addCleanup) ends the stub
    GitHub and removes the root."""

    def __init__(self) -> None:
        self.root = os.path.realpath(tempfile.mkdtemp(prefix="tools-e2e-"))
        self.home, self.bin, self.tmp = (os.path.join(self.root, d) for d in ("home", "bin", "tmp"))
        for d in (self.home, self.bin, self.tmp, os.path.join(self.home, "bin")):
            os.makedirs(d, exist_ok=True)
        self.calls_file = os.path.join(self.root, "calls.jsonl")
        open(self.calls_file, "w").close()
        self._tools: Optional[str] = None
        self.gh = None
        self._gh_server = None
        self.gh_url = ""
        shutil.copy2(os.path.join(HERE, "gh_shim.py"), os.path.join(self.bin, "gh"))
        for name in ("ssh", "nix", "kubectl", "sbt", "sbt-warm", "docker"):
            self.shim(name, REFUSING_SHIM)
        self.shim("agentctl", AGENTCTL_SHIM)
        shutil.copy2(os.path.join(self.bin, "agentctl"), os.path.join(self.home, "bin", "agentctl"))

    def close(self) -> None:
        if self._gh_server is not None:
            self._gh_server.shutdown()
            self._gh_server.server_close()
        shutil.rmtree(self.root, ignore_errors=True)

    # --- the stubs
    def github(self):
        """The stub GitHub (started on first use)."""
        if self.gh is None:
            import gh_stub
            self.gh = gh_stub.GitHub()
            self._gh_server, self.gh_url = gh_stub.serve(self.gh)
        return self.gh

    def shim(self, name: str, body: str) -> str:
        """A shim `name` first on PATH: Python, with NAME, argv, stdin, out(), err() in scope; every call logged."""
        path = os.path.join(self.bin, name)
        with open(path, "w") as f:
            f.write(SHIM_PRELUDE.format(python=sys.executable, name=name) + body + "\n")
        os.chmod(path, 0o755)
        return path

    def calls(self, name: str) -> List[Dict[str, Any]]:
        """Each call of shim `name`: its argv, cwd and stdin."""
        with open(self.calls_file) as f:
            logged: List[Dict[str, Any]] = [json.loads(line) for line in f]
        return [c for c in logged if c["name"] == name]

    # --- running
    def env(self, extra: Optional[Mapping[str, Optional[str]]] = None) -> Dict[str, str]:
        if self.gh is None:
            self.github()
        cache = os.path.join(self.home, ".cache")
        return cli_harness.isolated_env(os.environ, {
            "HOME": self.home, "TMPDIR": self.tmp, "XDG_CACHE_HOME": cache, "E2E_CALLS": self.calls_file,
            "E2E_GH_URL": self.gh_url, "GH_TOKEN": "e2e-not-a-token", "PATH": self.bin + os.pathsep + os.environ["PATH"],
            "KINOWO_FAIL_BEFORE_DIR": os.path.join(cache, "kinowo-fail-before"), "KINOWO_FAIL_BEFORE_ROUTE": "local",
            "KINOWO_SBT_WARM_ROUTE": "local", "RUN_SCRIPT_TESTS_DURATIONS": os.path.join(cache, "durations.tsv"),
            "RUN_SCRIPT_TESTS_LOG_DIR": os.path.join(cache, "script-tests-failed"),
            "PREPUSH_REUSE_DIR": os.path.join(cache, "kinowo-prepush"), "E2E_MUTATION": None, "CI": None, INSIDE: "1", **(extra or {})})

    def run(self, argv: Sequence[str], cwd: str, env: Optional[Mapping[str, Optional[str]]] = None,
            stdin: str = "", bound: float = STEP_BOUND) -> cli_harness.Done:
        """`argv` in `cwd`, its own process group, ended (and the step failed) past `bound` seconds."""
        p = subprocess.Popen(list(argv), cwd=cwd, env=self.env(env), stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE, text=True, start_new_session=True)
        try:
            out, err = p.communicate(stdin, timeout=bound)
        except subprocess.TimeoutExpired:
            os.killpg(p.pid, signal.SIGKILL)
            out, err = p.communicate()
            raise StepFailed(f"{' '.join(argv)} still running after {bound:.0f} s (ended)\n{out[-3000:]}\n{err[-3000:]}")
        return cli_harness.Done(p.returncode, out, err)

    def git(self, cwd: str, *args: str) -> str:
        done = self.run(["git", *args], cwd)
        if done.code != 0:
            raise StepFailed(f"git {' '.join(args)} in {cwd}: {done.code}\n{done.output}")
        return done.stdout.strip()

    # --- the tools under test
    def tool(self, rel: str) -> str:
        """The tool's path: this checkout's, or under $E2E_MUTATION its mutated copy's."""
        m = mutation()
        if m is None:
            return os.path.join(ROOT, rel)
        if self._tools is None:
            self._tools = os.path.join(self.root, "tools")
            copy_files(ROOT, self._tools, working_files(ROOT, TOOL_TREES))
            mutate(self._tools, m)
        return os.path.join(self._tools, rel)

    # --- repositories
    def write(self, repo: str, rel: str, text: str, executable: bool = False) -> str:
        path = os.path.join(repo, rel)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "w") as f:
            f.write(text)
        if executable:
            os.chmod(path, 0o755)
        return path

    def commit(self, repo: str, message: str, *paths: str) -> str:
        self.git(repo, "add", "-A", "--", *(paths or (".",)))
        self.git(repo, "commit", "-q", "--no-verify", "-m", message)
        return self.git(repo, "rev-parse", "HEAD")

    def toy_repo(self, name: str, files: Mapping[str, str]) -> "Repo":
        """A bare origin and a clone of it, holding `files` {path: text} in one commit pushed to main."""
        bare, clone = os.path.join(self.root, "github", f"{name}.git"), os.path.join(self.root, name)
        os.makedirs(os.path.dirname(bare), exist_ok=True)
        self.git(self.root, "init", "-q", "--bare", "-b", "main", bare)
        self.git(self.root, "clone", "-q", bare, clone)
        for rel, text in files.items():
            self.write(clone, rel, text, executable=text.startswith("#!"))
        self.commit(clone, "base")
        self.git(clone, "push", "-q", "origin", "HEAD:main")
        return Repo(self, bare, clone)

    def movies_clone(self) -> "Repo":
        """A clone of this checkout (its HEAD; the big fixtures left out) with the tool trees as they are in the working
        tree -- or as the mutation leaves them -- committed on top and pushed to a bare origin as its main."""
        bare, clone = os.path.join(self.root, "github", "movies.git"), os.path.join(self.root, "movies")
        self.git(self.root, "clone", "-q", "--bare", "--shared", ROOT, bare)
        self.git(self.root, "clone", "-q", "--shared", "--no-checkout", bare, clone)
        self.git(clone, "sparse-checkout", "set", "--no-cone", *SPARSE)
        self.git(clone, "checkout", "-q", "-B", "main", self.git(ROOT, "rev-parse", "HEAD"))
        source = ROOT
        if mutation() is not None:
            self.tool("scripts")  # the mutated copy of the tool trees
            source = self._tools or ROOT
        for rel in self.git(clone, "ls-files", "--", *TOOL_TREES).splitlines():
            if not os.path.exists(os.path.join(source, rel)):
                os.remove(os.path.join(clone, rel))
        copy_files(source, clone, working_files(source, TOOL_TREES))
        if self.git(clone, "status", "--porcelain", "--", *TOOL_TREES):
            self.commit(clone, "e2e: the tools under test, as the working tree has them", *TOOL_TREES)
        self.git(clone, "push", "-q", "-f", "--no-verify", "origin", "HEAD:main")  # the temp bare's main: this HEAD
        self.git(clone, "fetch", "-q", "origin", "main")
        return Repo(self, bare, clone)


class Repo:
    """A bare origin and the clone a scenario works in."""

    def __init__(self, world: World, bare: str, clone: str) -> None:
        self.world, self.bare, self.clone = world, bare, clone

    def main(self) -> str:
        return self.world.git(self.world.root, "--git-dir", self.bare, "rev-parse", "main")

    def worktree(self, name: str, base: str = "origin/main") -> str:
        path = os.path.join(self.world.root, f"wt-{name}")
        self.world.git(self.clone, "fetch", "-q", "origin")
        self.world.git(self.clone, "worktree", "add", "-q", "-b", name, path, base)
        return path


class Scenario(unittest.TestCase):
    """A scenario's base: its World, closed at its end."""

    def setUp(self) -> None:
        if os.environ.get(INSIDE):
            self.skipTest("inside another scenario's world: its own run is the outer one's")
        self.w = World()
        self.addCleanup(self.w.close)

    def assertRan(self, done: cli_harness.Done, code: int, *lines: str) -> None:  # noqa: N802 (unittest's style)
        self.assertEqual(code, done.code, done.output[-6000:])
        for line in lines:
            self.assertIn(line, done.output, done.output[-6000:])
