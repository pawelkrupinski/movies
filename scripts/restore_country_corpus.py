#!/usr/bin/env python3
"""Restore the recorded pair a hermetic convergence leg replays into this checkout.

Restore the recorded PAIR a hermetic convergence leg replays -- a country's scrape corpus and the enrichment tree
recorded beside it -- into this checkout, exactly as CI's `.github/actions/convergence-setup` does.

  scripts/restore-country-corpus.sh <pl|de|uk|es|us|all>

Where CI keeps them (none of it is in git, bar two stale July corpora -- see below):
  - `hermetic-<code>.txt` in the rolling `convergence-fixtures` release pins the pair:
    `<recording run id> <recorded at> <tree asset>`.
  - the corpus is the `scrape-fixtures-<code>` ARTIFACT of that `Record scrape fixtures` run (14-day retention):
    `scrapes-<code>.tar.gz`, holding test/resources/fixtures/corpus/{cinema-scrapes-<code>.json.gz,
    prod-coverage-<code>.json, prod-films-<code>.json.gz, cinema-scrapes-<code>-sample.json.gz,
    prod-coverage-<code>-sample.json, prod-films-<code>-sample.json.gz} (prod-films only in recordings since
    2026-10-13).
  - the tree is that release asset (`enrichment-<code>-<run>.tar.zst`, ~40-320 MB), unpacked to
    test/resources/fixtures/enrichment-<code>, with every FILL later legs published over it laid on top
    (`convergence-fill.sh fills`), as `restore-enrichment-tree.sh` does in CI.

It fails loudly -- never falls back to anything -- when no pair is pinned, the pinned corpus has expired, or the tree
is gone: a run over anything else is not the experiment CI ran. Record a fresh pair with
`gh workflow run "Record scrape fixtures" --ref main` (~10 min).

The repo still TRACKS a July Polish and German corpus (cinema-scrapes-{pl,de}.json.gz, prod-coverage-{pl,de}.json);
restoring overwrites them, which `git checkout --` undoes. CI overwrites them the same way on every leg.

Exit: 0 restored, 1 a failure (said on stderr; a failed unpack its own status), 64 usage. FIXTURE_RELEASE_TAG (default
convergence-fixtures) and RESTORE_ROOT (default this checkout) are overridable for the test,
scripts/restore_country_corpus_test.py. The reads are lane-2 CLIs, run as they are: convergence-setup.sh pinned,
scripts/ci/gh-optional.sh, unpack-fixture-archive.sh, convergence-fill.sh fills, restore-enrichment-tree.sh."""
from __future__ import annotations

import os
import shutil
import sys
import tempfile
from dataclasses import dataclass
from typing import Dict, List, Mapping, Optional

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "lib"))
from processes import machine  # noqa: E402
import tool_process  # noqa: E402

COUNTRIES = ("pl", "de", "uk", "es", "us")  # the shell tool's `all` order
RECORD = 'Record one: gh workflow run "Record scrape fixtures" --ref main'
FIXTURES = "test/resources/fixtures"


class Failed(Exception):
    """The restore failed: the message (said as `restore-country-corpus: <message>`) and the exit status."""

    def __init__(self, message: str, code: int = 1) -> None:
        super().__init__(message)
        self.code = code


@dataclass(frozen=True)
class Pair:
    corpus: str
    recorded: str
    tree: str


class Restorer:
    """One country's pair into `root`, each read one of the CLIs CI's convergence-setup runs."""

    def __init__(self, runner: tool_process.Runner, env: Mapping[str, str], repo: str, root: str) -> None:
        self.runner, self.env, self.repo, self.root = runner, dict(env), repo, root
        self.tag = self.env["FIXTURE_RELEASE_TAG"]

    def capture(self, argv: List[str], cwd: Optional[str] = None) -> tool_process.Captured:
        """`$(cmd)`: its stdout captured, trailing newlines dropped; its stderr passed on."""
        done = tool_process.capture_loudly(self.runner, tool_process.Command(argv, self.env, cwd=cwd))
        return tool_process.Captured(done.code, done.stdout.rstrip("\n"), done.stderr)

    def script(self, relative: str) -> str:
        return os.path.join(self.repo, relative)

    def restore(self, code: str) -> None:
        with tempfile.TemporaryDirectory() as work:
            pair = self.pinned(code)
            print(f"==> {code}: corpus from recording run {pair.corpus} ({pair.recorded}), tree {pair.tree}",
                  flush=True)
            self.lay_corpus(code, pair, work)
            self.lay_tree(code, pair, os.path.join(work, "tree"))

    def pinned(self, code: str) -> Pair:
        """The pin's one reader, shared with CI's convergence-setup."""
        marker = f"hermetic-{code}.txt"
        done = self.capture([self.script(".github/scripts/convergence-setup.sh"), "pinned", code])
        if done.code == 2:
            raise Failed(f"{marker} is malformed")
        if done.code != 0:
            raise Failed(f"could not read release {self.tag} for {marker} (gh auth?)")
        words = done.stdout.split()
        if not words:
            raise Failed(f"no recorded pair pinned for {code} ({marker} in release {self.tag}). {RECORD}")
        words += [""] * (3 - len(words))
        return Pair(words[0], words[1], words[2])

    def lay_corpus(self, code: str, pair: Pair, work: str) -> None:
        artifact = f"scrape-fixtures-{code}"
        done = self.capture([self.script("scripts/ci/gh-optional.sh"), "run", "download", pair.corpus, "--name",
                             artifact, "--dir", os.path.join(work, "corpus")])
        if done.code != 0:
            raise Failed(f"could not download {artifact} from run {pair.corpus}")
        archive = os.path.join(work, "corpus", f"scrapes-{code}.tar.gz")
        if done.stdout != "present" or not os.path.isfile(archive):
            raise Failed(f"recording run {pair.corpus}'s {artifact} artifact is gone (14-day retention). {RECORD}")
        status = self.runner.run(tool_process.Command(
            [self.script(".github/scripts/unpack-fixture-archive.sh"), archive, self.root], self.env))
        if status != 0:
            raise Failed("", status)
        scrapes = f"{FIXTURES}/corpus/cinema-scrapes-{code}.json.gz"
        if not os.path.isfile(os.path.join(self.root, scrapes)):
            raise Failed(f"{archive} held no {scrapes}")
        size = self.capture(["du", "-h", os.path.join(self.root, scrapes)]).stdout.split("\t")[0]
        print(f"    corpus: {size} {scrapes}", flush=True)

    def lay_tree(self, code: str, pair: Pair, stage: str) -> None:
        done = self.capture([self.script(".github/scripts/convergence-fill.sh"), "fills", code, pair.corpus],
                            cwd=self.repo)
        if done.code != 0:
            raise Failed(f"could not list the fills published over {pair.tree}")
        fills = done.stdout
        status = self.runner.run(tool_process.Command(
            [self.script(".github/scripts/restore-enrichment-tree.sh"), code, "hermetic", stage],
            {**self.env, "KINOWO_CONVERGENCE_TREE_ASSET": pair.tree, "KINOWO_CONVERGENCE_FILL_ASSETS": fills},
            cwd=self.repo))
        if status != 0:
            raise Failed(f"could not restore the pinned tree {pair.tree}" + (f" and its fills {fills}" if fills else ""))
        tree = f"{FIXTURES}/enrichment-{code}"
        if not os.path.isdir(os.path.join(stage, tree)):
            raise Failed(f"{pair.tree} held no enrichment-{code} tree")
        replace_tree(os.path.join(stage, FIXTURES), os.path.join(self.root, FIXTURES), f"enrichment-{code}")
        files = count_files(os.path.join(self.root, tree))
        print(f"    tree:   {files} files in {tree}" + (f" (fills: {fills})" if fills else ""), flush=True)


def replace_tree(staged: str, fixtures: str, tree: str) -> None:
    """The tree REPLACES the local one, never merges into it: what a local run fetched live is not in the pair, and a
    hermetic replay must not find it. Every other staged entry is copied over what is there (`cp -a`)."""
    shutil.rmtree(os.path.join(fixtures, tree), ignore_errors=True)
    os.makedirs(fixtures, exist_ok=True)
    for entry in sorted(os.listdir(staged)):
        source, target = os.path.join(staged, entry), os.path.join(fixtures, entry)
        if os.path.isdir(source) and not os.path.islink(source):
            shutil.copytree(source, target, symlinks=True, dirs_exist_ok=True)
        else:
            shutil.copy2(source, target, follow_symlinks=False)


def count_files(directory: str) -> int:
    """`find DIR -type f | wc -l`: regular files, a symbolic link not one."""
    return sum(1 for base, _, names in os.walk(directory) for name in names
               if not os.path.islink(os.path.join(base, name)) and os.path.isfile(os.path.join(base, name)))


def main(argv: List[str], env: Dict[str, str]) -> int:
    which = argv[0] if argv else ""
    if which not in COUNTRIES + ("all",):
        print(f"usage: {sys.argv[0]} <pl|de|uk|es|us|all>", file=sys.stderr)
        return 64
    repo = os.path.dirname(HERE)
    env = {**env, "FIXTURE_RELEASE_TAG": env.get("FIXTURE_RELEASE_TAG") or "convergence-fixtures"}
    restorer = Restorer(tool_process.SubprocessRunner(), env, repo, env.get("RESTORE_ROOT") or repo)
    try:
        for code in COUNTRIES if which == "all" else (which,):
            restorer.restore(code)
    except Failed as failed:
        if str(failed):
            print(f"restore-country-corpus: {failed}", file=sys.stderr)
        return failed.code
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], dict(machine.environment())))
