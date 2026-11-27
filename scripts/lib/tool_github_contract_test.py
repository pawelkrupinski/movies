#!/usr/bin/env python3
"""THE GitHub CONTRACT (tool_github.py; CLAUDE.md, "Side effects live at the leaves"): one suite held by GitHub over its
two boundaries -- InMemoryGh in the process (what the tools' unit tests run on) and a real `gh` PROCESS through the
subprocess runner (the integration leg: an executable on disk answering from the same held state, so the exit codes,
the captured stdout and stderr, stdin and the file a download writes cross a real process boundary). No case reaches
GitHub itself: a test never makes live HTTP."""
from __future__ import annotations

import base64
import gzip
import json
import os
import stat
import sys
import tempfile
import unittest
from typing import Any, Dict, Mapping

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import cli_harness  # noqa: E402
import tool_github  # noqa: E402
import tool_process  # noqa: E402

API: Dict[str, Any] = {"repos/o/r/commits/main": {"sha": "abc", "parents": [{"sha": "p1"}, {"sha": "p2"}]},
                       "repos/o/r/pulls": [{"number": 7, "head": {"ref": "x"}}, {"number": 8, "head": {"ref": "y"}}]}
RELEASES: Dict[str, Dict[str, bytes]] = {"store": {"a.tsv": b"row\n", "r.gz": gzip.compress(b"whole\n"),
                                                   "cut.gz": gzip.compress(b"whole\n")[:-6]}}
STUB = """#!{python} -B
import base64, json, sys
sys.path.insert(0, {lib!r})
import tool_github
with open({state!r}) as f:
    state = json.load(f)
releases = {{t: {{n: base64.b64decode(d) for n, d in a.items()}} for t, a in state["releases"].items()}}
code, out, err = tool_github.InMemoryGh(state["api"], releases).answer(sys.argv[1:])
sys.stdout.write(out)
sys.stderr.write(err)
sys.exit(code)
"""


def real_gh(directory: str, api: Mapping[str, Any], releases: Mapping[str, Mapping[str, bytes]]) -> str:
    """An executable `gh` answering from the held state, written into `directory`."""
    state = os.path.join(directory, "state.json")
    with open(state, "w") as f:
        json.dump({"api": api, "releases": {t: {n: base64.b64encode(d).decode() for n, d in a.items()}
                                            for t, a in releases.items()}}, f)
    path = os.path.join(directory, "gh")
    with open(path, "w") as f:
        f.write(STUB.format(python=sys.executable, lib=HERE, state=state))
    os.chmod(path, os.stat(path).st_mode | stat.S_IXUSR)
    return path


class Contract(unittest.TestCase):
    def github(self, directory: str) -> tool_github.GitHub:
        raise unittest.SkipTest("the contract itself: its implementations' subclasses run it")

    def setUp(self) -> None:
        self.dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.dir.cleanup)
        self.gh = self.github(self.dir.name)

    def test_read_answers_or_is_none_never_empty(self) -> None:
        self.assertEqual("abc\n", self.gh.read(["api", "repos/o/r/commits/main", "--jq", ".sha"], 30))
        self.assertEqual("p1\np2\n", self.gh.read(["api", "repos/o/r/commits/main", "--jq", ".parents[].sha"], 30))
        self.assertIsNone(self.gh.read(["api", "repos/o/r/nothing"], 30, quiet=True))

    def test_json_is_the_parsed_answer_or_none(self) -> None:
        self.assertEqual(API["repos/o/r/pulls"], self.gh.json(["api", "repos/o/r/pulls"], 30))
        self.assertEqual([7, 8], [p["number"] for p in self.gh.json(["api", "repos/o/r/pulls"], 30)])
        self.assertIsNone(self.gh.json(["api", "repos/o/r/nothing"], 30, quiet=True))
        self.assertIsNone(self.gh.json(["api", "repos/o/r/commits/main", "--jq", ".parents[].sha"], 30, quiet=True))

    def test_succeeds_and_asset_names(self) -> None:
        self.assertTrue(self.gh.succeeds(["api", "repos/o/r/pulls"], 30))
        self.assertFalse(self.gh.succeeds(["api", "repos/o/r/nothing"], 30))
        self.assertEqual(["a.tsv", "cut.gz", "r.gz"], self.gh.asset_names("store", 30))
        self.assertIsNone(self.gh.asset_names("no-such-release", 30, quiet=True))

    def test_download_writes_the_asset_or_says_it_is_not_there(self) -> None:
        dest = os.path.join(self.dir.name, "out", "a.tsv")
        self.assertTrue(self.gh.download("store", "a.tsv", dest, 30))
        with open(dest, "rb") as f:
            self.assertEqual(b"row\n", f.read())
        self.assertFalse(self.gh.download("store", "missing.tsv", os.path.join(self.dir.name, "m"), 30))

    def test_fetch_gzipped_unpacks_a_whole_gzip_only(self) -> None:
        path = os.path.join(self.dir.name, "r.tsv")
        self.assertTrue(self.gh.fetch_gzipped("store", "r.gz", path, 30))
        with open(path, "rb") as f:
            self.assertEqual(b"whole\n", f.read())
        self.assertFalse(self.gh.fetch_gzipped("store", "cut.gz", path, 30))


class InMemoryGhContractTest(Contract):
    def github(self, directory: str) -> tool_github.GitHub:
        return tool_github.GitHub(tool_github.InMemoryGh(API, RELEASES), {})


class GhProcessContractTest(Contract):
    def github(self, directory: str) -> tool_github.GitHub:
        gh = real_gh(directory, API, RELEASES)
        return tool_github.GitHub(tool_process.SubprocessRunner(), cli_harness.isolated_env(os.environ), gh)


class InMemoryGhTest(unittest.TestCase):
    def test_an_unheld_command_or_jq_says_so(self) -> None:
        gh = tool_github.InMemoryGh(API, RELEASES)
        self.assertEqual(1, gh.answer(["pr", "list"])[0])
        with self.assertRaisesRegex(ValueError, "only .key and"):
            tool_github.jq_path({}, ".a | length")


if __name__ == "__main__":
    unittest.main()
