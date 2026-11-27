#!/usr/bin/env python3
"""scripts/label-lookup's two effects over fakes: the prod mirror (freshness, then mongosh) and the git blame, each
through a scripted runner; the black-box cases over a scratch repository are label-lookup-test.sh's."""
from __future__ import annotations

import importlib.machinery
import importlib.util
import os
import sys
import unittest
from types import ModuleType
from typing import Dict, List

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "lib"))
import tool_git  # noqa: E402
import tool_process  # noqa: E402


def load() -> ModuleType:
    loader = importlib.machinery.SourceFileLoader("label_lookup", os.path.join(HERE, "label-lookup"))
    spec = importlib.util.spec_from_loader("label_lookup", loader)
    assert spec is not None
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module


lookup = load()


class Scripted:
    """Answers a command by its first word (`freshness`, `mongosh`, `git`); records what it was asked."""

    def __init__(self, answers: Dict[str, tool_process.Captured]) -> None:
        self.answers, self.asked = answers, []  # type: Dict[str, tool_process.Captured], List[tool_process.Command]

    def capture(self, command: tool_process.Command) -> tool_process.Captured:
        self.asked.append(command)
        return self.answers[os.path.basename(command.argv[0])]

    def run(self, command: tool_process.Command) -> int:
        raise AssertionError("never run")


TAKES = '[["Kino A", "Lalka", 1321666, "title"], ["Kino B", "Lalka", null, "none"]]'


class ProdMirrorTest(unittest.TestCase):
    def mirror(self, fresh: tool_process.Captured, mongo: tool_process.Captured) -> "tuple[Scripted, object]":
        runner = Scripted({"freshness": fresh, "mongosh": mongo})
        return runner, lookup.ProdMirror(runner, {"LOCAL_MIRROR_PORT": "1234"})

    def test_a_fresh_mirrors_takes_are_read_with_the_freshness_said_first(self) -> None:
        runner, mirror = self.mirror(tool_process.Captured(0, "pl 5m old\n", ""), tool_process.Captured(0, TAKES, ""))
        fresh, takes = mirror.takes("pl", {"title": "Lalka"})  # type: ignore[attr-defined]
        self.assertEqual("5m old", fresh)
        self.assertEqual([("Kino A", "Lalka", "tmdb:1321666", "title"), ("Kino B", "Lalka", "none", "none")], takes)
        self.assertIn("mongodb://127.0.0.1:1234/kinowo_prod_mirror", runner.asked[1].argv[2])
        self.assertEqual('{"title": "Lalka"}', runner.asked[1].env["LABEL_LOOKUP_QUERY"])

    def test_a_stale_mirror_says_so_and_an_absent_one_is_not_asked(self) -> None:
        _, stale = self.mirror(tool_process.Captured(3, "de 9d old\n", ""), tool_process.Captured(0, "[]", ""))
        self.assertEqual("9d old -- NOT FRESH: its takes are as of then", stale.takes("de", {})[0])  # type: ignore[attr-defined]
        runner, absent = self.mirror(tool_process.Captured(2, "", "no mirror here"), tool_process.Captured(0, "[]", ""))
        self.assertEqual(("no mirror: no mirror here", []), absent.takes("pl", {}))  # type: ignore[attr-defined]
        self.assertEqual(1, len(runner.asked))

    def test_a_mirror_that_answers_nothing_is_said_not_to_have_answered(self) -> None:
        _, silent = self.mirror(tool_process.Captured(0, "pl 1m\n", ""), tool_process.Captured(124, "", ""))
        why, takes = silent.takes("uk", {})  # type: ignore[attr-defined]
        self.assertTrue(why.startswith("no mirror: the local prod mirror at mongodb://127.0.0.1:1234/kinowo_uk_prod_mirror"))
        self.assertEqual([], takes)


PORCELAIN = ("a" * 40 + " 1 7 1\nauthor-time 1700000000\nsummary Judge Lalka right\nfilename x\n\tbody\n"
             + "0" * 40 + " 2 8 1\nauthor-time 1\nsummary x\n")


class BlameTest(unittest.TestCase):
    def entries(self) -> List[object]:
        return [lookup.Entry("t.tsv", 7, "pl", "*", "Lalka", ["tmdb:1"], "right", ""),
                lookup.Entry("t.tsv", 8, "pl", "*", "Emil", ["tmdb:2"], "right", "")]

    def test_each_entry_names_the_commit_that_last_changed_its_line(self) -> None:
        runner = Scripted({"git": tool_process.Captured(0, PORCELAIN, "")})
        entries = self.entries()
        lookup.blame(tool_git.Git(runner, {}, "/repo"), entries)
        self.assertEqual(["aaaaaaaaa 2023-11-14 Judge Lalka right", "uncommitted"], [e.history for e in entries])  # type: ignore[attr-defined]
        self.assertEqual(1, len(runner.asked))  # one blame for the file
        self.assertIn("-L", runner.asked[0].argv)

    def test_a_blame_that_fails_leaves_the_history_empty(self) -> None:
        runner = Scripted({"git": tool_process.Captured(128, "", "fatal")})
        entries = self.entries()
        lookup.blame(tool_git.Git(runner, {}, "/repo"), entries)
        self.assertEqual(["", ""], [e.history for e in entries])  # type: ignore[attr-defined]


if __name__ == "__main__":
    unittest.main()
