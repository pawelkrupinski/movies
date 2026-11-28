"""lint_registry_reads.py over in-memory files and a fake hook -- no git, no repository, no process: what a spec reads
(literals, root files, a resource opener), the probes of a directory, the ignored paths left out, and the registry rows
that miss a read (the black-box run over the real repository: scripts/hooks/lint-registry-test.sh).
Run: python3 scripts/ci/lint_registry_reads_test.py"""
from __future__ import annotations

import os
import sys
import unittest
from typing import Dict, List, Set

os.environ.setdefault("KINOWO_GOVERNOR", "0")  # the pool of misses() asks no governor for a width here
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path[:0] = [HERE, os.path.join(os.path.dirname(HERE), "lib")]
import lint_registry_reads as lrr  # noqa: E402
from tool_files import Files  # noqa: E402

SPEC = "worker:scripts.ThingSpec"
SOURCE = ('RepoFile.read("Dockerfile"); read("infra/nix/files/host.json"); read("data/rosters/"); '
          'getResourceAsStream("/routes"); read("not/a/file.txt")')


def lay_out(tracked: Dict[str, str], untracked: Dict[str, str] = {}) -> "tuple[lrr.Repo, Files]":
    files = Files.in_memory("/repo")
    for path, text in {**tracked, **untracked}.items():
        files.make_directories(os.path.dirname(path) or ".")
        files.write_text(path, text)
    return lrr.Repo(tracked, files), files


def nothing_ignored(paths: List[str]) -> Set[str]:
    return set()


class ReadsTest(unittest.TestCase):
    def setUp(self) -> None:
        self.repo, self.files = lay_out({
            "Dockerfile": "", "infra/nix/files/host.json": "{}", "data/rosters/a.tsv": "", "data/rosters/b.tsv": "",
            "data/rosters/c.json": "", "worker/src/test/resources/routes": ""})

    def test_it_names_literal_paths_root_files_directories_by_one_file_a_kind_and_resources(self) -> None:
        got = lrr.reads(SOURCE, self.repo, nothing_ignored)
        self.assertEqual({"Dockerfile": ["Dockerfile"], "infra/nix/files/host.json": ["infra/nix/files/host.json"],
                          "data/rosters/": ["data/rosters/a.tsv", "data/rosters/c.json"],
                          "routes": ["worker/src/test/resources/routes"]}, got)

    def test_a_path_git_ignores_is_never_probed_and_only_untracked_paths_are_asked(self) -> None:
        asked: List[List[str]] = []

        def ignoring(paths: List[str]) -> Set[str]:
            asked.append(paths)
            return {"infra/nix/files/host.json"}
        repo, _ = lay_out({"Dockerfile": ""}, {"infra/nix/files/host.json": "{}"})
        got = lrr.reads(SOURCE, repo, ignoring)
        self.assertEqual({"Dockerfile": ["Dockerfile"]}, got)
        self.assertEqual([["infra/nix/files/host.json"]], asked)

    def test_a_file_on_disk_the_index_lacks_stands_for_its_directory(self) -> None:
        repo, _ = lay_out({}, {"data/new/x.tsv": ""})
        self.assertEqual(["data/new/x.tsv"], lrr.probes("data/new", repo))


class MissesTest(unittest.TestCase):
    def test_a_row_selecting_no_probe_of_a_literal_its_spec_reads_is_a_miss(self) -> None:
        repo, _ = lay_out({
            "worker/src/test/scala/scripts/ThingSpec.scala": 'read("infra/nix/files/host.json")',
            "infra/nix/files/host.json": "{}", "reg.tsv": f"{SPEC}\tinfra/nix/files/*\n"})
        selects: Dict[str, Set[str]] = {"infra/nix/files/host.json": set()}
        got = lrr.misses("reg.tsv", set(), repo, nothing_ignored, lambda p: selects[p])
        self.assertEqual([f"{SPEC}\tinfra/nix/files/host.json"], got)
        selects["infra/nix/files/host.json"] = {SPEC}
        self.assertEqual([], lrr.misses("reg.tsv", set(), repo, nothing_ignored, lambda p: selects[p]))

    def test_a_dash_row_declares_nothing_to_read(self) -> None:
        repo, _ = lay_out({"worker/src/test/scala/scripts/ThingSpec.scala": 'read("infra/nix/files/host.json")',
                           "infra/nix/files/host.json": "{}", "reg.tsv": f"{SPEC}\t-\n"})
        self.assertEqual([], lrr.misses("reg.tsv", set(), repo, nothing_ignored, lambda p: set()))

    def test_a_spec_reading_an_artefact_without_a_row_is_rowless(self) -> None:
        repo, _ = lay_out({"worker/src/test/scala/scripts/ThingSpec.scala": 'read("infra/nix/files/host.json")',
                           "infra/nix/files/host.json": "{}", "reg.tsv": "# none\n"})
        self.assertEqual([SPEC], [s for s, _ in lrr.rowless_readers("reg.tsv", repo, nothing_ignored)])


if __name__ == "__main__":
    unittest.main()
