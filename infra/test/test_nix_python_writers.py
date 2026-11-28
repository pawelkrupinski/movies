#!/usr/bin/env python3
"""THE FLEET'S PYTHON WRITERS, LINTED AS THEIR BUILD LINTS THEM.

A `pkgs.writers.writePython3Bin` derivation runs flake8 over its script at BUILD time, with the writer's own
`flakeIgnore`. `nix eval` (infra/bin/check) never builds, so a flake8 error surfaced only when Stage NixOS closures
built the host: on 2026-11-27 (Stage NixOS closures #148) a typing change put a class one blank line under a comment in
nix/files/identity-regressions.py -- E302 -- and monitoring-1's closure failed to build, already on main.

This finds every writePython3Bin in nix/, rebuilds the text its builder lints -- the writer's own interpreter line
prepended, and the file's first line dropped where the expression does (`lib.drop 1`, fleet/auto-apply.nix: that is
where an E265 hides, invisible to a lint of the source file) -- and runs flake8 over it with exactly that writer's
ignores. Its flake8 is pinned (scripts/flake8-requirements.txt, via scripts/python-venv).

Run: python3 infra/test/test_nix_python_writers.py   (also run by infra/bin/check)
"""
from __future__ import annotations

import os
import pathlib
import re
import subprocess
import tempfile
import unittest
from typing import List, NamedTuple

INFRA = pathlib.Path(__file__).resolve().parent.parent
REPO = INFRA.parent
# writePython3Bin "<name>" { ... } <source expression ending in builtins.readFile <path>>
WRITER = re.compile(r'writePython3Bin\s+"(?P<name>[^"]+)"(?P<body>.*?)builtins\.readFile\s+(?P<path>[^\s)]+)', re.S)
IGNORE = re.compile(r'flakeIgnore\s*=\s*\[(?P<codes>[^\]]*)\]')
# the line writePython3Bin puts first; flake8 reads any interpreter path alike
BUILT_SHEBANG = "#! /nix/store/0000000000000000000000000000000-python3/bin/python3\n"


class Writer(NamedTuple):
    nix: pathlib.Path
    name: str
    source: pathlib.Path
    ignore: List[str]
    drops_first_line: bool


def writers(root: pathlib.Path = INFRA / "nix") -> List[Writer]:
    found: List[Writer] = []
    for nix in sorted(root.rglob("*.nix")):
        text = nix.read_text()
        for match in WRITER.finditer(text):
            ignore = IGNORE.search(match["body"])
            codes = re.findall(r'"([^"]+)"', ignore["codes"]) if ignore else []
            found.append(Writer(nix, match["name"], (nix.parent / match["path"]).resolve(), codes,
                                "lib.drop 1" in match["body"]))
    return found


def built_text(writer: Writer) -> str:
    text = writer.source.read_text()
    if writer.drops_first_line:
        text = "\n".join(text.split("\n")[1:])
    return BUILT_SHEBANG + text


def module_files(writer: Writer) -> List[pathlib.Path]:
    """The pieces a writer's script imports from beside it: `nix/files/<name with _>/*.py` (nixos-auto-apply.py's
    `nixos_auto_apply/`), shipped as a store copy the one swapped line of the script points at. The build lints only
    the script, so the pieces are linted here with the writer's ignores."""
    folder = writer.source.parent / writer.source.stem.replace("-", "_")
    return sorted(folder.glob("*.py")) if folder.is_dir() else []


def flake8() -> str:
    """The pinned flake8's directory (scripts/python-venv builds it once)."""
    done = subprocess.run([str(REPO / "scripts" / "python-venv"), str(REPO / "scripts" / "flake8-requirements.txt"),
                           "flake8"], capture_output=True, text=True)
    if done.returncode != 0:
        raise AssertionError(f"no flake8 (scripts/python-venv failed): {done.stderr.strip()}")
    return done.stdout.strip()


def lint(writer: Writer, flake8_dir: str) -> str:
    """flake8's findings on the text the writer builds, with its ignores: empty when it builds."""
    with tempfile.TemporaryDirectory() as tmp:
        built = os.path.join(tmp, writer.source.name)
        with open(built, "w") as f:
            f.write(built_text(writer))
        args = [os.path.join(flake8_dir, "python3"), "-m", "flake8"]
        if writer.ignore:
            args.append("--ignore=" + ",".join(writer.ignore))
        done = subprocess.run([*args, built], capture_output=True, text=True)
        return (done.stdout + done.stderr).replace(built, os.path.relpath(writer.source, REPO))


class NixPythonWritersTest(unittest.TestCase):
    def test_every_writer_is_found_with_its_source_and_ignores(self) -> None:
        found = {w.name: w for w in writers()}
        self.assertLessEqual({"identity-regressions", "scheduled-dispatch", "synthetic-probe-targets",
                              "nixos-auto-apply"}, set(found))
        for writer in found.values():
            self.assertTrue(writer.source.is_file(), writer)
        self.assertEqual(["E501", "E722", "W503", "W504"], found["nixos-auto-apply"].ignore)
        self.assertTrue(found["nixos-auto-apply"].drops_first_line)
        self.assertFalse(found["identity-regressions"].drops_first_line)

    def test_every_writer_passes_the_flake8_its_build_runs(self) -> None:
        flake8_dir = flake8()
        for writer in writers():
            with self.subTest(writer=writer.name):
                self.assertEqual("", lint(writer, flake8_dir), f"{writer.nix.relative_to(REPO)} would not build")

    def test_the_pieces_beside_a_script_pass_the_same_flake8(self) -> None:
        flake8_dir = flake8()
        pieces = {w.name: module_files(w) for w in writers()}
        self.assertTrue(pieces["nixos-auto-apply"], "nixos_auto_apply/ holds the applier's modules")
        for writer in writers():
            if not pieces[writer.name]:
                continue
            args = [os.path.join(flake8_dir, "python3"), "-m", "flake8", "--ignore=" + ",".join(writer.ignore)]
            done = subprocess.run([*args, *map(str, pieces[writer.name])], capture_output=True, text=True)
            self.assertEqual("", done.stdout + done.stderr, f"{writer.name}'s modules would not lint")

    def test_the_line_nix_swaps_for_the_store_copy_is_still_in_the_script(self) -> None:
        swapped = ('sys.path.insert(0, os.path.join(os.path.dirname(os.path.realpath(__file__)), '
                   '"nixos_auto_apply"))')
        writer = next(w for w in writers() if w.name == "nixos-auto-apply")
        self.assertEqual(1, writer.source.read_text().count(swapped))
        self.assertEqual(1, writer.nix.read_text().count(swapped))

    def test_the_lint_sees_what_only_the_build_sees(self) -> None:
        # a file keeping its own shebang: under the prepended one it is line 2, a block comment (E265)
        with tempfile.TemporaryDirectory() as tmp:
            source = pathlib.Path(tmp, "kept.py")
            source.write_text("#!/usr/bin/env python3\nX = 1\n")
            self.assertIn("E265", lint(Writer(source, "kept", source, ["E501"], False), flake8()))
            self.assertEqual("", lint(Writer(source, "dropped", source, ["E501"], True), flake8()))


if __name__ == "__main__":
    unittest.main()
