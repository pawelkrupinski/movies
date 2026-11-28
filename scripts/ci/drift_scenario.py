#!/usr/bin/env python3
"""THE DRIFT SCENARIO GUARD: head and base of the differential drift gate must replay the same scenario on a seed.

scripts.IdentityDrift writes `scenario-<cc>.tsv` beside its flips (seed, kind, key, value): what the harness DREW for
each seed, as against what the code under test decided --

  arrangement  one row a part: `inputs` (the listings the replay was dealt), `venues` (their order), `boot`,
               `arrival-1..3`, `leaving`, `half-1..5` (each re-scrape's half), each as a digest
  fill         one row a draw: `<round>:<question id>` -> 1 filed / 0 held, at the fills that file a fraction

    drift_scenario.py compare --seed S --head HEAD.tsv --base BASE.tsv

compares the parts that do not depend on the change's own questions: the arrangement, whenever both sides were dealt
the same `inputs` (a change to the listings themselves re-deals it: said, not compared), and the draw of every
question BOTH sides asked (a question only one side asks is the change's own). Any difference is a HARNESS ERROR --
the two sides met different scenarios, so their flips cannot be told apart -- printed loudly, never as flips: for two
weeks one `Random` drew both the fills and the halves, a change asking one question more re-dealt every later draw, and
the gate compared head and base flips over different scenarios unseen (DE seed 8's Geister, drift 42003442640).

Exit: 0 the same scenario (or nothing to compare: a side wrote no scenario -- a base from before the guard), 1 a harness
error (each difference named), 2 unreadable. The last line is always `scenario: <verdict>`.
"""
import argparse
import os
import sys
from typing import Iterator, Optional, Sequence

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from seams.tool_files import Files  # noqa: E402


Scenario = tuple[dict[str, str], dict[str, str]]  # (arrangement {part: digest}, fills {key: value}) of one seed


def _scenario_lines(files: Files, path: str) -> Optional[list[str]]:
    """The lines of a scenario TSV, header checked; None when the file is absent or empty."""
    try:
        lines = files.read_text(path).splitlines()
    except FileNotFoundError:
        return None
    if lines and lines[0].split("\t") != ["seed", "kind", "key", "value"]:
        raise ValueError(f"{path}: not a scenario TSV (header {lines[0]!r})")
    return lines or None


def _scenario_rows(path: str, lines: list[str]) -> Iterator[tuple[int, list[str]]]:
    """(line number, [seed, kind, key, value]) of each row under the header."""
    for n, line in enumerate(lines[1:], start=2):
        cols = line.split("\t")
        if line and len(cols) != 4:
            raise ValueError(f"{path}:{n}: {len(cols)} columns, not 4")
        if line:
            yield n, cols


def read(files: Files, path: str, seed: str) -> Optional[Scenario]:
    """(arrangement {part: digest}, fills {key: value}) of `seed` in a scenario TSV; None when the file is absent or empty."""
    lines = _scenario_lines(files, path)
    if lines is None:
        return None
    arrangement: dict[str, str] = {}
    fills: dict[str, str] = {}
    parts = {"arrangement": arrangement, "fill": fills}
    for n, (s, kind, key, value) in _scenario_rows(path, lines):
        if s != str(seed):
            continue
        if kind not in parts:
            raise ValueError(f"{path}:{n}: unknown kind {kind!r}")
        parts[kind][key] = value
    return arrangement, fills


def _arrangement_differences(ha: dict[str, str], ba: dict[str, str]) -> tuple[list[str], list[str]]:
    """(lines, said) of the arrangement parts the two sides differ on; not compared when their inputs differ."""
    if ha.get("inputs") != ba.get("inputs"):
        return [], ["the two sides were dealt different listings (inputs): the arrangement is not compared"]
    return [f"arrangement `{part}` differs: head {ha[part]}, base {ba[part]}" for part in ha if part in ba and ha[part] != ba[part]], []


def _fill_difference(hf: dict[str, str], bf: dict[str, str], common: list[str]) -> Optional[str]:
    """The line naming the fill draws both sides made and differ on, else None."""
    differ = [k for k in common if hf[k] != bf[k]]
    if not differ:
        return None
    return (f"{len(differ)} of the {len(common)} fill draw(s) both sides made differ, e.g. "
            + "; ".join(f"{k}: head {'filed' if hf[k] == '1' else 'held'}, base {'filed' if bf[k] == '1' else 'held'}"
                        for k in differ[:5]))


def compare(head: Scenario, base: Scenario) -> tuple[list[str], list[str]]:
    """The differences between two seeds' scenarios that are the harness's: (lines, said) -- `said` notes what was not compared."""
    (ha, hf), (ba, bf) = head, base
    lines, said = _arrangement_differences(ha, ba)
    common = sorted(set(hf) & set(bf))
    fill = _fill_difference(hf, bf, common)
    if fill:
        lines.append(fill)
    said.append(f"{len(common)} common fill draw(s), {len(set(hf) - set(bf))} head-only, {len(set(bf) - set(hf))} base-only")
    return lines, said


DOCSTRING = __doc__ or ""  # None under python -OO


def _written(scenario: Optional[Scenario]) -> bool:
    """Whether a side wrote a scenario (none: a replay from before the guard)."""
    return scenario is not None and bool(scenario[0] or scenario[1])


def _report(a: argparse.Namespace, head: Scenario, base: Scenario) -> int:
    lines, said = compare(head, base)
    for line in said:
        print(f"  {line}")
    for line in lines:
        print(f"  HARNESS: {line}")
    if lines:
        print(f"scenario: HARNESS ERROR on seed {a.seed} -- head and base replayed different scenarios; their flips are not comparable")
        return 1
    print(f"scenario: the same on seed {a.seed}")
    return 0


def main(argv: Sequence[str], files: Files) -> int:
    p = argparse.ArgumentParser(description=DOCSTRING.split("\n")[0])
    sub = p.add_subparsers(dest="cmd", required=True)
    c = sub.add_parser("compare")
    c.add_argument("--seed", required=True)
    c.add_argument("--head", required=True)
    c.add_argument("--base", required=True)
    a = p.parse_args(argv)
    try:
        head, base = read(files, a.head, a.seed), read(files, a.base, a.seed)
    except (OSError, ValueError) as e:
        print(f"scenario: unreadable -- {e}")
        return 2
    if head is None or base is None or not _written(head) or not _written(base):
        side = "head" if not _written(head) else "base"
        print(f"scenario: not compared on seed {a.seed} -- the {side} wrote no scenario (a replay from before the guard)")
        return 0
    return _report(a, head, base)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], Files.here()))
