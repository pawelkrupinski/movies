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
import sys


def read(path, seed):
    """(arrangement {part: digest}, fills {key: value}) of `seed` in a scenario TSV; None when the file is absent or empty."""
    try:
        with open(path, encoding="utf-8") as f:
            lines = f.read().splitlines()
    except FileNotFoundError:
        return None
    if not lines:
        return None
    if lines[0].split("\t") != ["seed", "kind", "key", "value"]:
        raise ValueError(f"{path}: not a scenario TSV (header {lines[0]!r})")
    arrangement, fills = {}, {}
    for n, line in enumerate(lines[1:], start=2):
        if not line:
            continue
        cols = line.split("\t")
        if len(cols) != 4:
            raise ValueError(f"{path}:{n}: {len(cols)} columns, not 4")
        s, kind, key, value = cols
        if s != str(seed):
            continue
        if kind == "arrangement":
            arrangement[key] = value
        elif kind == "fill":
            fills[key] = value
        else:
            raise ValueError(f"{path}:{n}: unknown kind {kind!r}")
    return arrangement, fills


def compare(head, base):
    """The differences between two seeds' scenarios that are the harness's: (lines, said) -- `said` notes what was not compared."""
    (ha, hf), (ba, bf) = head, base
    lines, said = [], []
    if ha.get("inputs") != ba.get("inputs"):
        said.append("the two sides were dealt different listings (inputs): the arrangement is not compared")
    else:
        for part in [p for p in ha if p in ba]:
            if ha[part] != ba[part]:
                lines.append(f"arrangement `{part}` differs: head {ha[part]}, base {ba[part]}")
    common = sorted(set(hf) & set(bf))
    differ = [k for k in common if hf[k] != bf[k]]
    if differ:
        lines.append(f"{len(differ)} of the {len(common)} fill draw(s) both sides made differ, e.g. "
                     + "; ".join(f"{k}: head {'filed' if hf[k] == '1' else 'held'}, base {'filed' if bf[k] == '1' else 'held'}"
                                 for k in differ[:5]))
    said.append(f"{len(common)} common fill draw(s), {len(set(hf) - set(bf))} head-only, {len(set(bf) - set(hf))} base-only")
    return lines, said


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    sub = p.add_subparsers(dest="cmd", required=True)
    c = sub.add_parser("compare")
    c.add_argument("--seed", required=True)
    c.add_argument("--head", required=True)
    c.add_argument("--base", required=True)
    a = p.parse_args(argv)
    try:
        head, base = read(a.head, a.seed), read(a.base, a.seed)
    except (OSError, ValueError) as e:
        print(f"scenario: unreadable -- {e}")
        return 2
    if head is None or base is None or not (head[0] or head[1]) or not (base[0] or base[1]):
        side = "head" if head is None or not (head[0] or head[1]) else "base"
        print(f"scenario: not compared on seed {a.seed} -- the {side} wrote no scenario (a replay from before the guard)")
        return 0
    lines, said = compare(head, base)
    for line in said:
        print(f"  {line}")
    if lines:
        for line in lines:
            print(f"  HARNESS: {line}")
        print(f"scenario: HARNESS ERROR on seed {a.seed} -- head and base replayed different scenarios; their flips are not comparable")
        return 1
    print(f"scenario: the same on seed {a.seed}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
