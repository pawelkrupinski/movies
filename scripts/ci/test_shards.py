#!/usr/bin/env python3
"""TEST SHARDS: one shard of a list of sbt test lines, so N jobs run what one job ran, each class exactly once.

The landing check's identity share (scripts/hooks/pre-push's `identity` check, PREPUSH_IDENTITY_SHARD=<k>/<n>) ran its
~300 selected specs in one job, ~10 min of tests behind ~2.5 min of compile (precommit-check run 41373956176). Split by
class over n jobs, each runs about 1/n of them.

Each input line is one sbt command (`[<module>/][Test/]testOnly <classes...> [-- <args>]` or anything else). A line
naming its classes outright is split by class: every class goes to exactly one shard, keeping its line's command and its
`-- <args>` (so a class keeps its module, its fork group and its country scope). A line that names no class outright (a
glob, an exclusion, `<module>/test`, any other command) is never split: it goes WHOLE to exactly one shard. The
assignment is longest-first onto the least-loaded shard (classes weighted by --weights' seconds, an unknown class by
--default-weight, an unsplit line by --whole-weight), ties to the lowest shard: deterministic for one input.

So the union of the n shards' lines runs exactly the input's classes and lines, each once (test_shards_test.py proves it
over random inputs).

Usage: test_shards.py --shard K/N [--weights FILE] < sbt lines   the shard's lines, in the input's order
"""

import argparse
import re
import sys

LINE = re.compile(r"^(?P<cmd>(?:\w+/)?(?:Test/)?testOnly)\s+(?P<classes>.*?)(?P<args>\s+--(?:\s.*)?)?$")
CLASS = re.compile(r"^[A-Za-z_$][\w$]*(?:\.[A-Za-z_$][\w$]*)*$")


def parse(line):
    """(command, [classes], args) for a line naming its classes outright, else None."""
    m = LINE.match(line.strip())
    if not m:
        return None
    classes = m.group("classes").split()
    if not classes or not all(CLASS.match(c) for c in classes):
        return None
    return m.group("cmd"), classes, (m.group("args") or "")


def read_weights(path):
    weights = {}
    if not path:
        return weights
    with open(path, encoding="utf-8") as fh:
        for raw in fh:
            raw = raw.split("#", 1)[0].strip()
            if raw:
                name, seconds = raw.split()
                weights[name] = float(seconds)
    return weights


def assign(lines, n, weights, default_weight, whole_weight):
    """{(line index, class or None): shard} over every class of every splittable line and every other line whole."""
    items = []
    for i, line in enumerate(lines):
        parsed = parse(line)
        if parsed is None:
            items.append((whole_weight, i, None))
        else:
            items += [(weights.get(c, default_weight), i, c) for c in dict.fromkeys(parsed[1])]
    loads = [0.0] * n
    where = {}
    for w, i, c in sorted(items, key=lambda t: (-t[0], t[1], t[2] or "")):
        k = min(range(n), key=lambda s: (loads[s], s))
        loads[k] += w
        where[(i, c)] = k
    return where


def shard(lines, k, n, weights=None, default_weight=3.0, whole_weight=600.0):
    """The lines shard k (0-based) of n runs, in the input's order."""
    lines = [l.rstrip("\n") for l in lines if l.strip()]
    where = assign(lines, n, weights or {}, default_weight, whole_weight)
    out = []
    for i, line in enumerate(lines):
        parsed = parse(line)
        if parsed is None:
            if where[(i, None)] == k:
                out.append(line)
            continue
        cmd, classes, args = parsed
        mine = [c for c in dict.fromkeys(classes) if where[(i, c)] == k]
        if mine:
            out.append(f"{cmd} {' '.join(mine)}{args}")
    return out


def main(argv):
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--shard", required=True, help="K/N, 1-based")
    ap.add_argument("--weights", help="TSV: <class> <seconds>, # comments")
    ap.add_argument("--default-weight", type=float, default=3.0)
    ap.add_argument("--whole-weight", type=float, default=600.0)
    a = ap.parse_args(argv)
    m = re.fullmatch(r"(\d+)/(\d+)", a.shard)
    if not m or not 1 <= int(m.group(1)) <= int(m.group(2)):
        ap.error(f"--shard {a.shard}: not K/N with 1 <= K <= N")
    for line in shard(sys.stdin.readlines(), int(m.group(1)) - 1, int(m.group(2)), read_weights(a.weights),
                      a.default_weight, a.whole_weight):
        print(line)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
