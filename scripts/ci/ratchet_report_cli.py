"""WHAT THE FILE-LEVEL QUALITY REPORTS SHARE (scripts/ci/mobile_quality_report.py, scripts/ci/config_quality_report.py): the
rows a baseline freezes, one language's verdict / update / markdown, the over-limit file `scripts/ratchet-diff --measure`
reads, and the command line."""
from __future__ import annotations

import argparse
import os
from typing import Callable, Dict, Iterable, List, Sequence, Tuple

import baseline_ratchet
from code_tools import quality_report
from seams import tool_files
import tool_git

Row = Tuple[str, str, float]


def over_limit(rows: Sequence[Row], limit: Dict[str, float]) -> baseline_ratchet.Rows:
    """The rows past their kind's limit, and every limit: what a baseline freezes."""
    current: baseline_ratchet.Rows = {("limit", kind): value for kind, value in limit.items()}
    current.update({(kind, key): float(value) for kind, key, value in rows if value > limit[kind]})
    return current


def sources(git: tool_git.Git, files: tool_files.Files, holds: Callable[[str], bool]) -> Dict[str, str]:
    """Every tracked file the language holds, path -> text."""
    return {p: files.read_text(p) for p in git.lines("ls-files") if holds(p)}


def run_one(spec: baseline_ratchet.Spec, name: str, rows: Sequence[Row], limit: Dict[str, float],
            args: argparse.Namespace, shown: Sequence[str]) -> bool:
    """One language's verdict (check), baseline (update) or report (the `shown` kinds' worst rows): whether it passed."""
    current = over_limit(rows, limit)
    if args.ratchet == "update":
        baseline_ratchet.update(spec, current)
        return True
    if args.ratchet == "check":
        return baseline_ratchet.check(spec, current, args.range)
    print(quality_report.markdown(f"{name.capitalize()} quality report", rows, [(kind, limit[kind]) for kind in shown]))
    return True


def write_over_limit(files: tool_files.Files, out: str, name: str, spec: baseline_ratchet.Spec,
                     current: baseline_ratchet.Rows) -> None:
    """The held rows past a limit, and the limits (what the baseline would freeze: scripts/ratchet-diff --measure)."""
    files.write_text(os.path.join(out, f"{name}-over-limit.tsv"), "".join(
        f"{k}\t{key}\t{baseline_ratchet.show(v)}\n" for (k, key), v in sorted(current.items())
        if k == "limit" or baseline_ratchet.gated(spec, key)))


def parse(program: str, description: str, languages: Iterable[str], argv: List[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(prog=program, description=description,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    quality_report.ratchet_arguments(parser)
    parser.add_argument("--language", action="append", choices=sorted(languages))
    return parser.parse_args(argv)
