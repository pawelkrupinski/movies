"""WHAT THE QUALITY REPORTS SHARE (scripts/py-quality/report.py, scripts/ci/mobile_quality_report.py): a function's key in
a baseline, the worst rows as markdown, and the ratchet's command line."""
from __future__ import annotations

import argparse
from typing import Dict, Iterable, List, Sequence, Tuple

Row = Tuple[str, str, float]


def numbered_keys(path: str, names: Iterable[str]) -> List[str]:
    """Each function's key, `<path>:<name>`, a repeated name `#2`, `#3`... in the file's order (no line: an edit above
    never re-keys it)."""
    seen: Dict[str, int] = {}
    out: List[str] = []
    for name in names:
        seen[name] = seen.get(name, 0) + 1
        out.append(f"{path}:{name}" + (f"#{seen[name]}" if seen[name] > 1 else ""))
    return out


def show(value: float) -> str:
    return str(int(value)) if value == int(value) else f"{value:g}"


def markdown(title: str, rows: Sequence[Row], limits: Sequence[Tuple[str, float]]) -> str:
    """Each kind's ten worst rows past its limit, under `# <title>`."""
    lines = [f"# {title}", ""]
    for kind, limit in limits:
        worst = sorted((r for r in rows if r[0] == kind), key=lambda r: -r[2])[:10]
        lines += [f"## {kind} (limit {show(limit)})", ""]
        lines += [f"- `{key}`: {show(v)}" for _, key, v in worst if v > limit] or ["- none past it"]
        lines.append("")
    return "\n".join(lines)


def ratchet_arguments(parser: argparse.ArgumentParser) -> None:
    """--check / --update (into `ratchet`), --range (repeatable) and --out."""
    parser.add_argument("--check", dest="ratchet", action="store_const", const="check")
    parser.add_argument("--update", dest="ratchet", action="store_const", const="update")
    parser.add_argument("--range", action="append", default=[])
    parser.add_argument("--out")
