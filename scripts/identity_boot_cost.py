#!/usr/bin/env python3
"""THE IDENTITY BOOT-COST RATCHET (scripts/identity-boot-cost): what `scripts.IdentityBootCost` measured -- bytes and
time a family for each phase (resolve, boot, boot-steady) over the pinned identity slices -- against the recorded
baseline, test/resources/identity-boot-cost-baseline.tsv.

A phase FAILS when its bytes a family rose more than the baseline's tolerance (its `tolerance` row, a percentage chosen
from the measure's own run-to-run spread); a fall passes, and `update` records it. Time is reported, never held: a
laptop's wall clock moves with its load. Families are counted, not held: a resolver change that splits or merges
families moves the denominator, which the report says.

  identity_boot_cost.py check  --current <rows.tsv> [--baseline <tsv>]   the report; exit 1 when a phase rose past it
  identity_boot_cost.py update --current <rows.tsv> [--baseline <tsv>]   the report, then the baseline rewritten from the
                                                                          measure (its tolerance kept)
"""

import argparse
import os
import sys
from typing import Any, Dict, List, Optional, Tuple

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "lib"))
from tool_files import Files  # noqa: E402

Row = Tuple[int, int, int]  # families, bytes, nanoseconds

DEFAULT_BASELINE = os.path.join(HERE, "..", "test", "resources", "identity-boot-cost-baseline.tsv")
PHASES = ("resolve", "boot", "boot-steady")
HEADER = """# THE IDENTITY BOOT-COST BASELINE (scripts/identity-boot-cost --update writes it; scripts/identity_boot_cost.py holds a
# run to it). What the identity model allocates a family over the pinned identity slices (scripts.IdentityBootCost: a
# cold resolve, a take-up re-resolving every family under changed rules, and one with nothing moved), measured in a JVM
# without escape analysis, the median of three runs:
#   tolerance  the rise in bytes a family, in percent, past which a phase fails. Measured 2026-11-20, three fresh JVMs
#              on one tree: bytes moved by under 2 KB in 18 GB (1e-7), time by 24%; so 2% -- a megabyte a family at
#              boot -- is far above the measure's own noise and holds a real regression
#   phase      <phase> <families> <bytes a family> <nanoseconds a family> (time recorded, never held)
"""


def read_rows(files: Files, path: str) -> Tuple[Dict[str, Row], List[Tuple[Any, ...]]]:
    """{phase: (families, bytes, nanos)} from the measure's totals, and [(phase, slice, families, bytes, nanos)] its slices."""
    totals: Dict[str, Row] = {}
    slices: List[Tuple[Any, ...]] = []
    for line in files.read_text(path).splitlines():
        if not line.strip():
            continue
        phase, slice_name, families, total_bytes, nanos = line.split("\t")
        row = (int(families), int(total_bytes), int(nanos))
        if slice_name == "*":
            totals[phase] = row
        else:
            slices.append((phase, slice_name) + row)
    return totals, slices


def read_baseline(files: Files, path: str) -> Tuple[Optional[float], Dict[str, Row]]:
    tolerance: Optional[float] = None
    phases: Dict[str, Row] = {}
    for line in files.read_text(path).splitlines():
        if not line.strip() or line.startswith("#"):
            continue
        cells = line.split("\t")
        if cells[0] == "tolerance":
            tolerance = float(cells[1])
        elif cells[0] == "phase":
            phases[cells[1]] = (int(cells[2]), int(cells[3]), int(cells[4]))
    if tolerance is None:
        raise ValueError(f"{path}: no tolerance row")
    return tolerance, phases


def per_family(row):
    families, total_bytes, nanos = row
    return (total_bytes // families, nanos // families) if families else (0, 0)


def megabytes(n):
    return f"{n / 1048576:.2f} MB"


def compare(totals, slices, tolerance, baseline):
    """The report's lines and whether any phase rose past the tolerance."""
    out, failed = [], False
    out.append(f"# identity-boot-cost: bytes and time a family, against the baseline (tolerance +{tolerance:g}%)")
    out.append("")
    out.append("| phase | families | bytes / family | baseline | change | time / family | verdict |")
    out.append("|---|---|---|---|---|---|---|")
    for phase in PHASES:
        if phase not in totals:
            out.append(f"| {phase} | - | - | - | - | - | MISSING from the measure |")
            failed = True
            continue
        families = totals[phase][0]
        bytes_each, nanos_each = per_family(totals[phase])
        if phase not in baseline:
            out.append(f"| {phase} | {families} | {megabytes(bytes_each)} | - | - | {nanos_each / 1e6:.2f} ms | NEW (no baseline row) |")
            continue
        base_families, base_bytes, _ = baseline[phase]
        change = 100.0 * (bytes_each - base_bytes) / base_bytes if base_bytes else 0.0
        rose = change > tolerance
        failed = failed or rose
        verdict = "ROSE past the tolerance" if rose else ("fell: `--update` records it" if change < -tolerance else "ok")
        moved = "" if families == base_families else f" (baseline {base_families})"
        out.append(f"| {phase} | {families}{moved} | {megabytes(bytes_each)} | {megabytes(base_bytes)} | {change:+.1f}% | "
                   f"{nanos_each / 1e6:.2f} ms | {verdict} |")
    heaviest = sorted((s for s in slices if s[0] == "boot" and s[2]), key=lambda s: -s[3] / s[2])[:5]
    if heaviest:
        out.append("")
        out.append("Heaviest boot re-resolves, bytes a family:")
        for _, name, families, total_bytes, nanos in heaviest:
            out.append(f"- {name}: {megabytes(total_bytes // families)} over {families} families, {nanos / families / 1e6:.2f} ms each")
    return out, failed


def baseline_text(totals, tolerance):
    lines = [HEADER.rstrip("\n"), f"tolerance\t{tolerance:g}"]
    for phase in PHASES:
        families = totals[phase][0]
        bytes_each, nanos_each = per_family(totals[phase])
        lines.append(f"phase\t{phase}\t{families}\t{bytes_each}\t{nanos_each}")
    return "\n".join(lines) + "\n"


def run(argv: List[str], files: Files) -> int:
    parser = argparse.ArgumentParser(prog="identity_boot_cost.py")
    parser.add_argument("command", choices=("check", "update"))
    parser.add_argument("--current", required=True)
    parser.add_argument("--baseline", default=DEFAULT_BASELINE)
    args = parser.parse_args(argv)
    totals, slices = read_rows(files, args.current)
    tolerance, baseline = read_baseline(files, args.baseline)
    report, failed = compare(totals, slices, tolerance, baseline)
    print("\n".join(report))
    if args.command == "update":
        files.replace_text(args.baseline, baseline_text(totals, tolerance))
        print(f"\nidentity-boot-cost: {args.baseline} updated")
        return 0
    return 1 if failed else 0


def main(argv: List[str]) -> int:
    return run(argv, Files.here())


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
