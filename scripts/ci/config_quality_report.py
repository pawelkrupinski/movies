#!/usr/bin/env python3
"""THE DECLARATIVE-CONFIG QUALITY REPORT (scripts/config-quality-report): Nix, Terraform and the Twirl templates' markup held
to a file-length limit (500) and clone groups of 50+ tokens (test/resources/design-rules.tsv's), GATED by
scripts/ci/config_quality_ratchet.py (the pre-push `configquality` check) against test/resources/{nix,terraform,twirl}-quality-baseline.tsv.
Why these two and no complexity: a declarative file has no function to be complex -- it grows long and it repeats itself
(CLAUDE.md, "Design rules"; the owner, 2026-11-28).

  scripts/config-quality-report --check [--language L]... [--range A..B]...   held rows against the baselines (exit 1)
  scripts/config-quality-report --update [--language L]...                    the baselines rewritten from the measure
  scripts/config-quality-report [--language L]... [--out DIR]                  the worst rows as markdown; DIR keeps
                                                                              each language's over-limit.tsv

L is nix, terraform or twirl (default all). Exit 0; 1 on a violation; 2 on an error."""
from __future__ import annotations

import os
import sys
from typing import Dict, List

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))  # scripts/lib
import config_quality_measure as measure  # noqa: E402
import config_quality_ratchet as ratchet  # noqa: E402
import ratchet_report_cli  # noqa: E402
import rules_table  # noqa: E402
from seams import tool_files  # noqa: E402
import tool_git  # noqa: E402

SHOWN = ("file-lines", "copies")


def limits(table: rules_table.Rules) -> Dict[str, float]:
    return {"file-lines": table.limit("file-lines"), "copies": 0, "clone-tokens": table.limit("clone-tokens")}


def main(argv: List[str]) -> int:
    args = ratchet_report_cli.parse("config-quality-report", __doc__ or "", measure.LANGUAGES, argv)
    files, git = tool_files.Files.here(), tool_git.Git.here()  # the composition root: the checkout it runs in
    try:
        limit = limits(rules_table.load("."))
        results: List[bool] = []
        for name in args.language or sorted(measure.LANGUAGES):
            language = measure.LANGUAGES[name]
            rows = measure.rows(language, ratchet_report_cli.sources(git, files, language.holds))
            if args.out:
                ratchet_report_cli.write_over_limit(files, args.out, name, ratchet.SPECS[name],
                                                    ratchet_report_cli.over_limit(rows, limit))
            results.append(ratchet_report_cli.run_one(ratchet.SPECS[name], name, rows, limit, args, SHOWN))
        return 0 if all(results) else 1
    except (OSError, ValueError, tool_git.Failed) as e:
        print(f"config-quality-report: {e}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
