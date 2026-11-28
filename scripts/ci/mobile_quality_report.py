#!/usr/bin/env python3
"""THE SWIFT AND KOTLIN QUALITY REPORT (scripts/mobile-quality-report): the code-quality measures every language is held
to (CLAUDE.md, "Code quality ratchets"; the limits are test/resources/design-rules.tsv's) taken over the iOS app's Swift
(ios/) and the Android app's Kotlin (android/), and GATED by THE SWIFT AND KOTLIN QUALITY RATCHETS
(scripts/ci/mobile_quality_ratchet.py, the pre-push `mobilequality` check) against
test/resources/{swift,kotlin}-quality-baseline.tsv:

  scripts/mobile-quality-report --check [--language L]... [--range A..B]...   held rows against the baselines (exit 1)
  scripts/mobile-quality-report --update [--language L]...                    the baselines rewritten from the measure
  scripts/mobile-quality-report [--language L]... [--out DIR]                  the worst rows as markdown; DIR keeps
                                                                               each language's over-limit.tsv

The measures (mobile_quality_measure.py), by lizard (pinned in scripts/lizard-requirements.txt, built once into a venv by scripts/python-venv; a
missing python3 or a venv that cannot be built BLOCKS the push, with the fix):
  cognitive <= 10, cyclomatic <= 10, function <= 50 lines, file <= 500 lines, clone groups of 50+ tokens
L is swift or kotlin (default both). Exit 0; 1 on a violation; 2 on an error."""
from __future__ import annotations

import importlib
import os
import sys
from typing import Dict, Iterable, List, Protocol, Tuple, cast

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))  # scripts/lib
import mobile_quality_measure as measure  # noqa: E402
import mobile_quality_ratchet as ratchet  # noqa: E402
import ratchet_report_cli  # noqa: E402
import rules_table  # noqa: E402
from seams import tool_files  # noqa: E402
import tool_git  # noqa: E402

PINNED = "1.26.1"  # requirements.txt's lizard
REQUIREMENTS = "scripts/lizard-requirements.txt"
FUNCTION_KINDS = ("cognitive", "cyclomatic", "method-lines", "file-lines")
Row = measure.Row


class LizardModule(Protocol):
    version: str

    def get_extensions(self, names: List[str]) -> List[object]: ...

    def FileAnalyzer(self, extensions: List[object]) -> measure.Analyzer: ...  # noqa: N802 (lizard's own name)


class TokenReader(Protocol):
    def generate_tokens(self, source_code: str) -> Iterable[str]: ...


class LanguagesModule(Protocol):
    def get_reader_for(self, filename: str) -> TokenReader: ...


class PinnedLizard:
    """lizard, the pinned version only: its cognitive extension on, each language's reader for the tokens."""

    def __init__(self, module: LizardModule, languages: LanguagesModule) -> None:
        if module.version != PINNED:
            raise RuntimeError(f"lizard {module.version} is on the path, {REQUIREMENTS} pins {PINNED}: run through"
                               " scripts/mobile-quality-report, which puts the pinned venv first")
        self.analyzer = module.FileAnalyzer(module.get_extensions(["cognitive"]))
        self.languages = languages

    def analyze(self, path: str, code: str) -> measure.FileInfo:
        return self.analyzer.analyze_source_code(path, code)

    def tokens(self, path: str, code: str) -> Iterable[str]:
        return self.languages.get_reader_for(path).generate_tokens(code)


def limits(table: rules_table.Rules) -> Dict[str, float]:
    return {**{kind: table.limit(kind) for kind in FUNCTION_KINDS}, "copies": 0,
            "clone-tokens": table.limit("clone-tokens")}


def measured(git: tool_git.Git, files: tool_files.Files, lizard: PinnedLizard, language: str) -> List[Row]:
    return measure.rows(lizard, ratchet_report_cli.sources(git, files, measure.LANGUAGES[language].holds))


def main(argv: List[str]) -> int:
    args = ratchet_report_cli.parse("mobile-quality-report", __doc__ or "", measure.LANGUAGES, argv)
    files, git = tool_files.Files.here(), tool_git.Git.here()  # the composition root: the checkout it runs in
    try:
        lizard = PinnedLizard(cast(LizardModule, importlib.import_module("lizard")),
                              cast(LanguagesModule, importlib.import_module("lizard_languages")))
        limit = limits(rules_table.load("."))
        results: List[Tuple[str, bool]] = []
        for language in args.language or sorted(measure.LANGUAGES):
            rows = measured(git, files, lizard, language)
            if args.out:
                ratchet_report_cli.write_over_limit(files, args.out, language, ratchet.SPECS[language],
                                                    ratchet_report_cli.over_limit(rows, limit))
            results.append((language, ratchet_report_cli.run_one(ratchet.SPECS[language], language, rows, limit, args,
                                                                 (*FUNCTION_KINDS, "copies"))))
        return 0 if all(ok for _, ok in results) else 1
    except ImportError as e:
        print(f"mobile-quality-report: {e}: run it as scripts/mobile-quality-report (the pinned venv)", file=sys.stderr)
        return 2
    except (OSError, RuntimeError, ValueError, tool_git.Failed) as e:
        print(f"mobile-quality-report: {e}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
