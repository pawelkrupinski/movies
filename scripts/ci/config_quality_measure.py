"""THE DECLARATIVE-CONFIG MEASURES (scripts/config-quality-report, config_quality_report.py): what is held of a language
that has no functions to measure -- Nix (`infra/`), Terraform and the Twirl templates' markup (the owner's 2026-11-28
decision: a file-length limit and the clones, on the one limits table, test/resources/design-rules.tsv): every file's
lines (blank ones and comments counted, as the other languages' file-lines) and the clone groups of 50+ tokens over the
file's tokens, comments left out, names kept as written (scripts/lib/code_tools/clone_groups.py)."""
from __future__ import annotations

import os
import re
import sys
from dataclasses import dataclass
from typing import Dict, List

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from code_tools import clone_groups  # noqa: E402

Row = clone_groups.Row
TOKEN = re.compile(r"\"(?:\\.|[^\"\\])*\"|[A-Za-z_][\w.'-]*|\d+|\S")
HASH_COMMENT = r"#[^\n]*"
BLOCK_COMMENT = r"/\*.*?\*/"
TWIRL_COMMENT = r"@\*.*?\*@"
HTML_COMMENT = r"<!--.*?-->"


@dataclass(frozen=True)
class Language:
    name: str
    suffix: str
    comments: "re.Pattern[str]"
    root: str = ""

    def holds(self, path: str) -> bool:
        return path.startswith(self.root) and path.endswith(self.suffix)


LANGUAGES = {
    "nix": Language("nix", ".nix", re.compile(f"{HASH_COMMENT}|{BLOCK_COMMENT}", re.S)),
    "terraform": Language("terraform", ".tf", re.compile(f"{HASH_COMMENT}|//[^\\n]*|{BLOCK_COMMENT}", re.S)),
    "twirl": Language("twirl", ".scala.html", re.compile(f"{TWIRL_COMMENT}|{HTML_COMMENT}", re.S), "web/src/main/twirl/"),
}


def blanked(comments: "re.Pattern[str]", text: str) -> str:
    """The text with each comment replaced by its newlines alone, so the tokens keep their lines."""
    return comments.sub(lambda found: "\n" * found.group(0).count("\n"), text)


def tokens_of(language: Language, text: str) -> List[clone_groups.Token]:
    out: List[clone_groups.Token] = []
    for number, line in enumerate(blanked(language.comments, text).split("\n"), 1):
        out += [(token, number) for token in TOKEN.findall(line)]
    return out


def rows(language: Language, sources: Dict[str, str]) -> List[Row]:
    """Every row of one language's sources (path -> text): each file's lines, each clone set."""
    out: List[Row] = [("file-lines", path, len(text.splitlines())) for path, text in sorted(sources.items())]
    return out + clone_groups.copies_rows({path: tokens_of(language, text) for path, text in sources.items()})
