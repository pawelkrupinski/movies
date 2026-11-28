"""THE DECLARATIVE-CONFIG QUALITY RATCHETS (the pre-push `configquality` check; CLAUDE.md, "Design rules": every language
but bash is ratcheted on the one limits table). Nix, Terraform and the Twirl templates' markup have no function to measure
(their `<script>`s are production JS, held by the JS/TS ratchet), so they are held to what a declarative file can grow
past -- the Python and JS/TS ratchets' engine (baseline_ratchet.py), each kind under the Scala ratchet's trailer:

  Length:       file-lines <file>
  Duplication:  copies <file>|<file>... (the copies of every clone group spanning exactly those files), clone-tokens

HELD is every tracked `.nix` and `.tf`, and the `.scala.html` under web/src/main/twirl/. A row only falls, and the fall is
recorded in the push making it (scripts/config-quality-report --update)."""
from __future__ import annotations

import os
import sys
from typing import Dict, Tuple

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))  # the engine beside it, under `python3 -I` too
import baseline_ratchet  # noqa: E402

TRAILERS: Dict[str, Tuple[str, ...]] = {"Length": ("file-lines",), "Duplication": ("copies", "clone-tokens")}
KINDS = tuple(kind for kinds in TRAILERS.values() for kind in kinds)
BASELINES = {"nix": "test/resources/nix-quality-baseline.tsv", "terraform": "test/resources/terraform-quality-baseline.tsv",
             "twirl": "test/resources/twirl-quality-baseline.tsv"}
UPDATE = "scripts/config-quality-report --update"
TITLES = {"nix": "Nix", "terraform": "Terraform", "twirl": "Twirl markup"}


def header(language: str) -> str:
    return f"""# THE {language.upper()} QUALITY BASELINE (scripts/config-quality-report --update writes it; scripts/ci/config_quality_ratchet.py
# holds every push touching {TITLES[language]} files to it). Every {TITLES[language]} file and clone set past a limit of
# test/resources/design-rules.tsv, frozen:
#   file-lines <file>                                                           Length:
#   copies <file>|<file>... (clone groups spanning exactly them), clone-tokens  Duplication:
# A row may only fall, and the push making a fall records it (--update, no trailer); a row added or raised, or a limit
# raised, lands only in a commit carrying its kind's trailer (CLAUDE.md, "Design rules")."""


HINT = ("Split the file, extract the clone -- or record it: scripts/config-quality-report --update, committed with"
        " the kind's trailer (`Length:`, `Duplication: <why>`) in the commit message's final trailer block.")


def spec(language: str) -> baseline_ratchet.Spec:
    return baseline_ratchet.Spec(
        held=lambda part: True, trailers=TRAILERS, held_rows="every file past a limit", update_command=UPDATE, hint=HINT,
        header=header(language), baseline=BASELINES[language], tool=f"{language} quality", title=f"{language.upper()} QUALITY")


SPECS = {language: spec(language) for language in BASELINES}
