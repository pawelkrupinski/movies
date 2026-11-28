"""THE SWIFT AND KOTLIN QUALITY RATCHETS (the pre-push `mobilequality` check; CLAUDE.md, "Code quality ratchets": every
language but bash is ratcheted on the one limits table, test/resources/design-rules.tsv): the iOS app's Swift (ios/) and
the Android app's Kotlin (android/), measured by scripts/mobile-quality-report (pinned lizard), may not grow past their
baselines -- the Python and JS/TS ratchets' engine (baseline_ratchet.py), each kind under the Scala ratchet's trailer:

  Complexity:   cognitive, cyclomatic                 <file>:<function>[#n]
  Length:       method-lines <file>:<function>[#n], file-lines <file>
  Duplication:  copies <file>|<file>... (the copies of every clone group spanning exactly those files), clone-tokens

HELD is the production code: Swift under ios/Kinowo/ (the app; ios/Tests and ios/KinowoUITests are reported, never
held), Kotlin under an android/ module's src/main/, src/tuning/ or src/noTuning/ and the Gradle scripts (`*.kts`: build
configuration is code, 2026-11-28) -- src/test, androidTest and sharedTest are reported, never held. A row only falls, and the fall is recorded in the push making it."""
from __future__ import annotations

import os
import sys
from typing import Dict, Tuple

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))  # the engine beside it, under `python3 -I` too
import baseline_ratchet  # noqa: E402

TRAILERS: Dict[str, Tuple[str, ...]] = {
    "Complexity": ("cognitive", "cyclomatic"),
    "Length": ("method-lines", "file-lines"),
    "Duplication": ("copies", "clone-tokens"),
}
KINDS = tuple(kind for kinds in TRAILERS.values() for kind in kinds)
BASELINES = {"swift": "test/resources/swift-quality-baseline.tsv", "kotlin": "test/resources/kotlin-quality-baseline.tsv"}
PRODUCTION = {"swift": ("ios/Kinowo/",), "kotlin": ("/src/main/", "/src/tuning/", "/src/noTuning/")}
UPDATE = "scripts/mobile-quality-report --update"


def held(language: str, part: str) -> bool:
    """A key's file is the language's production code (a test's, a UI test's or a build script's is reported only)."""
    path = part.split(":", 1)[0]
    if language == "swift":
        return path.startswith(PRODUCTION["swift"])
    return path.startswith("android/") and (path.endswith(".kts") or any(root in path for root in PRODUCTION["kotlin"]))


def header(language: str) -> str:
    name = language.capitalize()
    return f"""# THE {language.upper()} QUALITY BASELINE (scripts/mobile-quality-report --update writes it; scripts/ci/mobile_quality_ratchet.py
# holds every push touching the {name} code to it). Every production {name} function, file and clone set past a limit
# of test/resources/design-rules.tsv, frozen:
#   cognitive / cyclomatic <file>:<function>[#n]                                Complexity:
#   method-lines <file>:<function>[#n], file-lines <file>                       Length:
#   copies <file>|<file>... (clone groups spanning exactly them), clone-tokens  Duplication:
# A row may only fall, and the push making a fall records it (--update, no trailer); a row added or raised, or a limit
# raised, lands only in a commit carrying its kind's trailer (CLAUDE.md, "Code quality ratchets")."""


def spec(language: str) -> baseline_ratchet.Spec:
    return baseline_ratchet.Spec(
        title=f"{language.upper()} QUALITY", tool=f"{language} quality", baseline=BASELINES[language],
        header=header(language),
        hint=("Split the function or file, extract the clone -- or record it: scripts/mobile-quality-report --update,"
              " committed with the kind's trailer (`Complexity:`, `Length:`, `Duplication: <why>`) in the commit"
              " message's final trailer block."),
        update_command=UPDATE, held_rows="production rows past a limit; tests reported, not held", trailers=TRAILERS,
        held=lambda part: held(language, part))


SPECS = {language: spec(language) for language in BASELINES}
