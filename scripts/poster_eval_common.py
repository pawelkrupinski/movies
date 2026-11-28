"""What every part of poster_eval shares (scripts/poster_eval.py's logic, split by concept): the countries, PosterEvidence's
baseline variant, the dataset's shapes, the log line, and numpy.

numpy is loaded by name, not `import numpy`: the tool's own dependency (as vips is), absent from the CI container and the
type checker's environment, where a static import would be an unresolved one and every array an unknown type. Arrays are
`Any` from here on."""
from __future__ import annotations

import importlib
import os
import sys
from typing import Any, Dict, Tuple

Here = os.path.dirname(os.path.abspath(__file__))
sys.path[:0] = [Here, os.path.join(Here, "ci"), os.path.join(Here, "lib")]
from mongo import mongo_local  # noqa: E402

np: Any = importlib.import_module("numpy")

Array = Any
Meta = Dict[str, Any]
Arrays = Dict[str, Any]
Variant = Dict[str, int]
Key = Tuple[str, ...]  # a listing's (venue, raw title)

Root = os.path.dirname(Here)
Countries = ("pl", "uk", "de", "us", "es")
Databases = {cc: mongo_local.mirror_database(cc) for cc in Countries}
Language = {"pl": "pl", "uk": "en", "us": "en", "de": "de", "es": "es"}
Baseline: Variant = {"posters": 8, "vote": 4, "veto_match": 8, "veto": 10}  # PosterEvidence's constants
FiledPosters = 8  # PosterEvidence.FilmPosters: how many a film's filed answer holds
RefreshHours = 6
ExtractVersion = 1
Missing = 255  # no distance: the venue poster or every poster of the film unhashed
Mask = (1 << 64) - 1
UserAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0 Safari/537.36"


def log(*parts: object) -> None:
    print(*parts, file=sys.stderr, flush=True)
