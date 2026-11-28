"""TITLE RE-SPELLINGS: how one title re-spells another (scripts/recording_diff.py, scripts/listing_history.py read it).

`respelling(a, b)` names the kinds of difference between two titles -- whitespace, case, accents, punctuation, truncation --
each one needed, so a resolver keyed on the spelling sees a new title only where a kind says so."""
import re
import unicodedata
from typing import Callable, List, Optional, Tuple

TRUNCATION_MIN = 20          # a shorter title below this many letters is another title, not a truncation of the longer


def _ws(s: str) -> str:
    return re.sub(r"\s+", " ", s).strip()


def _case(s: str) -> str:
    return s.casefold()


def strip_accents(s: str) -> str:
    return "".join(c for c in unicodedata.normalize("NFKD", s) if not unicodedata.combining(c))


def _punct(s: str) -> str:
    # a mark at either end leaves no space there: only one inside the title is a word break
    return re.sub(r"[^\w\s]|_", " ", s).strip()


# applied in this order, so the whitespace a removed punctuation mark leaves is collapsed
_TRANSFORMS: List[Tuple[str, Callable[[str], str]]] = [("case", _case), ("accents", strip_accents), ("punctuation", _punct), ("whitespace", _ws)]
_ORDER: List[str] = ["whitespace", "case", "accents", "punctuation"]


def _canonical(s: str, skip: Optional[str] = None) -> str:
    for name, transform in _TRANSFORMS:
        if name != skip:
            s = transform(s)
    return s


def _related(a: str, b: str, truncation: bool) -> bool:
    if a == b:
        return True
    if not truncation:
        return False
    short, long_ = sorted((a, b), key=len)
    return len(short.replace(" ", "")) >= TRUNCATION_MIN and long_.startswith(short)


def respelling(a: str, b: str) -> Optional[List[str]]:
    """How title `b` re-spells title `a`: the kinds of difference (whitespace, case, accents, punctuation, truncation),
    each one needed — without it the two are not the same title; None when they are two titles."""
    if a == b:
        return None
    ca, cb = _canonical(a), _canonical(b)
    truncation = ca != cb
    if not _related(ca, cb, truncation):
        return None
    kinds = [kind for kind in _ORDER if not _related(_canonical(a, kind), _canonical(b, kind), truncation)]
    return kinds + (["truncation"] if truncation else [])
