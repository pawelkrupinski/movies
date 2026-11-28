"""KNOWN TITLE MARKERS and their spellings in a corpus (scripts/recording_diff.py): sneak, preview, pokaz, double bill,
certificates -- each a family of spellings the title rules have to read."""
import re
from collections import defaultdict
from typing import Dict, List, Pattern, Tuple

# Known title markers: each is a family of spellings the title rules have to read. `follow`: the marker word stands
# alone and its qualifier is the next word ("Sneak Over", "Pokaz przyjazny"), so the spelling takes the next word too.
MARKERS: List[Tuple[str, Pattern[str], bool]] = [
    ("sneak", re.compile(r"sneak", re.I), True),
    ("preview", re.compile(r"preview|vorpremiere|przedpremier|avant-?premi", re.I), True),
    ("pokaz", re.compile(r"pokaz", re.I), True),
    ("double bill", re.compile(r"double[\s_-]*(?:bill|feature)|doppel(?:vorstellung|programm)|podw[oó]jn\w*", re.I), False),
    ("certificate", re.compile(r"[(\[](?:U|PG|12A?|15|18|R18|PG-13|NC-17|R|FSK ?\d{1,2})[)\]]", re.I), False),
]


def _word_spellings(words: List[str], marker: Pattern[str], follow: bool) -> List[str]:
    """The words holding the marker, each with its next word when the marker stands alone."""
    found: List[str] = []
    for i, word in enumerate(words):
        match = marker.search(word)
        if match:
            standalone = follow and match.group(0) == word and i + 1 < len(words)
            found.append(f"{word} {words[i + 1]}".casefold() if standalone else word.casefold())
    return found


def _spanning_spellings(title: str, marker: Pattern[str]) -> List[str]:
    """A marker spanning words ("Double Bill"): the words it spans."""
    found: List[str] = []
    for match in marker.finditer(title):
        start = title.rfind(" ", 0, match.start()) + 1
        end = title.find(" ", match.end())
        found.append(title[start:end if end >= 0 else len(title)].casefold())
    return found


def _spellings(title: str, marker: Pattern[str], follow: bool) -> List[str]:
    """The marker's spellings in a title: the word holding the match, and its next word when the marker stands alone."""
    return _word_spellings(title.split(), marker, follow) or _spanning_spellings(title, marker)


def marker_spellings(venues: Dict[str, Dict[str, object]]) -> Dict[Tuple[str, str], List[Tuple[str, str]]]:
    """(marker, spelling) → [(venue, title)...]"""
    out: Dict[Tuple[str, str], List[Tuple[str, str]]] = defaultdict(list)
    for venue, films in venues.items():
        for title in films:
            for name, marker, follow in MARKERS:
                for spelling in _spellings(title, marker, follow):
                    out[(name, spelling)].append((venue, title))
    return out
