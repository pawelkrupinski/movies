"""THE DESIGN RULES TABLE, read (test/resources/design-rules.tsv; its header says every row's shape): the limits, the
name rules, the effect adapters, the directories whose distance is measured and the directories' layers -- one table
for Scala, Python and JS/TS, so a rule is stated once. In the shared library: the general rule core
(scripts/design-rules/) and every language's own measure (scripts/py-quality, scripts/js-quality) read it."""
from __future__ import annotations

import dataclasses
import os
from typing import Dict, FrozenSet, List, Optional, Set, Tuple

TABLE = "test/resources/design-rules.tsv"
LANGUAGES = ("scala", "py", "js")


@dataclasses.dataclass(frozen=True)
class Layer:
    language: str
    directory: str
    rank: int
    why: str


@dataclasses.dataclass(frozen=True)
class Rules:
    limits: Dict[str, float]
    limit_languages: Dict[str, FrozenSet[str]]
    suffixes: Tuple[str, ...]
    abbreviations: FrozenSet[str]
    idioms: FrozenSet[str]
    adapters: Dict[Tuple[str, str], FrozenSet[str]]   # (language, module) -> the effects it may touch
    distance_dirs: FrozenSet[Tuple[str, str]]          # (language, directory)
    layers: Tuple[Layer, ...]

    def limit(self, kind: str) -> float:
        return self.limits[kind]

    def adapter_effects(self, language: str, module: str) -> FrozenSet[str]:
        return self.adapters.get((language, module), frozenset())

    def layer_of(self, language: str, directory: str) -> Optional[Layer]:
        """The row naming exactly this directory (a directory is its own package: no prefix match)."""
        for layer in self.layers:
            if layer.language == language and layer.directory == directory:
                return layer
        return None

    def words_refused(self) -> Set[str]:
        return set(self.abbreviations) - set(self.idioms)


@dataclasses.dataclass
class Collected:
    """The rows read so far, by kind: what `parse` freezes into a `Rules`."""
    limits: Dict[str, float] = dataclasses.field(default_factory=lambda: {})
    limit_languages: Dict[str, FrozenSet[str]] = dataclasses.field(default_factory=lambda: {})
    suffixes: List[str] = dataclasses.field(default_factory=lambda: [])
    abbreviations: Set[str] = dataclasses.field(default_factory=lambda: set())
    idioms: Set[str] = dataclasses.field(default_factory=lambda: set())
    adapters: Dict[Tuple[str, str], FrozenSet[str]] = dataclasses.field(default_factory=lambda: {})
    distance_dirs: Set[Tuple[str, str]] = dataclasses.field(default_factory=lambda: set())
    layers: List[Layer] = dataclasses.field(default_factory=lambda: [])

    def limit(self, cells: List[str]) -> None:
        self.limits[cells[1]] = float(cells[2])
        self.limit_languages[cells[1]] = frozenset(cells[3].split())

    def add(self, kind: str, cells: List[str]) -> None:
        if kind == "limit":
            self.limit(cells)
        elif kind == "suffix":
            self.suffixes.append(cells[1])
        elif kind in ("abbreviation", "idiom"):
            (self.abbreviations if kind == "abbreviation" else self.idioms).add(cells[1].lower())
        elif kind == "adapter":
            self.adapters[(cells[1], cells[2])] = frozenset(cells[3].split())
        elif kind == "distance":
            self.distance_dirs.add((cells[1], cells[2]))
        elif kind == "layer":
            self.layers.append(Layer(cells[1], cells[2], int(cells[3]), cells[4]))
        else:
            raise ValueError(f"unknown row kind {kind!r}")

    def rules(self) -> Rules:
        return Rules(self.limits, self.limit_languages, tuple(self.suffixes), frozenset(self.abbreviations),
                     frozenset(self.idioms), self.adapters, frozenset(self.distance_dirs), tuple(self.layers))


def parse(text: str) -> Rules:
    rows = Collected()
    for number, line in enumerate(text.splitlines(), 1):
        if not line.strip() or line.startswith("#"):
            continue
        cells = line.split("\t")
        try:
            rows.add(cells[0], cells)
        except (IndexError, ValueError) as e:
            raise ValueError(f"{TABLE}:{number}: {e}: {line!r}") from e
    return rows.rules()


def load(root: str) -> Rules:
    with open(os.path.join(root, TABLE), encoding="utf-8") as f:
        return parse(f.read())


def checkout_root() -> str:
    """The checkout this library sits in (scripts/lib/..)."""
    return os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
