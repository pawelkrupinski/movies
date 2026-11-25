"""How the coupling baseline and the layer table moved between two revisions (coupling_ratchet.py, its module
docstring): what grew (needing a `Coupling:` trailer), what fell (`--update` records it), which frozen rows rose and
what admits it (an honest split, a rename, a limit change), and how the layer table loosened."""
from __future__ import annotations

from typing import AbstractSet, Dict, List, Optional, Tuple

from coupling_rows import (DISTANCE_CAP, DISTANCE_SPLIT_TOLERANCE, FROZEN, PRESENCE, VALUED, Layers, Rows, ceilings,
                           layer_of, limits, moved, of_kind, refrozen, remeasured_only)

DISTANCE_CUT = ("move a leaf value type down to models or tools, or a trait to the seam"
                " (84a5db6d1 took services.movies from 0.39 to 0.28 so)")


def growth(old: Rows, new: Rows) -> List[str]:
    """How the baseline grew from `old` to `new`: rows added, values raised (the presence kinds' weights aside)."""
    out: List[str] = []
    old_lim, new_lim = limits(old), limits(new)
    for key in sorted(new_lim):
        if new_lim[key] > old_lim.get(key, 0):
            out.append(f"limit {key} raised {old_lim.get(key, 0)} -> {new_lim[key]}")
    out += [f"{kind} rows taken again under new limits ({len(of_kind(new, kind))} frozen)"
            for kind, _, _ in FROZEN if refrozen(old, new, kind)]
    out += [f"{kind} {key} added" for kind in PRESENCE for key in sorted(set(of_kind(new, kind)) - set(of_kind(old, kind)))]
    old_ce = of_kind(old, "ce")
    for key, ce in sorted(of_kind(new, "ce").items()):
        allowed = old_ce.get(key, old_lim["new-package-ce"])
        if ce > allowed:
            out.append(f"ce {key} raised {allowed} -> {ce}")
    return out


def falls(current: Rows, baseline: Rows) -> List[str]:
    """The baseline rows the measured code is below (or no longer has): falls `--update` would record."""
    out = [f"{kind} {key} gone" for kind in PRESENCE
           for key in sorted(set(of_kind(baseline, kind)) - set(of_kind(current, kind)))]
    for kind in VALUED:
        cur = of_kind(current, kind)
        out += [f"{kind} {key} {value} -> {cur.get(key, 0 if kind == 'ce' else 'below the limit')}"
                for key, value in sorted(of_kind(baseline, kind).items()) if cur.get(key, 0) < value]
    return out


def past_ceiling(value: int, cap: int) -> str:
    return (f", past its ceiling {cap}: free {(value - cap) / 100:.2f} of its D (the ceiling only falls) -- {DISTANCE_CUT}"
            if value > cap else "")


def tolerated_rise(key: str, was: int, value: int, cap: int, split_packages: AbstractSet[str]) -> bool:
    """Whether a distance rise is a split's: the package holds or is reached by it, the rise within the tolerance and the ceiling."""
    return key in split_packages and value - was <= DISTANCE_SPLIT_TOLERANCE and value <= cap


def raised_row(kind: str, key: str, was: int, value: int, caps: Dict[str, int], split_packages: AbstractSet[str]) -> List[str]:
    """The refusal of one frozen row that `value` raised past `was` (none when it did not, or a split's tolerance admits it)."""
    cap = caps.get(key, was) if kind == "distance" else value
    if value <= was or (kind == "distance" and tolerated_rise(key, was, value, cap, split_packages)):
        return []
    return [f"{kind} {key} raised {was} -> {value}" + past_ceiling(value, cap)]


def raised_rows(kind: str, old: Rows, new: Rows, split: AbstractSet[str], split_packages: AbstractSet[str],
                renamed: Dict[Tuple[str, str], str]) -> List[str]:
    """The rows of a frozen `kind` that `new` raised or added past `old`, unless an honest split, a rename or a
    tolerated distance rise admits them."""
    out: List[str] = []
    before, after = of_kind(old, kind), of_kind(new, kind)
    caps = ceilings(old)
    for key, value in sorted(after.items()):
        if key in before:
            out += raised_row(kind, key, before[key], value, caps, split_packages)
        elif not is_credited(kind, key, value, before, after, split, renamed):
            out.append(f"{kind} {key} added at {value}")
    return out


def is_credited(kind: str, key: str, value: int, before: Dict[str, int], after: Dict[str, int], split: AbstractSet[str],
                renamed: Dict[Tuple[str, str], str]) -> bool:
    """Whether a row new in `after` is no growth: a frozen row that moved, a piece of an honest split, or a rename's."""
    return moved(key, value, before, after) or (kind == "fanout" and key in split) or (kind, key) in renamed


def raised_ceilings(old: Rows, new: Rows, renamed: Dict[Tuple[str, str], str]) -> List[str]:
    """The ceilings (THE CEILING) `new` raised past `old`'s, or past the ceiling a renamed package carries."""
    out: List[str] = []
    was = ceilings(old)
    for key, cap in sorted(ceilings(new).items()):
        old_name = renamed.get(("distance", key))
        if key in was and cap > was[key]:
            out.append(f"{DISTANCE_CAP} {key} raised {was[key]} -> {cap} -- a ceiling only falls"
                       + ("" if (DISTANCE_CAP, key) in new else f" (no `{DISTANCE_CAP}` row: scripts/coupling-report --update writes it)"))
        elif old_name is not None and old_name in was and cap > was[old_name]:
            old_cap = was[old_name]
            out.append(f"{DISTANCE_CAP} {key} {cap} is past {old_name}'s {old_cap}: a renamed package keeps"
                       f" its ceiling -- set `{DISTANCE_CAP}\t{key}\t{old_cap}` in the baseline")
    return out


def frozen_growth(old: Rows, new: Rows, split: AbstractSet[str] = frozenset(), split_packages: AbstractSet[str] = frozenset(),
                  renamed: Optional[Dict[Tuple[str, str], str]] = None) -> List[str]:
    """The frozen rows the baseline gained or raised from `old` to `new` -- never admitted, whatever the trailer, unless
    the kind's limit itself changed (`refrozen`: the rows are taken again, and the limit change carries the trailer), the
    row is a piece of an honest split (`split`, from `split_credit`), or the distance of a package holding one
    (`split_packages`) rose by DISTANCE_SPLIT_TOLERANCE at most and stays within its ceiling at `old`, or the row is a
    rename's (`renamed`, (kind, key) from `rename_credit`) -- and a ceiling raised (THE CEILING)."""
    renamed = renamed or {}
    out: List[str] = []
    for kind, _, _ in FROZEN:
        if not refrozen(old, new, kind):
            out += raised_rows(kind, old, new, split, split_packages, renamed)
    if not refrozen(old, new, "distance") or remeasured_only(old, new, "distance"):
        out += raised_ceilings(old, new, renamed)
    return out


def loosening(old: Layers, new: Layers) -> List[str]:
    """How the layer table loosened from `old` to `new`: an `allow` edge added, or a package that had a layer or a rank
    given another (up, its own edges may point higher; down, lower packages may reach it). A new package taking its
    first layer or rank, or a row dropped (its packages unplaced: a violation), loosens nothing."""
    out = [f"allow {edge} added" for edge in sorted(new[1] - old[1])]
    for prefix in sorted(set(old[0]) | set(new[0])):
        was, now = layer_of(old[0], prefix), layer_of(new[0], prefix)
        if was is not None and now is not None and was != now:
            out.append(f"{prefix} moved from layer {was} to {now}")
    for pkg in sorted(set(old[2]) & set(new[2])):
        if old[2][pkg] != new[2][pkg]:
            out.append(f"{pkg} re-ranked from {old[2][pkg]} to {new[2][pkg]}")
    return out
