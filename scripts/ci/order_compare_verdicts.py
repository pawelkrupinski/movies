"""order_compare.py's records read back: every leg's verdict as markdown (`summary`), scala's reverse and shuffled legs
compared, and the run's one word with its JSON (`verdict`)."""
from __future__ import annotations

import glob
import os
from typing import Iterable, List, Tuple

NO_RECORD = "no record (the leg did not finish)"


def write(path: str, text: str) -> None:
    with open(path, "w") as f:
        f.write(text)


def read(path: str) -> str:
    """A record's text, its trailing newlines dropped (`$(cat f)`); "" when it is not there."""
    try:
        with open(path) as f:
            return f.read().rstrip("\n")
    except OSError:
        return ""


def lines_of(path: str) -> List[str]:
    text = read(path)
    return text.split("\n") if text else []


def lines(items: Iterable[str]) -> str:
    """A line each, or nothing at all."""
    return "".join(f"{i}\n" for i in items)


def names(tests: Iterable[str]) -> str:
    return ", ".join(f"`{t}`" for t in tests)


def recorded(directory: str) -> List[Tuple[str, str]]:
    """(leg, verdict) of every <leg>.verdict, in name order."""
    paths = sorted(p for p in glob.glob(os.path.join(directory, "*.verdict")) if os.path.isfile(p))
    return [(os.path.basename(p)[:-len(".verdict")], read(p)) for p in paths]


def leg_line(directory: str, leg: str, verdict: str) -> str:
    if verdict == "pass":
        return f"- `{leg}`: passed"
    if verdict == "regression":
        failing = names(lines_of(os.path.join(directory, f"{leg}.control.txt")))
        return f"- `{leg}`: **REGRESSION, not an order dependence**: {failing} fail in default order too"
    if verdict == "order":
        return f"- `{leg}`: **order dependence (or a flake)**: {names(lines_of(os.path.join(directory, f'{leg}.txt')))}" \
               " pass in default order"
    return f"- `{leg}`: no order verdict (it failed naming no test)"


def summary(directory: str, seed: str) -> str:
    out = ["### Order verdict, every leg"]
    out += [leg_line(directory, leg, verdict) for leg, verdict in recorded(directory)] or [
        "- no leg recorded a verdict (none reached its tests: a preflight, its setup or the runner)"]
    return lines(out) + scala_compare(directory, seed)


class ScalaLegs:
    """Scala's two orders side by side: a suite failing in BOTH is a regression; one in one order alone a candidate."""

    def __init__(self, directory: str) -> None:
        self.directory = directory
        for order in ("reverse", "shuffled"):
            if not os.path.isfile(self.path(order, "txt")):
                write(self.path(order, "txt"), "")
        self.failed = {order: lines_of(self.path(order, "txt")) for order in ("reverse", "shuffled")}

    def path(self, order: str, kind: str) -> str:
        return os.path.join(self.directory, f"scala-{order}.{kind}")

    def result(self, order: str) -> str:
        return read(self.path(order, "result")) if os.path.isfile(self.path(order, "result")) else NO_RECORD

    def reached(self, order: str) -> bool:
        """A leg gives a verdict when it passed or named what failed; a suite failing in one order is compared only
        against a leg that did, never against one cut short (a compile error, a timeout) before reaching it."""
        return self.result(order) == "success" or os.path.getsize(self.path(order, "txt")) > 0

    def only(self, order: str, other: str) -> List[str]:
        return [s for s in self.failed[order] if s not in self.failed[other]]

    def one_order(self, order: str, other: str, seed: str) -> List[str]:
        suites = self.only(order, other)
        if not suites:
            return []
        if not self.reached(other):
            return [f"{names(suites)} failed in the {order} order; the {other} leg reached no verdict to compare it with"]
        replay = "reverse" if order == "reverse" else seed or "<seed>"
        return [f"**order dependence**: {names(suites)} fails only in the {order} order -- replay: "
                f"`TEST_ORDER_SEED={replay} sbt testUnitNoE2e`"]

    def findings(self, seed: str) -> List[str]:
        """The regression first, then each order's suites -- those the other leg reached no verdict to compare with
        before the order dependences -- then each leg that reached no verdict."""
        orders = ("reverse", "shuffled")
        out = self.in_both()
        uncompared = [o for o in orders if not self.reached(self.other(o))]
        for o in uncompared + [o for o in orders if o not in uncompared]:
            out += self.one_order(o, self.other(o), seed)
        out += [self.no_verdict(o) for o in orders if not self.reached(o)]
        return out or ["both orders passed"]

    def in_both(self) -> List[str]:
        both = [s for s in self.failed["reverse"] if s in self.failed["shuffled"]]
        return [f"**regression, not an order dependence: the same suite fails in both orders**: {names(both)}"] if both else []

    def no_verdict(self, order: str) -> str:
        return (f"**no order verdict** from the {order} leg: it ended {self.result(order)} naming no failing suite (a"
                " compile error, a timeout or the runner)")

    @staticmethod
    def other(order: str) -> str:
        return "shuffled" if order == "reverse" else "reverse"


def scala_compare(directory: str, seed: str) -> str:
    return lines(["### Scala suites, reverse vs shuffled"] + [f"- {f}" for f in ScalaLegs(directory).findings(seed)])


def next_word(verdict: str, word: str) -> str:
    """The run's word after one more leg: regression > order > none > pass."""
    if verdict == "regression":
        return "regression"
    if verdict == "order":
        return word if word == "regression" else "order"
    if verdict == "none":
        return word if word in ("regression", "order") else "none"
    return word or "pass"


def run_verdict(directory: str) -> str:
    word = ""
    legs: List[str] = []
    for leg, verdict in recorded(directory):
        word = next_word(verdict, word)
        failed = ",".join(f'"{t}"' for t in lines_of(os.path.join(directory, f"{leg}.txt")) if t)
        control = ",".join(f'"{t}"' for t in lines_of(os.path.join(directory, f"{leg}.control.txt")) if t)
        legs.append(f'"{leg}":{{"verdict":"{verdict}","failed":[{failed}],"default_order_failed":[{control}]}}')
    word = word or "none"  # no leg recorded anything: no verdict
    write(os.path.join(directory, "order-verdict.json"), f'{{"verdict":"{word}","legs":{{{",".join(legs)}}}}}\n')
    return word
