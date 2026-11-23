"""THE MUTATIONS scripts/identity_impact_check.py proves the selection with: each a change, made in the checkout's
working tree, that fails some identity test -- a title rule, a judgement, an expected take, a slice, a seed, a title
word, the resolver, the capture reader, what the identity specs read by class alone, a spec reached only through a
class of its own file.

  MUTATIONS          [Mutation(name, what, apply)], `apply(tree)` raising RuntimeError when it changes nothing
  chosen(only)       those `only` names (none: all), in order"""
from __future__ import annotations

import glob
import os
import re
from typing import Callable, List, NamedTuple, Set, Tuple

Cells = List[str]


class Mutation(NamedTuple):
    name: str
    what: str
    apply: Callable[[str], None]


def edit(tree: str, path: str, fn: Callable[[str], str]) -> None:
    p = os.path.join(tree, path)
    with open(p, encoding="utf-8") as fh:
        text = fh.read()
    new = fn(text)
    if new == text:
        raise RuntimeError(f"the mutation changed nothing in {path}")
    with open(p, "w", encoding="utf-8") as fh:
        fh.write(new)


def tsv_row(text: str, pred: Callable[[Cells], bool], change: Callable[[Cells], Cells]) -> str:
    lines = text.split("\n")
    for i, line in enumerate(lines):
        if line and not line.startswith("#") and pred(line.split("\t")):
            lines[i] = "\t".join(change(line.split("\t")))
            return "\n".join(lines)
    raise RuntimeError("no row to mutate")


def replaced(old: str, new: str) -> Callable[[str], str]:
    return lambda text: text.replace(old, new)


def appended(row: str) -> Callable[[str], str]:
    return lambda text: text.rstrip("\n") + "\n" + row + "\n"


def rule_pattern(rule: str, old: str, new: str) -> Callable[[str], None]:
    """A title-rules.tsv row's pattern (its 8th column) with `old` made `new`."""
    def apply(tree: str) -> None:
        edit(tree, "common/src/main/resources/title-rules.tsv", lambda t: tsv_row(
            t, lambda c: c[0] == rule, lambda c: c[:7] + [c[7].replace(old, new)] + c[8:]))
    return apply


def in_file(path: str, fn: Callable[[str], str]) -> Callable[[str], None]:
    return lambda tree: edit(tree, path, fn)


def judgement_pl(tree: str) -> None:
    """A Polish take the ratchet holds right, judged wrong."""
    expected = os.path.join(tree, "test/resources/fixtures/identity-unmatched/expected-matches.tsv")
    with open(expected, encoding="utf-8") as fh:
        takes = [line.split("\t") for line in fh.read().split("\n")[1:] if line.startswith("pl\t")]
    keys: Set[Tuple[str, str, str, str]] = {(t[0], t[1], t[2], t[3]) for t in takes} | \
        {(t[0], "*", t[2], t[3]) for t in takes}
    edit(tree, "test/resources/fixtures/identity-judgements.tsv", lambda t: tsv_row(
        t, lambda c: len(c) > 5 and (c[0], c[1], c[2], c[3]) in keys and c[4] == "right" and c[5] == "open",
        lambda c: c[:4] + ["wrong"] + c[5:]))


def slice_uk(tree: str) -> None:
    d = sorted(glob.glob(os.path.join(tree, "test/resources/fixtures/identity-slices/uk-act-one-*")))[0]
    path = os.path.relpath(os.path.join(d, "expected.tsv"), tree)
    edit(tree, path, lambda t: re.sub(r"\ttmdb:1702778\t", "\ttmdb:1\t", t, count=1))


MUTATIONS = [
    Mutation("rules-pl-venue", "a Polish venue's title rule (Kino Muza) no longer strips its programme suffix",
             rule_pattern("muza-najlepsze-z-najgorszych", "najgorszych", "najgorszy")),
    Mutation("rules-de-row", "a German-scoped title rule's pattern changed",
             rule_pattern("xtra-special-bracket-suffix", "Special", "Spezial")),
    Mutation("judgement-pl", "a Polish take the ratchet holds right judged wrong", judgement_pl),
    Mutation("expected-uk", "a British take expected that no capture makes",
             in_file("test/resources/fixtures/identity-unmatched/expected-matches.tsv",
                     appended("uk\tNo Such Cinema\tNo Such Billing\ttmdb:1\tNo Such Film (1900)"))),
    Mutation("slice-uk", "a British slice expecting another film", slice_uk),
    Mutation("seed-us", "an American hard-cluster seed no corpus holds",
             in_file("test/resources/fixtures/corpus/hard-clusters-seeds.tsv",
                     appended("us\tNo Such Film Billed Anywhere\tfinding"))),
    Mutation("words-de", "a German event word that every country's titles bill",
             in_file("common/src/main/resources/title-words.tsv", lambda t: re.sub(
                 r"(?m)^(anniversary\ten\tanniversary)$", "\\1\nanniversary\tde\tfilm", t, count=1))),
    Mutation("agreement", "AgreementStage corrects no take an apply",
             in_file("identity/src/main/scala/services/identity/agreement/AgreementStage.scala",
                     replaced("val CorrectionsPerApply = 200", "val CorrectionsPerApply = 0"))),
    Mutation("capture-reader", "the capture reader (worker fixtures) names no capture",
             in_file("worker/src/fixtures/scala/tools/UnmatchedClusters.scala", replaced(
                 'def fixturePath(country: Country): Path = Directory.resolve(s"${country.code}.json.gz")',
                 'def fixturePath(country: Country): Path = Directory.resolve(s"${country.code}.json.gz.missing")'))),
    # what the identity specs read by class alone (is_identity_reach_path): a common model, a generic testkit helper
    Mutation("model-year", "a common model (EmbeddedYear) reads every bracketed year one late",
             in_file("common/src/main/scala/models/EmbeddedYear.scala",
                     replaced("case Seq(single) => Some(single)", "case Seq(single) => Some(single + 1)"))),
    Mutation("mutable-clock", "the testkit's MutableClock advances twice as far as asked",
             in_file("testkit/src/main/scala/tools/MutableClock.scala",
                     replaced("def advance(d: Duration): Unit          = now = now.plus(d)",
                              "def advance(d: Duration): Unit          = now = now.plus(d).plus(d)"))),
    # a spec reached only through a class of its own file (project/SameFileEdges.scala): 604134d1a's
    # TitleRelation.sameOriginal failed PopulationMonotonicityPart2Spec, a concrete subclass declared in the abstract
    # PopulationMonotonicitySpec's own file, which only that file's class files connect to the change
    Mutation("same-file-subclass",
             "TitleRelation.sameOriginal as 604134d1a had it: a concrete spec of an abstract one's file fails",
             in_file("identity/src/main/scala/services/identity/TitleRelation.scala",
                     replaced("(a.originals ++ b.originals).exists(o => o == a.whole || o == b.whole) ||",
                              "a.originals(b.whole) || b.originals(a.whole) ||"))),
]


def chosen(only: Set[str]) -> List[Mutation]:
    return [m for m in MUTATIONS if not only or m.name in only]
