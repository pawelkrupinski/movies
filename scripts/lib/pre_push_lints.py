"""WHICH LINT SPECS THE PRE-PUSH HOOK'S SBT CHECK RUNS, read off THE LINT REGISTRY (scripts/hooks/lint-registry.tsv; its
header has the format): `--lint-plan` and `--per-file-lints`, the standing lints, the registry's named files (whose
change alone runs the sbt check), whether a change selects a GitOps reader, and the testOnly lines with every
unfetched GitOps reader left out. A row runs when a pushed path matches what it says its spec reads, or the push changes
its spec's own code (a test-side path its module's Test config compiles)."""
from __future__ import annotations

from dataclasses import dataclass
from fnmatch import fnmatchcase
from typing import Callable, Dict, List, Sequence, Set

PATTERN_CHARS = set("*?[")


@dataclass(frozen=True)
class Row:
    spec: str          # `<module>:<package.Spec>`
    reads: List[str]   # the row's words

    @property
    def module(self) -> str:
        return self.spec.split(":", 1)[0]

    @property
    def gitops(self) -> bool:
        return "@gitops" in self.reads


def rows(registry: str) -> List[Row]:
    out: List[Row] = []
    for line in registry.splitlines():
        cells = line.split("\t")
        if not cells[0] or cells[0].startswith("#"):
            continue
        out.append(Row(cells[0], (cells[1] if len(cells) > 1 else "").split()))
    return out


def standing(registry: str) -> List[str]:
    return [r.spec for r in rows(registry) if r.reads == ["*"]]


def named_files(registry: str) -> List[str]:
    """The files the rows name (a word with no pattern character), each once."""
    seen: List[str] = []
    for line in registry.splitlines():
        cells = line.split("\t")
        if line.startswith("#") or len(cells) < 2:
            continue
        for word in cells[1].split(" "):
            if word and not PATTERN_CHARS & set(word) and word != "-" and not word.startswith("@") and word not in seen:
                seen.append(word)
    return seen


def any_match(paths: Sequence[str], pattern: str) -> bool:
    return any(fnmatchcase(p, pattern) for p in paths)


def module_match(paths: Sequence[str], pattern: str) -> bool:
    """A leading `*/` is ONE directory, a module: the rest matched below it."""
    return any("/" in p and fnmatchcase(p.split("/", 1)[1], pattern[2:]) for p in paths)


def any_path(paths: Sequence[str]) -> int:
    return 1 if paths else 0


SPECIAL: Dict[str, Callable[[Sequence[str]], int]] = {"*": lambda _paths: 1, "-": lambda _paths: -1, "@repo": any_path}


def word_selects(word: str, paths: Sequence[str]) -> int:
    """1 when the word selects the row for these paths, -1 for `-` (never), 0 otherwise."""
    if word in SPECIAL:
        return SPECIAL[word](paths)
    if word.startswith("@"):
        return 0
    return 1 if (module_match(paths, word) if word.startswith("*/") else any_match(paths, word)) else 0


def selection(row: Row, paths: Sequence[str]) -> int:
    for word in row.reads:
        said = word_selects(word, paths)
        if said:
            return said
    return 0


@dataclass(frozen=True)
class Push:
    """What the lint plan reads of the push: its paths, the modules whose own code it changes, its switches."""
    paths: List[str]
    own: Set[str]
    gitops: bool        # PREPUSH_GITOPS=1: the manifests are in place
    lintspecs_skipped: bool


def per_file_selected(row: Row, push: Push) -> bool:
    """A `@per-file` spec the paths select by what it reads alone, never by its own code."""
    return selection(row, push.paths) == 1 and "@per-file" in row.reads and row.module not in push.own


def selected(row: Row, push: Push) -> bool:
    """By what it reads or by its own code; with lintspecs skipped, only the standing rows."""
    said = selection(row, push.paths)
    if said == 0 and row.module in push.own:
        said = 1
    return said == 1 and (row.reads == ["*"] or not push.lintspecs_skipped)


def lint_plan(registry: str, push: Push, per_file: bool = False) -> List[str]:
    keep = per_file_selected if per_file else selected
    return [row.spec for row in rows(registry) if (push.gitops or not row.gitops) and keep(row, push)]


def reads_gitops(registry: str, push: Push) -> bool:
    """Whether the paths select a row reading the GitOps manifests (`@gitops`)."""
    wanted = set(lint_plan(registry, Push(push.paths, push.own, True, push.lintspecs_skipped)))
    return any(row.gitops and row.spec in wanted for row in rows(registry))


def gitops_left_out(line: str, gitops_specs: Sequence[str]) -> str:
    """One testOnly line with `-<spec>` added for each of its module's GitOps readers its patterns select."""
    if "/testOnly " not in line:
        return line
    module = line.split("/", 1)[0]
    words = line.split("testOnly ", 1)[1].split()
    add = ""
    for spec in gitops_specs:
        name = spec.split(":", 1)[1]
        if spec.split(":", 1)[0] == module and selects_suite(words, name):
            add += f" -{name}"
    if " -- " in line:
        head, tail = line.split(" -- ", 1)
        return f"{head}{add} -- {tail}"
    return line + add


def selects_suite(words: Sequence[str], name: str) -> bool:
    """Whether a testOnly line's words (patterns, `-exclusions`, until `--`) select the suite."""
    selected = False
    for word in words:
        if word == "--":
            break
        if word.startswith("-"):
            if fnmatchcase(name, word[1:]):
                return False
        elif fnmatchcase(name, word):
            selected = True
    return selected


def without_unfetched_gitops(registry: str, lines: Sequence[str], fetched: bool) -> List[str]:
    specs = [] if fetched else [row.spec for row in rows(registry) if row.gitops]
    return [gitops_left_out(line, specs) for line in lines]
