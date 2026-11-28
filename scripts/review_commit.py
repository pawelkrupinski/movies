#!/usr/bin/env python3
"""Commit the review pages' answers as the user's judgements
Commits the review pages' answers as the user's judgements: the rows the dev review pages (the digest, the queue, the
matchable and recent pages) wrote into test/resources/fixtures/identity-judgements.tsv as each answer was given, and
the `.used-until` mark beside it -- nothing else. Lints the file first; prints what it commits.

  scripts/review-commit [-m "<subject>"]

Run it in the review worktree scripts/review-server serves from (~/projects/movies-review-labels, branch
review/labels) -- never the root checkout, which it refuses. It commits; land with `~/bin/agentctl land` from
there. An answer given while no page was writing the file (REVIEW_LABELS_FILE unset, or the server down):
`REVIEW_LABELS_FILE=$PWD/test/resources/fixtures/identity-judgements.tsv sbt "web/runMain services.review.ReviewLabelsCli export"`.
Tests: scripts/review_commit_test.py."""
from __future__ import annotations

import os
import re
import sys
from dataclasses import dataclass
from typing import List, Mapping, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from processes import machine  # noqa: E402
import tool_git  # noqa: E402
import tool_process  # noqa: E402

TOOL = "review-commit"
TSV = "test/resources/fixtures/identity-judgements.tsv"
USED = TSV + ".used-until"
ANSWER_ROW = re.compile(r"^[+-][a-z][a-z]\t")
CONFIRMED = re.compile(r"was: (right|wrong): .*agent[- ]judged", re.I)


@dataclass
class Options:
    subject: str = "Judgements: the user's review answers"
    help: bool = False


def read_options(argv: List[str]) -> Options:
    options, i = Options(), 0
    while i < len(argv):
        if argv[i] in ("-h", "--help"):
            options.help = True
            return options
        if argv[i] != "-m" or i + 1 >= len(argv):
            raise ValueError(f"unknown argument {argv[i]}")
        options.subject = argv[i + 1]
        i += 2
    return options


@dataclass(frozen=True)
class Counts:
    added: int
    removed: int
    confirmed: int


def count_rows(diff: str) -> Counts:
    lines = diff.splitlines()
    added = [line for line in lines if ANSWER_ROW.match(line) and line[0] == "+"]
    removed = [line for line in lines if ANSWER_ROW.match(line) and line[0] == "-"]
    return Counts(len(added), len(removed), sum(1 for line in added if CONFIRMED.search(line)))


def message(subject: str, counts: Counts) -> str:
    return (f"{subject}\n\n{counts.added} rows written or rewritten, {counts.removed} replaced or taken back "
            f"({counts.confirmed} of them agent judgements the user\nconfirmed or overruled), from the dev review "
            "pages' answers (review_local.review_answers).\n")


def other_staged(names: List[str]) -> List[str]:
    return [name for name in names if name not in (TSV, USED)]


def refuse_root(root: str) -> int:
    print(f"{TOOL}: {root} is the shared root checkout; the review answers belong in the review worktree.", file=sys.stderr)
    print("  Run scripts/review-server (it makes ~/projects/movies-review-labels on review/labels), answer there,\n"
          "  then run scripts/review-commit from that worktree and land with ~/bin/agentctl land.", file=sys.stderr)
    return 1


class Review:
    """The checkout the answers are committed in: git, and the judgements lint (the leaves)."""

    def __init__(self, git: tool_git.Git, runner: tool_process.Runner, env: Mapping[str, str], root: str) -> None:
        self.git, self.runner, self.env, self.root = git, runner, env, root

    def is_root_checkout(self) -> bool:
        return self.git.line("rev-parse", "--path-format=absolute", "--git-dir") == \
            self.git.line("rev-parse", "--path-format=absolute", "--git-common-dir")

    def unchanged(self) -> bool:
        return self.git.ok("diff", "--quiet", "HEAD", "--", TSV, USED)

    def staged(self) -> List[str]:
        return self.git.lines("diff", "--cached", "--name-only")

    def lint(self) -> int:
        return self.runner.run(tool_process.Command(["python3", "scripts/ci/identity_judgements_lint.py"], self.env,
                                                    cwd=self.root))

    def diff(self) -> str:
        return self.git.out("diff", "HEAD", "--", TSV) or ""

    def commit(self, text: str) -> bool:
        print(self.git.out("diff", "HEAD", "--stat", "--", TSV, USED) or "", end="")
        self.git.call("add", "--", TSV, USED)
        done = self.git.call("commit", "-q", "-F", "-", input=text)
        print(self.git.out("log", "--oneline", "-1") or "", end="")
        return done.code == 0


def run(options: Options, review: Review) -> int:
    root_refused = review.is_root_checkout() and review.env.get("REVIEW_COMMIT_IN_ROOT") != "1"
    if root_refused:
        return refuse_root(review.root)
    if review.unchanged():
        print(f"{TOOL}: no answers to commit ({TSV} is as committed)")
        return 0
    staged = other_staged(review.staged())
    if staged:
        print(f"{TOOL}: other files are staged; commit or unstage them first:\n" + "\n".join(staged), file=sys.stderr)
        return 1
    linted = review.lint()
    if linted != 0:
        return linted
    if not review.commit(message(options.subject, count_rows(review.diff()))):
        return 1
    print(f"{TOOL}: land it: ~/bin/agentctl land (from {review.root})")
    return 0


def main(argv: List[str], env: Mapping[str, str]) -> int:
    try:
        options = read_options(argv)
    except ValueError as error:
        print(f"{TOOL}: {error}", file=sys.stderr)
        return 2
    if options.help:
        print(__doc__ or "", end="")
        return 0
    runner = tool_git.shared_runner()
    top: Optional[str] = tool_git.Git(runner, env, ".").line("rev-parse", "--show-toplevel")
    if top is None:
        return 128
    return run(options, Review(tool_git.Git(runner, env, top), runner, env, top))


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], machine.environment()))
