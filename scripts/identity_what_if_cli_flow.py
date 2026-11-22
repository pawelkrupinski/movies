"""THE RUN (scripts/identity_what_if_cli.py): a pure move short cut, --recording handed to scripts/identity-measure-local,
TMDB's key from the vault (the run again under `secrets run`; exit 64 from it answers from the local store), the refs
resolved, then --bisect or every country's what-if (identity_what_if_cli_country.py) -- and, on every exit, the head's
compile waited for and the fences left lingering."""
from __future__ import annotations

import os
import sys
from typing import List, Tuple

from identity_what_if_cli_bisect import Bisect
from identity_what_if_cli_context import TOOL_FILES, Context, Exit, tools_hash
from identity_what_if_cli_country import Change, Country, HeadCompile, Starter
from identity_what_if_cli_mirror import Mirror
from identity_what_if_cli_resolve import KeptJvms, Resolver
from identity_what_if_cli_sides import SideKey, Sides
from identity_what_if_cli_verdict import Verdicts, pure_move
import tool_process  # (scripts/lib, on the path the context put it on)


class WhatIf:
    def __init__(self, ctx: Context, me: str, start: Starter) -> None:
        self.ctx, self.me = ctx, me
        self.mirror = Mirror(ctx)
        self.resolver = Resolver(ctx, KeptJvms(ctx), me)
        self.compile = HeadCompile(ctx, self.resolver, start)

    def clean_up(self) -> None:
        self.compile.done()
        self.mirror.linger()

    def main(self) -> int:
        o = self.ctx.o
        if pure_move(self.ctx):
            return 0
        if o.recording:
            self.recording()
        answers = self.answers()
        answer_args = ["--answers", answers]
        if answers != "offline":
            answer_args += ["--answers-dir", o.answers_dir or os.path.join(self.ctx.cache, "what-if-answers")]
        base = self.commit(o.base)
        head, head_label = self.head()
        out = o.out or os.path.join(self.ctx.repo, "target", "identity-what-if", head_label)
        os.makedirs(out, exist_ok=True)
        key = SideKey(tools_hash(self.ctx.repo, TOOL_FILES), "offline" if answers == "offline" else "live")
        sides = Sides(self.ctx, self.resolver, key, answer_args)
        if o.bisect_good:
            return Bisect(self.ctx, self.mirror, sides, key).run()
        verdicts = Verdicts(self.ctx, head, base, out)
        change = self.change(base, head, head_label, out, answers, answer_args)
        country = Country(self.ctx, change, sides, key, self.mirror, self.compile, verdicts)
        status = 0
        for cc in o.ccs:
            rc = country.run(cc)
            status = rc if rc != 0 else status
        return status

    def recording(self) -> None:
        """A CI recording, replayed by CI's own two tools (scripts/identity-measure-local): the measure's numbers."""
        o = self.ctx.o
        if o.head == ".":
            print("--recording measures commits: name --head REF (commit the change first)", file=sys.stderr)
            raise Exit(2)
        tool = os.path.join(self.ctx.repo, "scripts", "identity-measure-local")
        sys.stdout.flush()
        sys.stderr.flush()
        os.execve(tool, [tool, "--base", o.base, "--head", o.head, "--recording", o.recording, "--countries",
                         ",".join(o.ccs)], self.ctx.env)

    def answers(self) -> str:
        """live, offline or cached: TMDB's key from the vault for the live asks only, the whole run under it."""
        env, answers = self.ctx.env, self.ctx.o.answers
        if answers == "live" and not env.get("TMDB_API_KEY") and not env.get("KINOWO_WHAT_IF_NO_VAULT"):
            rc = self.ctx.runner.run(tool_process.Command(
                ["secrets", "run", "movies", "TMDB_API_KEY", "--", "env", "KINOWO_WHAT_IF_NO_VAULT=1", self.me,
                 *self.ctx.o.argv], env))
            if rc != 64:  # 64: the vault refused (locked, a Touch ID prompt timed out)
                raise Exit(rc)
            self.ctx.log("the vault did not give TMDB_API_KEY (secrets unlock movies, then run again): answering from "
                         "the local store only, nothing asked live")
            answers = "cached"
        if answers == "live" and not env.get("TMDB_API_KEY"):
            self.ctx.log("no TMDB_API_KEY: answering from the local store only")
            answers = "cached"
        return answers

    def commit(self, ref: str) -> str:
        sha = self.ctx.commit_of(ref)
        if sha is None:
            print(f"no commit {ref}", file=sys.stderr)
            raise Exit(2)
        return sha

    def head(self) -> Tuple[str, str]:
        """(head sha or `.`, its label)."""
        if self.ctx.o.head == ".":
            return ".", "worktree-" + self.ctx.git("rev-parse", "--short=12", "HEAD").stdout.strip()
        sha = self.commit(self.ctx.o.head)
        return sha, sha[:12]

    def step_failed(self, step: str, code: int) -> None:
        """A step before the resolve failing says which it was and its status (Order independence #90)."""
        self.ctx.harness_error(f"{step} failed (exit {code})")
        raise Exit(2)

    def trees(self, base: str, head: str, head_label: str) -> str:
        """Both refs laid out (once each); the head's tree."""
        self.ctx.log(f"base {base}, head {head_label}; laying out and compiling the refs (once each)")
        code, _ = self.resolver.tree_of(base)
        if code:
            self.step_failed(f"laying out the base {base[:12]}", code)
        code, head_tree = self.resolver.tree_of(head)
        if code:
            self.step_failed(f"laying out the head {head_label}", code)
        return head_tree

    def reaches(self, base: str, head: str) -> bool:
        """Whether the code alone says the change reaches a take (identity_what_if.py reaches)."""
        done = self.ctx.capture(self.ctx.what_if_py(["reaches", "--repo", self.ctx.repo, "--base", base, "--head",
                                                     head]))
        if done.code:
            self.step_failed("identity_what_if.py reaches (whether the head's change reaches a take)", done.code)
        return done.stdout.rstrip("\n") != "none"

    def change(self, base: str, head: str, head_label: str, out: str, answers: str, answer_args: List[str]) -> Change:
        o = self.ctx.o
        head_tree = self.trees(base, head, head_label)
        # HEAD = BASE (the same tree): a control, whose every moved take is the harness's own
        same_code = head != "." and self.ctx.git("diff", "--quiet", base, head, "--").code == 0
        reaches = self.reaches(base, head)
        # --all by default for a change reaching a take, the selection with --verify (which checks it) or one reaching none
        every = o.all == "1" or (o.all == "auto" and reaches and not o.verify)
        judgements = o.judgements or os.path.join(head_tree, "test/resources/fixtures/identity-judgements.tsv")
        return Change(base, head, head_label, head_tree, out, judgements, every, same_code,
                      reaches or every or o.verify, answers, answer_args)
