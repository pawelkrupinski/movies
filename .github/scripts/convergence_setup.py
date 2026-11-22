#!/usr/bin/env python3
"""The logic of `.github/actions/convergence-setup`'s fixture steps, out of the action's inline `run:` blocks so it is
tested (convergence_setup_test.py, against a fake `gh`) and runnable locally. Each subcommand is one step; the action
passes its inputs as arguments and the step's exports go to $GITHUB_ENV / $GITHUB_OUTPUT exactly as the inline block
wrote them. convergence-setup.sh, beside it, is the name its callers run.

  convergence-setup.sh pinned <code>
      The pair pinned for <code> -- the first line of `hermetic-<code>.txt` in the release $FIXTURE_RELEASE_TAG:
      `<corpus run> <recorded at> <tree asset> [fills...]`. Prints nothing (exit 0) when no pair is pinned, exits 1 on
      a FAILED read (gh-optional.sh), 2 on a line without a tree. The ONE reader of the pin: this action and
      the laptop's restore-country-corpus.sh both read it here.

  convergence-setup.sh pair <code> <mode> [given pair] [corpus run]
      "Resolve the recorded pair to replay". A RECORDING leg takes the corpus it is given (its own recorder run) or the
      newest, and the working tree. A HERMETIC leg replays the given pair, else the named corpus run's bare pair (its
      tree, no fills), else the pinned one with every fill published over its tree -- and with none, fails: no pin is
      never a quiet fall-back to live filling.

  convergence-setup.sh pins <codes, comma-separated>
      THE RUN'S PAIRS, pinned once (identity-model-convergence.yml's preflight): `{"<code>": "<pair>", ...}`, each
      country's pinned pair with the fills published over its tree as `pair` resolves it now. A leg's order-pass rows
      replay exactly that (`hermetic-pair`), not what the release holds when each row starts: a fill another row
      publishes mid-run otherwise reaches one pass and not the next (2026-11-03, UK p12). A country `pair` cannot
      resolve is left out: its rows resolve, and fail, alone.

  convergence-setup.sh corpus <code>
      "Restore the recorded scrape corpus" into ./scrape-archive: this job's own recording (no download), the pinned
      run's artifact (gone or unreadable = exit 1, each said apart), else the newest of the last 10 green `Record scrape
      fixtures` runs that still has one.

  convergence-setup.sh unpack <code> <mode>
      "Unpack whichever fixtures are present": waits for the background tree restore, moves the staged tree in, unpacks
      the corpus, and counts both -- the remembered-answer cache apart. (convergence_setup_unpack.py)"""
from __future__ import annotations

import os
import sys
import tempfile
from typing import List

HERE = os.path.dirname(os.path.abspath(__file__))
# (its own directory is sys.path's first, run as a script; convergence_release puts scripts/lib after it)
import convergence_release  # noqa: E402
import convergence_setup_unpack  # noqa: E402

TOOL = "convergence-setup.sh"
FILL = os.path.join(HERE, "convergence-fill.sh")


class Setup(convergence_release.Step):
    def require_release_tag(self) -> str:
        """The release the pin and its fills live on, from the calling workflow's env. Unset, every read would fail
        on it and `pair` would call that "no recorded pair pinned" (Identity family explorer #1, run 42182907464)."""
        tag = self.env.get("FIXTURE_RELEASE_TAG", "")
        if not tag:
            sys.stderr.write("::error::FIXTURE_RELEASE_TAG is not set -- the calling workflow's env must name the fixtures"
                             " release (FIXTURE_RELEASE_TAG: convergence-fixtures)\n")
            raise convergence_release.Exit(1)
        return tag

    def read_pin(self, code: str) -> "tuple[int, str]":
        tag = self.require_release_tag()
        marker = f"hermetic-{code}.txt"
        with tempfile.TemporaryDirectory() as directory:
            try:
                if not self.github.optional("release", "download", tag, "--pattern", marker, "--dir", directory,
                                            "--clobber"):
                    return 0, ""
            except convergence_release.ReadFailed:
                return 1, ""
            with open(os.path.join(directory, marker)) as f:
                line = f.readline().rstrip("\n")
        if len(line.split()) < 3:
            sys.stderr.write(f"{marker} is malformed: '{line}'\n")
            return 2, ""
        return 0, line

    def pinned(self, code: str) -> int:
        status, line = self.read_pin(code)
        if line:
            self.say(line)
        return status

    def recording_pair(self, code: str, corpus_run: str) -> int:
        if corpus_run:
            self.actions.export("KINOWO_CONVERGENCE_PIN_CORPUS_RUN", corpus_run)
        # A glob: the working tree is `.tar.zst` since it moved off gzip, `.tar.gz` until a country's first zstd
        # publish replaces (and deletes) it.
        self.actions.export("KINOWO_CONVERGENCE_TREE_ASSET", f"enrichment-{code}.tar.*")
        self.say(f"recording leg: corpus {corpus_run or 'newest'}, working tree enrichment-{code}.tar.*")
        return 0

    def hermetic_pair(self, code: str, given: str, corpus_run: str) -> "tuple[str, bool]":
        """The pair to replay and whether it is a NAMED recording's bare pair (its tree, no fills): identity-model-
        convergence's `recording` input, laid as identity-measure-local and identity-slice lay it."""
        if given:
            return given, False
        if corpus_run:
            recorded = self.github.created_at(corpus_run) or "unknown"
            return f"{corpus_run} {recorded} enrichment-{code}-{corpus_run}.tar.*", True
        # A failed read or a malformed pin is no pair, and fails just below; a missing release tag ends the step here.
        return self.read_pin(code)[1], False

    def pair(self, code: str, mode: str, given: str = "", corpus_run: str = "") -> int:
        if mode == "record":
            return self.recording_pair(code, corpus_run)
        pair, named = self.hermetic_pair(code, given, corpus_run)
        words = pair.split(None, 3)
        if len(words) < 3:
            self.say(f"::error::No recorded pair pinned for {code} (release asset hermetic-{code}.txt) — a hermetic leg"
                     " replays nothing else. Record one: gh workflow run \"Record scrape fixtures\" --ref main")
            raise convergence_release.Exit(1)
        corpus, recorded, tree = words[:3]
        fills = words[3] if len(words) > 3 else ""
        # The FILLS published over this tree (convergence-fill.sh), named here once: every one is laid over, or the
        # restore fails (restore-enrichment-tree.sh): the pair never names a fill the leg replayed without.
        if not given and not named:
            fills = self.github.captured([FILL, "fills", code, corpus]).stdout.rstrip("\n")
            pair = f"{corpus} {recorded} {tree}" + (f" {fills}" if fills else "")
        self.say(f"hermetic leg: corpus from recording run {corpus} ({recorded}), tree {tree}, fills: {fills or 'none'}")
        for name, value in (("PIN_CORPUS_RUN", corpus), ("TREE_ASSET", tree), ("FILL_ASSETS", fills)):
            self.actions.export(f"KINOWO_CONVERGENCE_{name}", value)
        if named:
            self.actions.export("KINOWO_CONVERGENCE_NAMED_RECORDING", corpus)
        self.actions.output("pair", pair)
        return 0

    def pins(self, codes: str) -> int:
        self.require_release_tag()
        pinned: List[str] = []
        with tempfile.TemporaryDirectory() as directory:
            for code in codes.replace(",", " ").split():
                output = os.path.join(directory, "output")
                open(output, "w").close()
                pair = self.pair_alone(code, output, os.path.join(directory, "env"))
                if pair:
                    pinned.append(f'"{code}":"{pair}"')
                else:
                    sys.stderr.write(f"::warning::could not pin {code}'s pair once for the run -- its rows resolve it each\n")
        self.say("{" + ",".join(pinned) + "}")
        return 0

    def pair_alone(self, code: str, output: str, env_file: str) -> str:
        """`pair <code> hermetic` with its exports kept apart and its lines on stderr; the pair it named, or ""."""
        alone = Setup(dict(self.env, GITHUB_OUTPUT=output, GITHUB_ENV=env_file), self.runner, sys.stderr, self.clock)
        try:
            alone.pair(code, "hermetic")
        except convergence_release.Exit:
            return ""
        with open(output) as f:
            return next((line[len("pair="):].rstrip("\n") for line in f if line.startswith("pair=")), "")

    def corpus(self, code: str) -> int:
        try:
            return convergence_setup_unpack.Corpus(self).restore(code)
        except convergence_release.ReadFailed:
            return 1

    def unpack(self, code: str, mode: str) -> int:
        return convergence_setup_unpack.unpack(self, code, mode)


USAGE = {"pinned": convergence_release.Usage(1, 1, "pinned <code>"),
         "pair": convergence_release.Usage(2, 4, "pair <code> <mode> [given pair] [corpus run]", more=True),
         "pins": convergence_release.Usage(1, 1, "pins <codes>"), "corpus": convergence_release.Usage(1, 1, "corpus <code>"),
         "unpack": convergence_release.Usage(2, 2, "unpack <code> <mode>")}


def main(argv: List[str]) -> int:
    return convergence_release.main(Setup, TOOL, USAGE, "<pinned|pair|pins|corpus|unpack> ...", argv)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
