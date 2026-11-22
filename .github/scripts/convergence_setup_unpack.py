"""convergence_setup.py's `corpus` (the recorded scrape corpus into ./scrape-archive) and `unpack` (the staged tree
moved in, the corpus unpacked, both counted) steps."""
from __future__ import annotations

import glob
import os
import shutil
import sys

sys.dont_write_bytecode = True  # `python3 -I` ignores PYTHONDONTWRITEBYTECODE: no __pycache__ in the tree
sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))), "scripts/lib"))
import convergence_release  # noqa: E402
import tool_process  # noqa: E402

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
ARCHIVE = "scrape-archive"


class Corpus:
    def __init__(self, setup: convergence_release.Step) -> None:
        self.setup = setup
        self.env = setup.env
        self.github = setup.github
        self.archive_tag = self.env.get("RECORDING_ARCHIVE_TAG") or "identity-recordings"

    def provenance(self, run: str, recorded: str, label: str) -> None:
        """Logged and exported, so the spec can say WHICH corpus it replayed (`CorpusProvenance`)."""
        self.setup.say(f"scrape corpus from {label} {run} ({recorded})")
        self.setup.actions.export("KINOWO_CONVERGENCE_CORPUS_RUN", run)
        self.setup.actions.export("KINOWO_CONVERGENCE_CORPUS_RECORDED_AT", recorded)
        self.setup.runner.run(tool_process.Command(["ls", "-l", f"{ARCHIVE}/"], self.env))

    def restore(self, code: str) -> int:
        os.makedirs(ARCHIVE, exist_ok=True)
        pinned = self.env.get("KINOWO_CONVERGENCE_PIN_CORPUS_RUN", "")
        # A recording leg's own run records the corpus itself, in a step after this action: only its provenance
        if pinned and pinned == self.env.get("GITHUB_RUN_ID", ""):
            self.setup.say(f"scrape corpus recorded by this job, in recorder run {pinned}")
            self.setup.actions.export("KINOWO_CONVERGENCE_CORPUS_RUN", pinned)
            self.setup.actions.export("KINOWO_CONVERGENCE_CORPUS_RECORDED_AT",
                                      self.setup.clock().strftime("%Y-%m-%dT%H:%M:%SZ"))
            return 0
        if pinned:
            return self.pinned(code, pinned)
        for run, recorded in self.github.green_recordings():
            try:
                present = self.github.optional("run", "download", run, "--name", f"scrape-fixtures-{code}",
                                               "--dir", ARCHIVE)
            except convergence_release.ReadFailed:
                present = False
            if present:
                self.provenance(run, recorded, "recording run")
                return 0
        self.setup.say(f"no recorded scrape corpus for {code} — the leg will fail and say so")
        return 0

    def pinned(self, code: str, run: str) -> int:
        # A failed read (gh-optional.sh has asked a throttled storage again for ~15 minutes already) is not an expired
        # pair: run 38763455103 told its reader to re-record over a 503 that cleared within the hour.
        try:
            present = self.github.optional("run", "download", run, "--name", f"scrape-fixtures-{code}", "--dir", ARCHIVE)
        except convergence_release.ReadFailed:
            self.setup.say(f"::error::could not download recording run {run}'s corpus (scrape-fixtures-{code}) — a failed"
                           " read (gh's error above), not an expired pair; re-run the failed job once GitHub answers")
            return 1
        try:
            present = present or self.from_archive(code, run)
        except convergence_release.ReadFailed:
            return 1
        if not present:
            # Not necessarily EXPIRED: a run this forge never recorded has no artifact.
            self.setup.say(f"::error::recording run {run}'s corpus is no longer downloadable — absent from this forge (a"
                           " run it never recorded, e.g. a github.com-era one, or its artifacts deleted); measure the"
                           " pinned pair or a recording this forge holds, or re-run Record scrape fixtures")
            return 1
        self.provenance(run, self.github.created_at(run) or "unknown", "PINNED recording run")
        return 0

    def from_archive(self, code: str, run: str) -> bool:
        """The ARCHIVE release keeps the landing recordings' corpora as scrapes-<code>-<run>.tar.*, for when their
        artifacts are gone; its "release not found" is GitHub's hiccup, asked again (run 43317254260)."""
        staged = os.path.join(ARCHIVE, "from-archive")
        try:
            present = self.github.optional("release", "download", self.archive_tag, "--pattern",
                                           f"scrapes-{code}-{run}.tar.*", "--dir", staged, "--clobber",
                                           release_exists=True)
        except convergence_release.ReadFailed:
            self.setup.say(f"::error::could not read release {self.archive_tag} for recording run {run}'s corpus (gh's"
                           " error above)")
            raise
        if present:
            # unpack reads scrapes-<code>.tar.gz and unpacks by the archive's magic, whichever compressor packed it
            os.replace(sorted(glob.glob(os.path.join(staged, f"scrapes-{code}-{run}.tar.*")))[0],
                       os.path.join(ARCHIVE, f"scrapes-{code}.tar.gz"))
            shutil.rmtree(staged, ignore_errors=True)
            self.setup.say(f"recording run {run}'s corpus from the archive release {self.archive_tag}")
        return present


def move_tree_in(setup: convergence_release.Step, stage: str) -> None:
    """Moved in by rename (same filesystem), entry by entry; a directory already present is merged into."""
    os.makedirs("test/resources/fixtures", exist_ok=True)
    entries = sorted(os.listdir(stage)) if os.path.isdir(stage) else []
    for name in entries:
        entry, target = os.path.join(stage, name), os.path.join("test/resources/fixtures", name)
        if os.path.lexists(target):
            setup.run_or_exit(["cp", "-a", f"{entry}/.", f"{target}/"])
        else:
            shutil.move(entry, target)


def named(root: str, suffix: str) -> int:
    """`find <root> -name '*<suffix>' | wc -l`: files and directories alike."""
    return sum(1 for _, dirs, files in os.walk(root) for n in dirs + files if n.endswith(suffix))


def unpack(setup: convergence_release.Step, code: str, mode: str) -> int:
    # The enrichment tree, fetched in the background since the top of the action. A replay leg's missing pin (exit 3)
    # ends the leg; a recording falls through to fetching live.
    tree = setup.runner.run(tool_process.Command([os.path.join(ROOT, "scripts/ci/in-background.sh"), "wait",
                                                  "tree-restore", "900"], setup.env))
    if tree != 0 and not (tree == 3 and mode == "record"):
        return tree
    move_tree_in(setup, os.path.join(setup.env.get("RUNNER_TEMP", ""), "tree-stage/test/resources/fixtures"))
    archive = f"{ARCHIVE}/scrapes-{code}.tar.gz"
    if os.path.isfile(archive):
        setup.run_or_exit([os.path.join(HERE, "unpack-fixture-archive.sh"), archive])
        size = setup.runner.capture(tool_process.Command(["du", "-h", archive], setup.env)).stdout.split("\t")[0]
        setup.say(f"restored {archive} ({size})")
    else:
        setup.say(f"no {archive} — this leg fetches live for it and the recorder captures it next")
    # A MISSING fixture directory is the normal fallback path, not an error: the leg fetches live.
    setup.say(f"fixture files available: "
              f"{convergence_release.regular_files('test/resources/fixtures/corpus', f'test/resources/fixtures/enrichment-{code}')}")
    # The REMEMBERED-ANSWER cache apart: a 404 returns nothing to record, so those verdicts live only in
    # `.enrichment-cache`, and a leg without it re-asks every one live (a 22-minute Poland leg against 12): the count
    # on both sides of the artifact hop is what tells "warm" from "silently cold".
    remembered = named(f"test/resources/fixtures/enrichment-{code}/.enrichment-cache", ".entry")
    if remembered > 0:
        setup.say(f"remembered enrichment answers restored: {remembered}")
    else:
        setup.say("NO remembered-answer cache in the artifact — this leg re-asks every")
        setup.say("unresolved film's rating slugs live, and publishes the cache it builds.")
    return 0
