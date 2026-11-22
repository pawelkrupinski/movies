#!/usr/bin/env python3
"""identity-drift-check against a stand-in sbt in a scratch git repository (a base commit and a head commit), black-box:
  - divergences beyond the allow-list fail (1) whatever the base did -- the hard invariant; allowed ones pass;
  - FLIPS ARE DIFFERENTIAL: only a flip the head shows on a seed and its base does not fails (1), naming the listing;
    one both show is a `pre-existing (backlog)` line and passes (0), one only the base shows is `removed:`;
  - a head whose identity code is its base's replays nothing twice and never fails on flips;
  - a newborn check's TEMPORARY flips-allow row passes its head-only flip;
  - a replay that wrote no report, no flips line, or no recording to replay is no verdict (2); so is one the memory
    guard ended, and one whose JVM crashed (its hs_err and replay files kept, its frame named);
  - the row's fixed seeds go to the head in one replay, to the base one replay a seed;
  - a red names its seed (verdict, seed file, a one-line reproducer with the red listing's drift slice);
  - a base that does not compile steps back to its parent (DRIFT_BASE_STEPS), saying so; one that never does is no verdict;
  - DRIFT_TWICE: two replays of the same seeds that differ fail; the explorer (DRIFT_EXPLORE) is never red;
  - THE SCENARIO GUARD: head and base that drew differently for a question both ask is a HARNESS ERROR (2), never flips;
    a question only the head asks is its own; a base with no scenario (from before the guard) is not compared;
  - THE HEAD'S MEMO (DRIFT_STORE): a head whose replay inputs a run already replayed reads that replay; any replay input
    moved replays, docs, web and CI wiring do not; DRIFT_TWICE, DRIFT_FRESH and the explorer always replay; a replay
    with no verdict is never stored; a full store is pruned, oldest first, before the upload.
The tool and the scripts it runs are copied into the scratch repository, as the replay's key reads its own tree. The
same cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import glob
import gzip
import json
import os
import re
import shutil
import sys
import unittest
from typing import Dict, List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/identity_drift_check.py", "scripts/ci/identity-drift-check.sh")
CI = os.path.join(cli_harness.ROOT, "scripts/ci")
COPIED = ("identity-drift-check.sh", "drift-replays-compare.sh", "drift_seeds.py", "drift_diff.py", "drift_explorer.py",
          "drift_scenario.py", "memory-guard.sh", "memory-guard-stop.sh", "gh-release.sh", "release-store-prune.sh")
STRANGER = "Riverside|The Stranger"
NEW = "Kino Muza|New Film"

# sbt as the replay leaves it: the report on stdout, the TSVs under --out, IdentityDrift's exit. The flips are
# FAKE_HEAD_FLIPS at the head and FAKE_BASE_FLIPS in the base's checkout (`venue|title;venue|title`, on every seed); the
# scenario one arrangement and the fill draws FAKE_HEAD_DRAWS / FAKE_BASE_DRAWS (`round:question=0|1,...`; none: no
# scenario, a base from before the guard).
SBT = r'''
import re, subprocess
joined = "\n".join(argv)
out_dir = re.findall(r"--out (\S+)", joined)[-1]
seed_list = re.findall(r"--seed ([0-9,]+)", joined)[-1]
side = "base" if "drift-base-checkout" in os.getcwd() else "head"
with open(env["FAKE_LOG"], "a") as f:
    f.write(f"{side} {seed_list}\n")
with open(out_dir + ".args", "w") as f:
    f.write("\n".join(argv) + "\n")
if env.get("FAKE_REPORT") == "none":
    out("[error] compilation failed"); exit(1)
if env.get("FAKE_HANG"):
    out("## pl pending [seed 1]: pending at projection: peak 92944"); sys.stdout.flush()
    subprocess.run(["sleep", "300"]); exit(0)
broken = env.get("FAKE_BROKEN_BASE", "")
if broken and side == "base":
    here = subprocess.run(["git", "rev-parse", "HEAD"], capture_output=True, text=True).stdout.strip()
    if broken in ("all", here):
        out("[error] (worker / Test / compileIncremental) Compilation failed"); exit(1)
if env.get("FAKE_JVM_CRASH") == side:
    with open("hs_err_pid4242.log", "w") as f:
        f.write("#\n# A fatal error has been detected by the Java Runtime Environment:\n#\n#  SIGSEGV (0xb) at "
                "pc=0x00007fb15fad6af0, pid=4242\n# Problematic frame:\n# V  [libjvm.so+0x6d6af0]  "
                "ciObject::constant_encoding()+0x0\n#\nCurrent CompileTask:\nC2:  91234 51234       4       "
                "dotty.tools.dotc.core.Types$Type::widen (66 bytes)\n")
    with open("replay_pid4242.log", "w") as f:
        f.write("replay data\n")
    out("sbt: line 36:  4242 Aborted                 (core dumped)"); exit(134)
os.makedirs(out_dir, exist_ok=True)
with open(os.path.join(out_dir, "drift-pl.tsv"), "w") as f:
    f.write("country\tseed\n")
flips = env.get("FAKE_BASE_FLIPS" if side == "base" else "FAKE_HEAD_FLIPS", "")
runs = os.path.join(out_dir, "..", "runs")
n = (int(open(runs).read()) if os.path.exists(runs) else 0) + 1
open(runs, "w").write(str(n))
seeds = seed_list.split(",")
with open(os.path.join(out_dir, "flips-pl.tsv"), "w") as f:
    f.write("country\tseed\tvenue\trawTitle\tfilms\treasons\ttakes\tcause\n")
    for s in seeds:
        for row in [r for r in flips.split(";") if r]:
            venue, title = row.split("|", 1)
            f.write(f"pl\t{s}\t{venue}\t{title}\ttmdb:1, tmdb:2\tback to a film it left\t"
                    "tmdb:1 [3 scrape] -> tmdb:2 [4 answer] -> tmdb:1 [9 scrape]\tanswer\n")
        if env.get("FAKE_VARY"):
            f.write(f"pl\t{s}\tRun\t{n}\ta\tb\tc\td\n")
draws = env.get("FAKE_BASE_DRAWS" if side == "base" else "FAKE_HEAD_DRAWS") or "1:q:shared=1"
if draws != "none":
    with open(os.path.join(out_dir, "scenario-pl.tsv"), "w") as f:
        f.write("seed\tkind\tkey\tvalue\n")
        for s in seeds:
            f.write(f"{s}\tarrangement\tinputs\taa11\n{s}\tarrangement\thalf-1\tbb22\n")
            for d in draws.split(","):
                f.write(f"{s}\tfill\t{d.rsplit('=', 1)[0]}\t{d.rsplit('=', 1)[1]}\n")
found = int(env.get("FAKE_FOUND", "0"))
out(f"## pl: 10804 listings, 1 seed(s), {found} divergence(s) (1 listings), {env.get('FAKE_ALLOWED', '0')} allow-listed, in 191s")
if found > 0:
    out("  1 listing(s)  deploy: incremental-only take (whole BelowThreshold, incremental Agreed)")
if env.get("FAKE_FLIPS") == "none":
    exit(0)
out("## pl flips: 9 flip(s) (1 listings), 0 allow-listed, 0 known")
out("  1 listing(s)  flip: back to a film it left")
out("      Head Row | Not The Verdict: tmdb:1 [1 boot] -> tmdb:2 [2 x] [seed 1]")
exit(0)
'''
# gh keeping the store in a directory: `view --jq` answered through jq over it (createdAt: the files' age order), an
# upload past FAKE_STORE_CAP assets refused, as GitHub refuses one past 1000.
GH = r'''
import shutil, subprocess
store = env["FAKE_STORE"]
if argv[:1] != ["release"]:
    exit(1)
verb = argv[1]
def flag(name):
    return argv[argv.index(name) + 1] if name in argv else None
if verb == "view":
    jq = flag("--jq")
    if not jq:
        exit(0)
    names = sorted(os.listdir(store), reverse=True)
    names.sort(key=lambda n: os.path.getmtime(os.path.join(store, n)))
    assets = [{"name": n, "createdAt": "2026-01-01T" + str(i).zfill(6)} for i, n in enumerate(names)]
    done = subprocess.run(["jq", "-r", jq], input=json.dumps({"assets": assets}), capture_output=True, text=True)
    sys.stdout.write(done.stdout); exit(done.returncode)
if verb == "create":
    exit(0)
if verb == "upload":
    if len(os.listdir(store)) >= int(env.get("FAKE_STORE_CAP", "1000")):
        exit(1)
    shutil.copy(argv[3], store); exit(0)
if verb == "delete-asset":
    p = os.path.join(store, argv[3])
    if os.path.exists(p):
        os.remove(p)
    exit(0)
if verb == "download":
    shutil.copy(os.path.join(store, flag("-p")), flag("-O")); exit(0)
exit(1)
'''


class IdentityDriftCheckTest(cli_harness.ToolCase):
    def setUp(self) -> None:
        super().setUp()
        self.repo = self.repo_dir()
        self.fx = os.path.join(self.repo, "test/resources/fixtures")
        self.fakes.program("sbt", SBT)
        self.store = self.path("store")
        os.makedirs(self.store)
        os.makedirs(self.path("tmp"))
        self.check_tool = os.path.join(self.repo, "scripts/ci", os.path.basename(TOOL))

    def repo_dir(self) -> str:
        repo = self.path("repo")
        os.makedirs(os.path.join(repo, "scripts/ci"))
        os.makedirs(os.path.join(repo, "scripts/lib"))
        tool_files = [os.path.join(CI, n) for n in COPIED if os.path.exists(os.path.join(CI, n))]
        tool_files += [p for p in glob.glob(os.path.join(CI, "identity_drift_check*.py")) if not p.endswith("_test.py")]
        for path in tool_files:
            shutil.copy2(path, os.path.join(repo, "scripts/ci"))
        for path in glob.glob(os.path.join(cli_harness.ROOT, "scripts/lib/*.py")):
            if not path.endswith("_test.py"):
                shutil.copy2(path, os.path.join(repo, "scripts/lib"))
        self.write("repo/test/resources/fixtures/identity-flips-allow.tsv",
                   "country\tvenue\trawTitle\treason\npl\tKino Nowe\tAllowed Film\tTEMPORARY owner: x; remove when y; z\n")
        self.write("repo/test/resources/fixtures/identity-flip-backlog.tsv",
                   "kind\tcountry\tseed\tslice\tvenue\trawTitle\ttakes\tfirstSeen\towner\tnote\n"
                   "drift\tpl\t1\t\tRiverside\tThe Stranger\t\t2026-10-29\tthe red fixer\tx\n")
        self.write("repo/test/resources/fixtures/corpus/.keep", "")
        self.write("repo/identity/src/main/Rule.scala", "base\n")
        self.git(repo, "init", "-q", "-b", "main")
        self.commit_all(repo, "base")
        self.write("repo/README.md", "docs\n")
        self.commit_all(repo, "docs only")
        return repo

    def commit_all(self, repo: str, message: str) -> None:
        self.git(repo, "add", "-A")
        self.git(repo, "commit", "-qm", message)

    def change(self, relative: str, text: str, message: str) -> None:
        self.write(os.path.join("repo", relative), text)
        self.commit_all(self.repo, message)

    def rev(self, ref: str) -> str:
        return self.git(self.repo, "rev-parse", ref)

    def corpus(self) -> None:
        self.write("repo/test/resources/fixtures/corpus/cinema-scrapes-pl.json.gz", "")

    def drift(self, **env: str) -> int:
        """One run of the check (off main, as a landing or measure run): its exit; its output in self.said."""
        self.write("sbt-calls", "")
        shutil.rmtree(self.path("out"), ignore_errors=True)
        extra: Dict[str, Optional[str]] = {"GITHUB_REF": "refs/heads/drift-check-test", "FAKE_LOG": self.path("sbt-calls"),
                                           "RUNNER_TEMP": self.path("tmp"), "FAKE_STORE": self.store}
        extra.update(env)
        done = self.run_tool("pl", self.path("out"), env=extra, tool=self.check_tool, cwd=self.repo)
        self.said = done.stdout + done.stderr
        return done.code

    def calls(self) -> str:
        return " ".join(self.read("sbt-calls").split("\n")).strip()

    def out(self, name: str) -> str:
        return self.read(os.path.join("out", name))

    def count(self, pattern: str, text: Optional[str] = None) -> int:
        return len(re.findall(pattern, self.said if text is None else text, re.MULTILINE))

    def identity_change(self) -> None:
        self.change("identity/src/main/Rule.scala", "head\n", "an identity change")

    # ── the verdicts: divergences, no verdict ────────────────────────────────────────────────

    def test_divergences_and_runs_with_no_verdict(self) -> None:
        self.assertEqual(2, self.drift(), self.said)  # no recorded corpus to replay
        self.corpus()
        self.assertEqual(0, self.drift(), self.said)
        self.assertEqual(1, self.drift(FAKE_FOUND="3"))
        self.assertEqual("3 divergence(s) from a whole resolve and 0 flip(s) on the way the change adds over its base, "
                         "beyond the allow-lists; 0 pre-existing (backlog) flip(s) not failed -- seed(s) 1 2\n",
                         self.out("verdict.txt"))
        self.assertEqual(1, self.count("incremental-only take", self.out("report.txt")))
        self.assertEqual(0, self.drift(FAKE_FOUND="3", FAKE_ALLOWED="3"))
        self.assertEqual(2, self.drift(FAKE_REPORT="none"))
        self.assertEqual(2, self.drift(FAKE_FLIPS="none"))

    def test_a_replay_the_memory_guard_ends_is_no_verdict(self) -> None:
        self.corpus()
        self.write("proc/meminfo", "MemTotal: 16000000 kB\nMemAvailable: 500000 kB\nSwapTotal: 0 kB\nSwapFree: 0 kB\n")
        self.assertEqual(2, self.drift(FAKE_HANG="1", PROC_ROOT=self.path("proc"), MEMORY_GUARD_INTERVAL="0.1",
                                       MEMORY_GUARD_STRIKES="2"), self.said)
        self.assertEqual(1, self.count(
            r"^identity drift \(pl\): the replay was ended before the runner ran out of memory, so nothing was judged: "
            r"the machine had under 1024 MB available for 2 samples \(15137 MB in use of 15625, 488 MB available, 0 MB "
            r"swap used\) after 0 min -- last: ## pl pending \[seed 1\]", self.out("verdict.txt")))
        self.assertEqual(1, self.count(r"^identity drift \(pl\) memory guard: ENDING process"))

    def test_the_explorer_is_never_red_and_names_what_no_backlog_row_does(self) -> None:
        self.corpus()
        self.assertEqual(0, self.drift(DRIFT_EXPLORE="1", DRIFT_EXPLORE_REF="run:9/a",
                                       FAKE_HEAD_FLIPS=f"{STRANGER};Kino Nowy|New Flip"))
        self.assertEqual("head 1,2", self.calls())
        rows = [line.split("\t") for line in self.out("explore-pl.tsv").splitlines()[1:]]
        self.assertEqual([["Kino Nowy", "New Flip", "pl", "1", "run:9/a"]], [r[:4] + [r[5]] for r in rows])
        self.assertEqual(1, self.count(r"^      explored: Kino Nowy \| New Flip \[seed 1\]"))
        self.assertEqual(0, self.count(r"^      Kino Nowy \| New Flip"))

    def test_a_head_with_its_bases_identity_code_never_fails_on_flips(self) -> None:
        self.corpus()
        self.assertEqual(0, self.drift(FAKE_HEAD_FLIPS=STRANGER))
        self.assertEqual("head 1,2", self.calls())
        self.assertEqual(2, self.count(r"^      pre-existing \(backlog\): Riverside \| The Stranger \[seed [12]\] -- in the "
                                       r"base too, not failed; backlog: owner the red fixer, first seen 2026-10-29"))
        self.assertEqual(0, self.count(r"^      Riverside \| The Stranger"))
        self.assertEqual(0, self.count("Head Row"))
        self.assertEqual(1, self.drift(FAKE_FOUND="1", FAKE_HEAD_FLIPS=STRANGER))

    # ── an identity change: the base replayed on the same seeds ──────────────────────────────

    def test_flips_are_differential(self) -> None:
        self.corpus()
        self.identity_change()
        self.assertEqual(0, self.drift(FAKE_HEAD_FLIPS=STRANGER, FAKE_BASE_FLIPS=STRANGER))
        self.assertEqual("head 1,2 base 1 base 2", self.calls())
        self.assertEqual(1, self.count(r"pre-existing \(backlog\): Riverside \| The Stranger \[seed 1\]"))
        self.assertEqual("0 divergence(s) from a whole resolve and 0 flip(s) on the way the change adds over its base, "
                         "beyond the allow-lists; 2 pre-existing (backlog) flip(s) not failed -- seed(s) 1 2\n",
                         self.out("verdict.txt"))
        self.assertEqual(1, self.drift(FAKE_HEAD_FLIPS=f"{STRANGER};{NEW}", FAKE_BASE_FLIPS=STRANGER))
        self.assertEqual(2, self.count(r"^      Kino Muza \| New Film: tmdb:1 \[3 scrape\] -> tmdb:2 \[4 answer\] -> "
                                       r"tmdb:1 \[9 scrape\] \[seed [12]\]$"))
        self.assertEqual(2, self.count(r"pre-existing \(backlog\): Riverside \| The Stranger"))
        self.assertEqual(1, self.count(" and 2 flip\\(s\\) on the way the change adds over its base", self.out("verdict.txt")))
        self.assertEqual(["New Film"], [r.split("\t")[3] for r in self.out("new-1.tsv").splitlines()[1:]])
        self.assertEqual(0, self.drift(FAKE_BASE_FLIPS=STRANGER))
        self.assertEqual(1, self.count(r"^      removed: Riverside \| The Stranger \[seed 1\]"))
        self.assertEqual(0, self.drift(FAKE_HEAD_FLIPS="Kino Nowe|Allowed Film"))
        self.assertEqual(1, self.count(r"allow-listed \(identity-flips-allow.tsv\): Kino Nowe \| Allowed Film \[seed 1\]"))

    def test_an_explicit_base_and_one_that_does_not_compile(self) -> None:
        self.corpus()
        self.identity_change()
        grand = self.rev("HEAD~2")
        self.assertEqual(1, self.drift(DRIFT_BASE=grand, FAKE_HEAD_FLIPS=STRANGER))
        self.assertEqual("head 1,2 base 1 base 2", self.calls())
        self.assertEqual(2, self.count(re.escape(f"head vs base {grand[:12]} (resolved)")))
        broken, parent = self.rev("HEAD~1"), grand
        self.assertEqual(0, self.drift(DRIFT_BASE=broken, FAKE_BROKEN_BASE=broken, FAKE_HEAD_FLIPS=STRANGER,
                                       FAKE_BASE_FLIPS=STRANGER), self.said)
        self.assertEqual("head 1,2 base 1 base 1 base 2", self.calls())
        self.assertEqual(1, self.count(re.escape(f"the base {broken[:12]} does not compile; judged against its parent "
                                                 f"{parent[:12]} instead")))
        self.assertEqual(2, self.count(re.escape(f"head vs base {parent[:12]} (resolved)")))
        self.assertEqual(2, self.drift(DRIFT_BASE=broken, FAKE_BROKEN_BASE="all", DRIFT_BASE_STEPS="1",
                                       FAKE_HEAD_FLIPS=STRANGER))

    def test_a_base_whose_jvm_crashes_is_no_verdict_its_report_kept(self) -> None:
        self.corpus()
        self.identity_change()
        self.assertEqual(2, self.drift(FAKE_JVM_CRASH="base", FAKE_HEAD_FLIPS=STRANGER))
        for name in ("hs_err_pid4242.log", "replay_pid4242.log"):
            self.assertTrue(os.path.isfile(self.path("out/base/seed-1", name)), name)
        self.assertEqual(1, self.count(r"JVM crashed.*ciObject::constant_encoding"))
        self.assertEqual(1, self.count(r"compiling: C2: .*Types\$Type::widen"))

    # ── the scenario guard ───────────────────────────────────────────────────────────────────

    def test_the_scenario_guard(self) -> None:
        self.corpus()
        self.identity_change()
        self.assertEqual(0, self.drift(FAKE_HEAD_DRAWS="1:q:a=1,1:q:b=0", FAKE_BASE_DRAWS="1:q:a=1,1:q:b=0"))
        self.assertEqual(2, self.count(r"^scenario: the same on seed [12]"))
        self.assertEqual(0, self.drift(FAKE_HEAD_DRAWS="1:q:new=1,1:q:a=1", FAKE_BASE_DRAWS="1:q:a=1"))
        self.assertEqual(2, self.drift(FAKE_HEAD_DRAWS="1:q:new=1,1:q:a=0,1:q:b=1", FAKE_BASE_DRAWS="1:q:a=1,1:q:b=0",
                                       FAKE_HEAD_FLIPS=NEW))
        verdict = self.out("verdict.txt")
        self.assertEqual((1, 0), (self.count(r"^harness error: head and base replayed different scenarios on seed\(s\) 1 2",
                                             verdict), self.count("flip\\(s\\) on the way", verdict)))
        self.assertEqual(1, self.count(r"^::error title=identity drift harness \(pl, seed 1\)::harness error"))
        self.assertEqual(2, self.count("HARNESS: 2 of the 2 fill draw\\(s\\) both sides made differ"))
        self.assertEqual(0, self.count(r"Kino Muza \| New Film"))
        self.assertEqual(0, self.drift(FAKE_BASE_DRAWS="none"))
        self.assertEqual(2, self.count(r"^scenario: not compared on seed [12] -- the base wrote no scenario"))

    # ── the row's fixed seeds, a red naming its seed, the double run ─────────────────────────

    def test_the_rows_seeds_and_a_red_naming_its_seed(self) -> None:
        self.corpus()
        self.identity_change()
        slice_dir = os.path.join(self.fx, "drift-slices/pl/muza-new-film-1")
        os.makedirs(slice_dir)
        with gzip.open(os.path.join(slice_dir, "slice.json.gz"), "wt") as f:
            json.dump({"country": "pl", "core": ["N\0Kino Muza\0https://x\0New Film"], "context": []}, f)
        self.assertEqual(1, self.drift(DRIFT_SEED_LIST="2 47", FAKE_HEAD_FLIPS=NEW))
        self.assertEqual("head 2,47 base 2 base 47", self.calls())
        self.assertEqual(1, self.count(r" -- seed\(s\) 2 47$", self.out("verdict.txt")))
        self.assertEqual("2\n47\n", self.out("seed"))
        self.assertEqual("IdentityDrift pl --seed 2,47; slice: IdentityDrift pl --slice "
                         "test/resources/fixtures/drift-slices/pl/muza-new-film-1 --seed 2,47\n", self.out("reproduce.txt"))
        self.assertEqual(1, self.count(r"^identity drift \(pl\): red on seed\(s\) 2 47 -- reproduce: IdentityDrift pl "
                                       r"--seed 2,47"))
        self.assertEqual(0, self.drift(DRIFT_SEED="38"))
        self.assertEqual("head 38", self.calls().split(" base")[0])
        self.assertEqual(0, self.count("red on seed"))

    def test_the_daily_double_run(self) -> None:
        self.corpus()
        self.assertEqual(0, self.drift(DRIFT_TWICE="true"))
        self.assertEqual(1, self.count("deterministic -- a second replay"))
        self.assertEqual(1, self.drift(DRIFT_TWICE="true", FAKE_VARY="1", FAKE_BASE_FLIPS="Run|1;Run|2;Run|3;Run|4"))
        self.assertEqual(1, self.count("^not deterministic", self.out("verdict.txt")))

    # ── the head's memo ──────────────────────────────────────────────────────────────────────

    def stored(self, pattern: str) -> List[str]:
        return glob.glob(os.path.join(self.store, pattern))

    def test_the_heads_memo(self) -> None:
        self.corpus()
        self.identity_change()
        self.fakes.program("gh", GH)
        red = {"FAKE_HEAD_FLIPS": f"{STRANGER};{NEW}", "FAKE_BASE_FLIPS": STRANGER, "DRIFT_STORE": "s"}
        self.assertEqual(1, self.drift(**red))
        self.assertEqual("head 1,2 base 1 base 2", self.calls())
        self.assertEqual(1, len(self.stored("drift-head-pl-1_2-*.tar.gz")))
        first, first_diff = self.out("verdict.txt"), self.out("diff.txt").replace("(resolved)", "(stored)")
        self.assertEqual(1, self.drift(**red))
        self.assertEqual("", self.calls())
        self.assertEqual(1, self.count(r"^identity drift \(pl\): the head's replay of seed\(s\) 1 2 is stored under the "
                                       r"same inputs \(drift-head-pl-1_2-.*\): read, not replayed$"))
        self.assertEqual((first, first_diff), (self.out("verdict.txt"), self.out("diff.txt")))
        self.assertEqual(1, self.drift(FAKE_HEAD_FLIPS="", DRIFT_STORE="s"))
        for env, replayed in (({"DRIFT_SEED": "1"}, "head 1"), ({"DRIFT_TWICE": "true"}, "head 1,2 head 1,2"),
                              ({"DRIFT_FRESH": "true"}, "head 1,2"), ({"DRIFT_EXPLORE": "1"}, "head 1,2"),
                              ({"HEAP": "9g"}, "head 1,2")):
            self.drift(DRIFT_STORE="s", **env)
            self.assertEqual(replayed, self.calls(), env)

    def test_the_replays_inputs_key_the_memo(self) -> None:
        self.corpus()
        self.identity_change()
        self.fakes.program("gh", GH)
        self.change("worker/src/fixtures/Fill.scala", "harness\n", "a fixtures-only change")
        self.drift(DRIFT_STORE="s")
        self.assertEqual("head 1,2 base 1 base 2", self.calls())  # not the same identity code: the base replayed
        self.drift(FAKE_HEAD_FLIPS=NEW)
        self.assertGreater(self.count(r"^      Kino Muza \| New Film"), 0)
        for path in ("data/pl/rows.tsv", "newmodule/src/main/A.scala", "infra/jvm/worker.opts"):
            self.change(path, path + "\n", path)
            self.drift(DRIFT_STORE="s")
            self.assertIn("head 1,2", self.calls(), path)
        self.drift(DRIFT_STORE="s")
        for path in ("docs/x.md", "web/app/A.scala", ".github/workflows/x.yml", "scripts/hooks/x.sh", "README.md"):
            self.change(path, path + "\n", path)
            self.drift(DRIFT_STORE="s")
            self.assertEqual("", self.calls(), path)
        with open(os.path.join(self.fx, "identity-drift-allow.tsv"), "a") as f:
            f.write("more\n")
        self.commit_all(self.repo, "an allow-list row")
        self.drift(DRIFT_STORE="s")
        self.assertIn("head 1,2", self.calls())
        with open(self.check_tool, "a") as f:
            f.write("# the check changed\n")
        self.commit_all(self.repo, "the check")
        self.drift(DRIFT_STORE="s")
        self.assertIn("head 1,2", self.calls())
        self.change("test/x", "x\n", "x")
        heads = len(self.stored("drift-head-*"))
        self.assertEqual((2, heads), (self.drift(FAKE_FLIPS="none", DRIFT_STORE="s"), len(self.stored("drift-head-*"))))

    def test_a_full_store_is_pruned_oldest_first_before_the_upload(self) -> None:
        self.corpus()
        self.fakes.program("gh", GH)
        for i in range(1, 13):
            path = os.path.join(self.store, f"drift-green-us-1-old{i}.tsv.gz")
            open(path, "w").close()
            os.utime(path, (1767225600 + i, 1767225600 + i))
        self.change("test/y", "y\n", "y")
        full = {"FAKE_STORE_CAP": "12", "DRIFT_STORE_ASSETS": "10", "DRIFT_STORE": "s"}
        self.assertEqual(0, self.drift(**full), self.said)
        self.assertEqual(1, len(self.stored("drift-head-pl-*")))
        self.assertLessEqual(len(os.listdir(self.store)), 10)
        self.assertEqual((0, 1), (len(self.stored("drift-green-us-1-old1.tsv.gz")),
                                  len(self.stored("drift-green-us-1-old12.tsv.gz"))))
        self.drift(**full)
        self.assertEqual("", self.calls())


if __name__ == "__main__":
    unittest.main()
