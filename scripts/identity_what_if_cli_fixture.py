"""The fixture scripts/identity-what-if's black-box tests share (identity_what_if_cli_test.py,
identity_what_if_cli_mirror_test.py): a scratch repository holding a copy of the tool and the scripts it runs, its
commits (good, inert, a take moved, after it, a resource the lookups read, a pure move, one more), the kept-side cache a
whole resolve fills, and stub binaries -- the mirror's freshness check, sbt printing a classpath, java standing in for
WhatIfResolve, gh's release upload -- each appending what it ran to <work>/calls in order. No JVM, no mirror. Ported
from identity-what-if-test.sh; the same cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import hashlib
import os
import shutil
import subprocess
import sys
from typing import Dict, List, Mapping, Optional, Tuple

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from processes import machine  # noqa: E402
from harness import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/identity_what_if_cli.py", "scripts/identity-what-if")
ROOT = cli_harness.ROOT
JUDGEMENTS = os.path.join(ROOT, "test/resources/fixtures/identity-judgements.tsv")
# the files the tool keys its kept sides by (its TOOL_FILES), copied in as the checkout holds them
TOOL_FILES = (
    "worker/src/test/scala/services/identity/OlderRefs.scala", "worker/src/test/scala/services/identity/WhatIfResolve.scala",
    "worker/src/test/scala/services/identity/WhatIfAnswers.scala", "worker/src/test/scala/scripts/StageAnswers.scala",
    "worker/src/fixtures/scala/tools/MemoLookups.scala", "worker/src/fixtures/scala/tools/OlderRefTypes.scala",
    "worker/src/fixtures/scala/services/identity/vocabulary/OlderRefsPackage.scala",
    "worker/src/fixtures/scala/services/identity/measures/OlderRefsPackage.scala",
    "worker/src/fixtures/scala/tools/KeptJvm.scala", "worker/src/test/scala/services/identity/MirrorWorld.scala",
    "worker/src/fixtures/scala/services/identityadapters/OlderRefsPackage.scala", "worker/src/fixtures/scala/services/tmdb/OlderRefsPackage.scala", "worker/src/fixtures/scala/services/agreementasks/OlderRefsPackage.scala", "worker/src/fixtures/scala/services/identitystaging/OlderRefsPackage.scala", "worker/src/fixtures/scala/services/lookupfill/OlderRefsPackage.scala", "worker/src/fixtures/scala/services/identityvenuepages/OlderRefsPackage.scala", "worker/src/fixtures/scala/services/identityprojection/OlderRefsPackage.scala", "worker/src/fixtures/scala/services/identitymodel/OlderRefsPackage.scala")
KEY = "Pthe venue\x1ethe film"
# the mirror generation the tool keys its sides by: the seed generation and the tail position's digest
TAIL_DIGEST = hashlib.sha1(b"T0").hexdigest()[:12]
GENERATION = "100.1-tail" + TAIL_DIGEST

# THE MIRROR'S GENERATION, as freshness --generation reports it: each call takes the next line of $work/generations
# (the seed generation) and of $work/tails (the tail position) -- the last one stays --, so a case can seed or tail a
# write mid-run; `100.1` (one finished seed) and `T0` by default -- and of $work/behind, the seconds the tail's last
# change lies behind (5 by default). Each call also notes the fences in force then.
FRESHNESS = r'''#!/usr/bin/env bash
next() { local v; v=$(head -1 "$1" 2>/dev/null); [ "$(wc -l 2>/dev/null < "$1" || echo 0)" -le 1 ] || { tail -n +2 "$1" > "$1.next"; mv "$1.next" "$1"; }; echo "${v:-$2}"; }
case " $* " in
  *" --tail "*) gen="" tail=$(head -1 "$WORK/tails" 2>/dev/null); tail=${tail:-T0}; behind=$(next "$WORK/behind" 5) ;;
  *) gen=$(next "$WORK/generations" 100.1) tail=$(next "$WORK/tails" T0) behind=$(head -1 "$WORK/behind" 2>/dev/null); behind=${behind:-5} ;;
esac
echo "freshness $*" >> "$WORK/freshness-calls"
cat "$WORK"/fence/kinowo_uk/* >> "$WORK/fences-seen" 2>/dev/null
cat "$WORK"/fence/kinowo_de/* >> "$WORK/fences-seen-de" 2>/dev/null
case " $* " in *" --tail "*) echo "TAIL $1 $tail $(( ($(date +%s) - behind) * 1000 ))" ;; *" --generation "*) echo "GENERATION $1 $gen"; echo "TAIL $1 $tail $(( ($(date +%s) - behind) * 1000 ))" ;; esac
case " $* " in *" --tail "*) exit "$(cat "$WORK/tail-exit" 2>/dev/null || echo 0)" ;; esac
exit 0
'''
# sbt prints a classpath; a compile of the tree $STUB_SLOW_SBT names outlasts the base's resolve
SBT = r'''#!/usr/bin/env bash
echo $$ >> "$STUB/sbt-pids"
echo "sbt-start $(basename "$PWD")" >> "$STUB/calls"
if [ "$(basename "$PWD")" = "${STUB_SLOW_SBT:-}" ]; then sleep 15; else sleep 1; fi
echo "$PWD/worker/target/scala-3.x/test-classes:/lib/a.jar"
echo "sbt-end $(basename "$PWD")" >> "$STUB/calls"
'''
# java is WhatIfResolve: a whole side, or the head's re-resolve of the selection (every listing taken alike). A whole side
# of the tree $STUB_AWAIT_COMPILE names ends only once a compile has started beside it (bounded), so "the head compiles
# beside the base" is an order, never a race of sleeps.
JAVA = r'''#!/usr/bin/env bash
echo "$*" >> "$STUB/calls.args"
case " $* " in *" --serve "*) echo "java-serve $(basename "$PWD")" >> "$STUB/calls"; exit 1 ;; esac
out="" select=""
while [ "$#" -gt 0 ]; do case "$1" in --out) out=$2 ;; --select) select=$2 ;; esac; shift; done
echo "java-start $(basename "$PWD")" >> "$STUB/calls"
if [ -z "$select" ] && [ -n "${STUB_AWAIT_COMPILE:-}" ]; then
  for _ in $(seq 1 100); do grep -q "sbt-start $STUB_AWAIT_COMPILE" "$STUB/calls" && break; sleep 0.1; done
else
  sleep 1
fi
key=$'Pthe venue\x1ethe film'
mkdir -p "$out"
printf 'key\tvenue\trawTitle\tfamily\ttake\trules\n' > "$out/takes.tsv"
take=tmdb:5; [ -z "$select" ] || take=${STUB_HEAD_TAKE:-tmdb:5}
if [ -z "$select" ] || [ -s "$select" ]; then printf '%s\tThe Venue\tThe Film\t%s\t%s\t\n' "$key" "$key" "$take" >> "$out/takes.tsv"; fi
printf 'key\tfinal\tbasis\n%s\ttmdb:5\tAgreed\n' "$key" > "$out/final.tsv"
if [ -z "$select" ]; then
  printf 'family\tblockKeys\n%s\t\n' "$key" > "$out/families.tsv"
  printf 'key\tprod\tbasis\n%s\ttmdb:5\t-\n' "$key" > "$out/prod.tsv"
else
  printf 'selected\tre-resolved\tgroups\trounds\n1\t1\t1\t1\n' > "$out/region.tsv"
fi
echo "${STUB_ANSWERS:-}" > "$out/answers.txt"
echo "java-end $(basename "$PWD")" >> "$STUB/calls"
'''
# gh's release, stood in for: an upload is a copy into $STUB/release (the verdict the stamp carry trusts)
GH = r'''#!/usr/bin/env bash
[ "$1 $2" = "release upload" ] || { echo "stub gh: $*" >&2; exit 1; }
mkdir -p "$STUB/release" && cp "$4" "$STUB/release/"
'''


class WhatIfCase(cli_harness.ToolCase):
    """A scratch repository with the tool's copy and commits c0..c6 (`self.c`), the stubs, and `what_if`."""

    def setUp(self) -> None:
        super().setUp()
        self._copy_scripts()
        self._stubs()
        self.repo_dir = self.repo("repo")
        self.c: Dict[str, str] = {}
        for name, message in (("c0", "good"), ("c1", "inert"), ("c2", "moves the take"), ("c3", "after it")):
            self.c[name] = self.commit(message)
        self.write("repo/common-row.txt", "row\n")
        self.write("repo/common/src/main/resources/rows.tsv", "row\n")
        self.c["c4"] = self.commit_all("a resource the lookups read")
        os.makedirs(self.path("repo/moved"))
        self.git(self.repo_dir, "mv", "common-row.txt", "moved/common-row.txt")
        self.c["c5"] = self.commit_all("a pure move")
        self.c["c6"] = self.commit("a head laid out nowhere")
        self.tool = os.path.join(self.repo_dir, os.path.relpath(TOOL, ROOT))
        self.hash = self._tools_hash()

    def _copy_scripts(self) -> None:
        self.copy_scripts("repo")
        scripts = os.path.join(ROOT, "scripts")
        for name in ("identity-what-if", "pure-move-check", "identity-what-if-roots.tsv"):
            shutil.copy2(os.path.join(scripts, name), self.path("repo/scripts"))
        for f in TOOL_FILES:
            os.makedirs(os.path.dirname(self.path("repo", f)), exist_ok=True)
            shutil.copy2(os.path.join(ROOT, f), self.path("repo", f))

    def _stubs(self) -> None:
        for name, text in (("repo/scripts/local-mirror/freshness", FRESHNESS), ("bin/sbt", SBT), ("bin/java", JAVA),
                           ("bin/gh", GH)):
            os.chmod(self.write(name, text), 0o755)

    def _tools_hash(self) -> str:
        listing = "".join(f + "\n" + self.read("repo/" + f) for f in TOOL_FILES)
        return hashlib.sha1(listing.encode()).hexdigest()[:12]

    def commit(self, message: str) -> str:
        self.write("repo/change.txt", message + "\n")
        return self.commit_all(message)

    def commit_all(self, message: str) -> str:
        return self.commit_tree(self.repo_dir, message)

    def tool_env(self, extra: Optional[Mapping[str, Optional[str]]] = None) -> Dict[str, str]:
        env: Dict[str, Optional[str]] = {
            "WORK": self.work, "STUB": self.work, "PATH": self.path("bin") + os.pathsep + machine.environment().get("PATH", ""),
            "KINOWO_GH": self.path("bin/gh"), "KINOWO_IDENTITY_CACHE": self.path("cache"),
            "KINOWO_MIRROR_FENCE_DIR": self.path("fence"), "TMDB_API_KEY": "stub"}
        env.update(extra or {})
        return self.env(env)

    def start(self, *args: str, env: Optional[Mapping[str, Optional[str]]] = None, err: str = "err",
              out: str = "out") -> "subprocess.Popen[str]":
        """The tool in the background, its stdout and stderr into <work>/<out> and <work>/<err>."""
        return subprocess.Popen([self.tool, *args], env=self.tool_env(env), cwd=self.repo_dir, text=True,
                                stdin=subprocess.DEVNULL, stdout=open(self.path(out), "w"),
                                stderr=open(self.path(err), "w"), start_new_session=True)

    def run_in_repo(self, *args: str, env: Optional[Mapping[str, Optional[str]]] = None) -> cli_harness.Done:
        for name in ("calls", "calls.args"):
            if os.path.exists(self.path(name)):
                os.remove(self.path(name))
        done = cli_harness.run_bounded([self.tool, *args], env=self.tool_env(env), cwd=self.repo_dir, stdin=None)
        self.write("err", done.stderr)
        return done

    def what_if(self, base: str, head: str, snapshot: str, *args: str,
                env: Optional[Mapping[str, Optional[str]]] = None, countries: Tuple[str, ...] = ("uk",)) -> cli_harness.Done:
        return self.run_in_repo("--base", self.c[base], "--head", self.c[head], "--at", snapshot, "--judgements",
                                JUDGEMENTS, *args, *countries, env=env)

    def bisect(self, good: str, bad: str, snapshot: str, *args: str) -> cli_harness.Done:
        return self.run_in_repo("--base", self.c["c0"], "--bisect", self.c[good], self.c[bad], "--listing",
                                "The Venue|The Film", "--at", snapshot, *args, "uk")

    def calls(self) -> str:
        """What the stubs ran, in order, `|`-joined; the base c0 and the heads c1 / c4 named so."""
        if not os.path.exists(self.path("calls")):
            return ""
        text = self.read("calls")
        for name, label in (("c0", "base"), ("c1", "head"), ("c4", "head")):
            text = text.replace(self.c[name], label)
        return "|".join(text.splitlines())

    def side_dir(self, sha: str, snapshot: str) -> str:
        return self.path("cache/what-if/uk", f"{sha}-{self.hash}-live-mirror{GENERATION}", snapshot)

    def side(self, commit: str, snapshot: str, model: str, final: str) -> None:
        """A kept whole side of that commit: every listing taken `model` by the model and `final` in the end."""
        d = self.side_dir(self.c[commit], snapshot)
        os.makedirs(d, exist_ok=True)
        with open(os.path.join(d, "takes.tsv"), "w") as f:
            f.write(f"key\tvenue\trawTitle\tfamily\ttake\trules\n{KEY}\tThe Venue\tThe Film\t{KEY}\t{model}\t\n")
        with open(os.path.join(d, "final.tsv"), "w") as f:
            f.write(f"key\tfinal\tbasis\n{KEY}\t{final}\tAgreed\n")
        with open(os.path.join(d, "prod.tsv"), "w") as f:
            f.write(f"key\tprod\tbasis\n{KEY}\t{final}\t-\n")

    def verdict_lines(self, head: str, base: str) -> List[str]:
        """The verdict record's lines of (head, base) (identity-tools.sh's what_if_verdicts_file)."""
        f = self.path("cache/what-if-verdicts", f"{self.c[head]}-{self.c[base]}.tsv")
        return open(f).read().splitlines() if os.path.exists(f) else []

    def assert_harness_error(self, message: str, done: cli_harness.Done) -> None:
        """Exit 2, `message` (a regex) on stderr, no CLEAN / BLOCKING verdict printed."""
        self.assertEqual(2, done.code, done.output)
        self.assertRegex(done.stderr, message)
        self.assertNotRegex(done.stdout, "CLEAN|BLOCKING|moved")
