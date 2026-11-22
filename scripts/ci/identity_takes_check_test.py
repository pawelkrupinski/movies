#!/usr/bin/env python3
"""identity-takes-check, black-box, against a fake `gh` (history, statuses, a release kept in a directory, the jobs API),
a fake `sbt` (the resolve, the final takes' ratchet and the corpus-shape specs, as tools or as the specs they were) and
throwaway repositories. Its choice of BASE: the newest earlier commit whose own check passed, however far back, across
pages of history; a check that only ever failed diffs from its first run's base; the parent only when the check never
ran; a failed history read stops the check (exit 2), GraphQL's failure falls back to git's history and REST's statuses
under one deadline; a range touching no identity code passes unresolved. THE STORE: what is resolved is stored, keyed
by identity code and inputs, and reused; a full store makes room; measurements store their base, not their head; the
A/A self-test; THE BASE ROW; capture gaps; THE HEAD'S OWN RESULTS; THE SKIP GATE and its shadow; THE CORPUS SHAPE.
The same cases hold the shell tool and its Python rewrite at one command line (cli_harness.tool_path)."""
from __future__ import annotations

import glob
import gzip
import os
import re
import shutil
import sys
import unittest
from typing import Dict, List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/identity_takes_check.py", "scripts/ci/identity-takes-check.sh")
CI = os.path.join(cli_harness.ROOT, "scripts/ci")
C0 = "0" * 40
BASH = "/usr/bin/env bash"
# gh: the release kept in $W/release; the jobs API answering the base row's state from $W/base-row (and handing over
# what $W/pending holds: the base row storing it meanwhile); else GraphQL history pages of the commits before c3 -- c2,
# then (PAGED: on a second page) c1, each with its parent and its `identity takes (pl)` state from $W/status-<sha>
# (absent: none). HISTORY_FAILS fails every read, GRAPHQL_FAILS only GraphQL's (REST still reads a status),
# HISTORY_FAILS_ONCE=<file> the first, HISTORY_HANGS / STATUS_HANGS=<pid file> never answer (blocked until killed).
GH = r"""
W=@W@ C1=@C1@ C2=@C2@ C3=@C3@ C0=@C0@
case "$1 $2" in
  "release download") [ -f "$W/release/$5" ] && cp "$W/release/$5" "$7"; exit ;;
  "release upload")   [ -n "${UPLOAD_RACES:-}" ] && { mkdir -p "$W/release" && cp "$4" "$W/release/"; exit 1; }
                      mkdir -p "$W/release" && cp "$4" "$W/release/"; exit ;;
  "release create")   echo "$3" >> "$W/creates"; mkdir -p "$W/release"; exit ;;
  "release delete-asset") rm -f "$W/release/$4"; exit ;;
  "release view")     case "$*" in
                        *sort_by*) filter=; while [ $# -gt 0 ]; do [ "$1" = --jq ] && filter=$2; shift; done
                                   ls -tr "$W/release" 2> /dev/null | jq -R . | jq -s '{assets: [to_entries[] | {name: .value, createdAt: ("2026-01-01T" + (.key | tostring | (("0" * (6 - length)) + .)))}]}' \
                                     | jq -r "$filter"; exit 0 ;;
                      esac
                      case "$*" in
                        *--json*) ;;
                        *) [ -d "$W/release" ] || { echo "release not found" >&2; exit 1; } ;;
                      esac
                      case "$*" in
                        *takes-full-*) f=$(ls "$W"/release/takes-full-*.txt 2> /dev/null | head -1)
                                       [ -n "$f" ] && python3 -c 'import os, sys, datetime as d; print(d.datetime.fromtimestamp(os.path.getmtime(sys.argv[1]), d.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"))' "$f" ;;
                        *) for f in "$W"/release/*; do [ -f "$f" ] && basename "$f"; done ;;
                      esac
                      exit 0 ;;
  "api repos/o/r/actions/runs/7/jobs"*)
    [ -d "$W/pending" ] && mkdir -p "$W/release" && mv "$W"/pending/* "$W/release/" 2> /dev/null && rmdir "$W/pending"
    cat "$W/base-row" 2> /dev/null || true; exit 0 ;;
esac
[ -n "${HISTORY_FAILS:-}" ] && { echo "HTTP 502" >&2; exit 1; }
case "$*" in "api graphql"*) [ -n "${GRAPHQL_FAILS:-}" ] && { echo "invalid character '<' looking for beginning of value" >&2; exit 1; } ;; esac
case "$*" in *"contexts {"*) echo "invalid character '<' looking for beginning of value" >&2; exit 1 ;; esac
[ -n "${HISTORY_FAILS_ONCE:-}" ] && [ ! -f "$HISTORY_FAILS_ONCE" ] && { : > "$HISTORY_FAILS_ONCE"; echo "invalid character '<' looking for beginning of value" >&2; exit 1; }
[ -n "${HISTORY_HANGS:-}" ] && { echo $$ >> "$HISTORY_HANGS"; exec sleep 86400; }
case "$*" in "api repos/o/r/commits/"*"/status"*) [ -n "${STATUS_HANGS:-}" ] && { echo $$ >> "$STATUS_HANGS"; exec sleep 86400; } ;; esac
state() { cat "$W/status-$1" 2>/dev/null || echo -; }
case "$*" in
  "api repos/o/r/commits/"*"/status"*) sha="${2#repos/o/r/commits/}"; state "${sha%%/*}"; exit 0 ;;
  "api repos/o/r/commits/"*) short="${2##*/}"; for c in $C1 $C2 $C3; do [ "${c#$short}" != "$c" ] && { echo "$c"; exit 0; }; done
                            echo "No commit found for SHA: $short" >&2; exit 1 ;;
  *"cursor=$C2 0"*) echo "$C1 $(state $C1) $C0" ;;
  *cursor=*) echo "not a valid cursor" >&2; exit 1 ;;
  *) echo "$C2 $(state $C2) $C1"
     if [ -n "${PAGED:-}" ]; then echo "NEXT $C2 0"; else echo "$C1 $(state $C1) $C0"; fi ;;
esac
"""
# sbt: every argument not a flag is a command, run in order: "[success]" after each, the first that fails ending the
# run. The resolve writes one take (STUB_HEAD_FILM: the head's, where the base's is film 1) and the ratchet the final
# takes (STUB_GAPS: a capture lacking a lookup, written only when read as unknown); each logs its checkout and how it
# ran to $SBT_CALLS. The corpus-shape command writes the findings $SHAPE_HEAD / $SHAPE_BASE name, and fails, as the
# specs do on a finding.
SBT = r"""
flag() { local i; for i in "${!words[@]}"; do [ "${words[$i]}" = "$1" ] && { echo "${words[$((i + 1))]:-yes}"; return; }; done; }
called() { echo "$(pwd -P) $1" >> "$SBT_CALLS"; }
decisions() { # <dir> <cc>
  mkdir -p "$1"
  local film=1; case "$(pwd -P)" in *identity-takes-base*) ;; *) film=${STUB_HEAD_FILM:-1} ;; esac
  echo '{"key":"k","venue":"v","rawTitle":"t","film":'"$film"',"filmTitle":"F","basis":"b","explanation":[],"blocker":null}' > "$1/decisions-$2.jsonl"
}
takes() { # <gaps read as unknown: 1 or empty>
  mkdir -p target/identity-unmatched
  if [ -n "${STUB_GAPS:-}" ]; then
    [ "$1" = 1 ] || { echo "the fixture lacks answers this resolve asks"; exit 1; }
    printf 'country\tkind\tlookup\npl\tfamily\t(RottenTomatoes, record|x)\nuk\ttmdb\tsearch y\n' > target/identity-unmatched/capture-gaps.tsv
  fi
  printf 'country\tvenue\trawTitle\tstatus\tfilm\ttitle\tbasis\npl\tv\tt\tRIGHT\ttmdb:1\tF\tb\n' > target/identity-unmatched/ratchet-takes.tsv
}
traced() { local t; t=$(flag --trace); [ -z "$t" ] || echo "{\"tool\":\"$1\"}" | gzip > "$t"; }
run_one() {
case "$command" in
  *corpusShapePoland*)
    pwd -P >> "$SBT_CALLS"
    mkdir -p target/convergence-diagnostics
    case "$(pwd -P)" in *identity-takes-base*) findings=$SHAPE_BASE ;; *) findings=$SHAPE_HEAD ;; esac
    printf '%s' "$findings" > target/convergence-diagnostics/corpus-shape-recorded-pl-SearchQueryMarkers.txt
    exit 1 ;;
  *"runMain integration.IdentityResolveDump "*)
    called dump-tool; traced dump; decisions "$(flag --out)" "$(flag --full)"
    [ -z "$(flag --population)" ] || echo "kind=identity.population_flips country=$(flag --full) chains=$(flag --population) families=1"
    [ -z "${STUB_DUMP_EXIT:-}" ] || exit "$STUB_DUMP_EXIT" ;;
  *IdentityResolveDumpIntegrationSpec*)
    called dump-spec; decisions "$KINOWO_IDENTITY_DUMP" "$KINOWO_IDENTITY_FULL" ;;
  *"runMain scripts.UnmatchedClustersRatchet "*)
    called ratchet-tool; traced ratchet; [ "$(flag --out)" = target/identity-unmatched ] || exit 2
    takes "$([ -n "$(flag --measure)" ] && echo 1)"; [ -z "${STUB_RATCHET_EXIT:-}" ] || exit "$STUB_RATCHET_EXIT" ;;
  *UnmatchedClustersRatchetSpec*)
    called ratchet-spec; takes "${KINOWO_RATCHET_CAPTURE_GAPS_UNKNOWN:-}" ;;
esac
}
for command in "$@"; do
  case "$command" in -*) continue ;; esac
  read -ra words <<< "$command"
  run_one
  echo "[success] Total time: 1 s"
done
"""
GATE = r"""#!/usr/bin/env bash
echo "$TAKES_SKIP_MODE $*" >> "$GATE_LOG"
if [ "${STUB_GATE:-RUN}" = SKIP ]; then echo SKIP; echo "measure: footprint, 0 readings differ (2 readings)"; echo "takes-skip-gate: SKIP -- stub" >&2; exit 0; fi
echo "takes-skip-gate: RUN -- stub" >&2; echo RUN; exit 1
"""
ARRIETTY = "finding\tCineworld Barnsley\tArrietty (4K Restoration) Experience It In IMAX\tsearches it (4k)\n"
PREVIEW = "finding\tFilmRaum\tLa bola negra , Preview am Mi.\tsearches it (preview)\n"
Env = Dict[str, Optional[str]]


class TakesCase(cli_harness.ToolCase):
    """The origin (c1 <- c2 <- c3, none touching identity code), the job's shallow checkout of it, and the fakes."""
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.origin = self.repo("origin.git", "--bare")
        self.src = self.repo("src")
        self.c: Dict[int, str] = {}
        for n in (1, 2, 3):
            self.commit(n, {"README": f"{n}\n"})
        self.push()
        self.clone_job()
        gh = GH
        for key, value in {"@W@": self.work, "@C1@": self.c[1], "@C2@": self.c[2], "@C3@": self.c[3], "@C0@": C0}.items():
            gh = gh.replace(key, value)
        self.fakes.script("gh", gh, shell=BASH)
        self.fakes.script("sbt", SBT, shell=BASH)
        self.done = cli_harness.Done(0, "", "")

    def commit(self, n: int, files: Dict[str, str]) -> None:
        for path, text in files.items():
            self.write(os.path.join("src", path), text)
            self.git(self.src, "add", path)
        self.git(self.src, "commit", "-q", "-m", f"c{n}")
        self.c[n] = self.git(self.src, "rev-parse", "HEAD")

    def push(self) -> None:
        self.git(self.src, "push", "-q", self.origin, "HEAD:refs/heads/main")
        # github.com serves a filtered fetch of any commit (the history-from-git fallback's)
        self.git(self.origin, "config", "uploadpack.allowFilter", "true")
        self.git(self.origin, "config", "uploadpack.allowAnySHA1InWant", "true")

    def clone_job(self) -> None:
        shutil.rmtree(self.path("job"), ignore_errors=True)
        os.makedirs(self.path("job"))
        self.job = self.path("job")
        self.git(self.job, "clone", "-q", "--depth=1", "--branch", "main", f"file://{self.origin}", ".")

    def status(self, n: int, state: Optional[str]) -> None:
        path = self.path(f"status-{self.c[n]}")
        if state is None:
            if os.path.exists(path):
                os.remove(path)
        else:
            self.write(f"status-{self.c[n]}", state + "\n")

    def run_check(self, env: Optional[Env] = None, bound: float = cli_harness.HANG_BOUND) -> int:
        shutil.rmtree(self.path("out"), ignore_errors=True)
        base: Env = {"GITHUB_REPOSITORY": "o/r", "GITHUB_SHA": self.c[3], "RUNNER_TEMP": self.work,
                     "IDENTITY_TAKES_HISTORY_BACKOFF": "0"}
        self.done = self.run_tool("pl", self.path("out"), env={**base, **(env or {})}, cwd=self.job, bound=bound)
        return self.done.code

    @property
    def log(self) -> str:
        return self.done.output

    def out(self, name: str) -> str:
        path = self.path("out", name)
        if not os.path.exists(path):
            return ""
        with open(path) as f:
            return f.read()

    def base(self) -> str:
        return self.out("base.txt").strip()

    def count(self, pattern: str, text: str) -> int:
        """grep -c: the lines of `text` matching the regular expression."""
        return sum(1 for line in text.splitlines() if re.search(pattern, line))

    # ── THE STORE's fixture: c4 and c5 touch identity code; c5 brings the resolve and the ratchet as tools ─────

    def store_fixture(self) -> None:
        self.commit(4, {"common/src/main/Identity.scala": "4\n"})
        self.commit(5, {"common/src/main/Identity.scala": "5\n",
                        "worker/src/it/scala/IdentityResolveDump.scala": 'object IdentityResolveDump // "trace"\n',
                        "worker/src/test/scala/scripts/UnmatchedClustersRatchet.scala":
                            'object UnmatchedClustersRatchet // "trace"\n'})
        self.push()
        self.clone_job()
        self.write("job/test/resources/fixtures/identity-judgements.tsv",
                   "country\tvenue\trawTitle\tfilm\tverdict\tstate\tscope\tfilmTitle\treason\tjudged\tevidence\n")
        for f in glob.glob(os.path.join(CI, "identity_*.py")) + [
                os.path.join(CI, n) for n in ("report_fields.py", "harness_aa.py", "gh-release.sh", "release-store-prune.sh",
                                              "corpus_shape_diff.py")]:
            os.makedirs(self.path("job/scripts/ci"), exist_ok=True)
            shutil.copy(f, self.path("job/scripts/ci"))

    def store_run(self, head: int, env: Optional[Env] = None, base: Optional[int] = 4) -> int:
        """The check of c<head> on main, storing on the release, against TAKES_BASE=c<base> (None: none named)."""
        shutil.rmtree(self.path("out"), ignore_errors=True)
        shutil.rmtree(self.path("identity-takes-base"), ignore_errors=True)
        self.write("sbt-calls", "")
        full: Env = {"RUNNER_TEMP": self.work, "GITHUB_REPOSITORY": "o/r", "GITHUB_SHA": self.c[head],
                     "GITHUB_REF": "refs/heads/main", "SBT_CALLS": self.path("sbt-calls"), "TAKES_STORE": "fixtures",
                     "KINOWO_CONVERGENCE_PIN_CORPUS_RUN": "42", "GATE_LOG": self.path("gate.log"),
                     **({"TAKES_BASE": self.c[base]} if base is not None else {}), **(env or {})}
        self.done = self.run_tool("pl", self.path("out"), env=full, cwd=self.job)
        return self.done.code

    def sbt_calls(self) -> List[str]:
        return self.read("sbt-calls").splitlines()

    def base_calls(self) -> int:
        return sum(1 for line in self.sbt_calls() if "identity-takes-base" in line)

    def head_calls(self) -> int:
        return sum(1 for line in self.sbt_calls() if "identity-takes-base" not in line)

    def head_labels(self) -> List[str]:
        return sorted(line.split(" ")[1] for line in self.sbt_calls() if "identity-takes-base" not in line)

    def release(self) -> List[str]:
        return sorted(os.listdir(self.path("release"))) if os.path.isdir(self.path("release")) else []

    def clear_release(self) -> None:
        shutil.rmtree(self.path("release"), ignore_errors=True)

    def timings(self, prefix: str = "") -> str:
        rows = [line.split("\t") for line in self.out("timings.tsv").splitlines()]
        return "".join(f"{r[0]}:{r[2]} " for r in rows if r[0].startswith(prefix))

    def at(self, n: int) -> None:
        """The job checked out at c<n>, as CI checks a head out."""
        self.git(self.job, "fetch", "-q", "--depth=1", "origin", self.c[n])
        self.git(self.job, "checkout", "-q", self.c[n])


class HistoryTest(TakesCase):
    def test_the_newest_green_commit_is_the_base_and_no_identity_code_passes_unresolved(self) -> None:
        self.status(1, "SUCCESS")
        self.run_check()
        self.assertEqual(self.c[1], self.base(), self.log)
        self.assertEqual(1, self.count("no identity code changed", self.out("verdict.txt")))
        self.status(2, "FAILURE")
        self.run_check({"PAGED": "1"})
        self.assertEqual(self.c[1], self.base(), "a green commit on a later page of history, past a red one")
        self.status(1, None)
        self.run_check()
        self.assertEqual(self.c[1], self.base(), "a check that only ever failed diffs from its first run's base")
        self.status(2, None)
        self.run_check()
        self.assertEqual(self.c[2], self.base(), "a check that never ran: the parent")
        self.status(2, "SUCCESS")
        self.run_check({"TAKES_BASE": self.c[1]})
        self.assertEqual(self.c[1], self.base(), "a base named by hand overrides the newest green commit")

    def test_a_history_read_failing_once_is_asked_again_and_failing_always_stops_the_check(self) -> None:
        self.status(1, "SUCCESS")
        code = self.run_check({"HISTORY_FAILS_ONCE": self.path("failed-once")})
        self.assertEqual(self.c[1], self.base(), self.log)
        self.assertNotEqual(2, code)
        self.assertEqual(1, self.count("history read 1 of 3 failed", self.log))
        self.status(1, None)
        self.assertEqual(2, self.run_check({"HISTORY_FAILS": "1"}))

    def test_graphql_failing_takes_the_history_from_git_and_the_statuses_from_rest(self) -> None:
        self.status(2, "FAILURE")
        self.status(1, "SUCCESS")
        code = self.run_check({"GRAPHQL_FAILS": "1"})
        self.assertEqual(self.c[1], self.base(), self.log)
        self.assertNotEqual(2, code)
        self.assertEqual(1, self.count(f"{self.c[1]} is green, 2 commit\\(s\\) back \\(history from git\\)", self.log))
        self.assertEqual("1", self.git(self.job, "rev-list", "--count", self.c[3]), "the checkout left one commit deep")
        self.status(1, None)
        code = self.run_check({"GRAPHQL_FAILS": "1"})
        self.assertEqual((2, 1), (code, self.count(r"no green commit in the 2 commit\(s\)", self.log)), self.log)

    def test_the_walk_from_git_stops_at_its_deadline_and_a_hung_read_at_its_bound(self) -> None:
        self.status(2, "FAILURE")
        hung = self.write("status-hung", "")
        code = self.run_check({"GRAPHQL_FAILS": "1", "STATUS_HANGS": hung, "IDENTITY_TAKES_GH_TIMEOUT": "86400",
                               "IDENTITY_TAKES_LOCAL_HISTORY_DEADLINE": "2"})
        self.assertEqual((2, 1, 0), (code, self.count("past its 2 s deadline", self.log), cli_harness.still_running(hung)),
                         self.log)
        self.status(2, None)
        hung = self.write("hung", "")
        code = self.run_check({"HISTORY_HANGS": hung, "IDENTITY_TAKES_GH_TIMEOUT": "1"})
        self.assertEqual((2, 0), (code, cli_harness.still_running(hung)), self.log)

    def test_a_short_base_is_expanded_and_one_naming_no_commit_or_unreachable_stops_the_check(self) -> None:
        self.status(2, "SUCCESS")
        self.run_check({"TAKES_BASE": self.c[1][:9]})
        self.assertEqual(self.c[1], self.base(), self.log)
        self.assertEqual(2, self.run_check({"TAKES_BASE": "deadbeef1"}))
        self.assertEqual(1, self.count("no commit deadbeef1", self.out("verdict.txt")))
        self.assertEqual(2, self.run_check({"TAKES_BASE": "1" * 40}))
        self.assertEqual(1, self.count("could not fetch base", self.out("verdict.txt")))


class StoreTest(TakesCase):
    def setUp(self) -> None:
        super().setUp()
        self.store_fixture()

    def test_with_nothing_stored_both_sides_resolve_and_store(self) -> None:
        # an earlier run that ended before removing its base checkout left it registered, its directory since removed
        self.git(self.job, "worktree", "add", "-q", "--detach", self.path("identity-takes-base"), "HEAD")
        code = self.store_run(5)
        self.assertEqual((0, 0), (code, self.count("already registered", self.log)), self.log)
        self.clear_release()
        if os.path.exists(self.path("creates")):
            os.remove(self.path("creates"))
        code = self.store_run(5)
        self.assertEqual((0, 2), (code, self.base_calls()), self.log)
        self.assertEqual(7, len(self.release()), self.release())
        self.assertEqual("fixtures\n", self.read("creates"))
        self.assertEqual(1, len([a for a in self.release() if a.startswith("takes-tracex-pl-")]))
        traces = [a for a in self.release() if re.match(r"takes-trace-pl-[0-9a-f]{24}-p42\.gz$", a)]
        self.assertEqual(1, len(traces), self.release())
        with open(self.path("release", traces[0]), "rb") as f:
            text = gzip.decompress(gzip.decompress(f.read())).decode()
        self.assertEqual(["dump", "ratchet"], sorted(re.findall(r'"tool":"([a-z]*)"', text)))
        self.assertEqual("phase:how head-final:replayed head-resolve:resolved base-final:replayed base-resolve:resolved ",
                         self.timings())
        self.assertEqual(1, self.count("the base resolve not stored -- takes-decisions-pl-.* not on release fixtures: "
                                       "resolving it", self.log))
        self.assertEqual(["dump-tool", "ratchet-tool"], self.head_labels())
        dirs = {line.split(" ")[1].split("-")[0]: os.path.basename(line.split(" ")[0]) for line in self.sbt_calls()
                if "identity-takes-base" not in line}
        self.assertEqual({"ratchet": "identity-takes-head-final", "dump": "job"}, dirs)
        self.assertEqual(["dump-spec", "ratchet-spec"],
                         sorted(line.split(" ")[1] for line in self.sbt_calls() if "identity-takes-base" in line))

    def test_a_store_over_its_budget_makes_room_first(self) -> None:
        os.makedirs(self.path("release"))
        for i in range(1, 13):
            path = self.write(f"release/takes-decisions-us-old{i}.gz", "")
            os.utime(path, (1767225600 + i, 1767225600 + i))
        code = self.store_run(5, {"TAKES_STORE_ASSETS": "10"})
        mine = [a for a in self.release() if a.startswith("takes-") and not a.startswith(("takes-decisions-us-",
                                                                                            "takes-full-"))]
        self.assertEqual((0, 6), (code, len(mine)), self.log)
        self.assertLessEqual(len(self.release()), 11)
        self.assertNotIn("takes-decisions-us-old1.gz", self.release())
        self.assertIn("takes-decisions-us-old12.gz", self.release())

    def test_outside_actions_the_base_is_in_a_scratch_directory_of_its_own_removed_at_exit(self) -> None:
        os.makedirs(self.path("tmp"))
        code = self.store_run(5, {"RUNNER_TEMP": None, "TMPDIR": self.path("tmp")})
        self.assertEqual((0, 2), (code, self.base_calls()), self.log)
        self.assertEqual([], os.listdir(self.path("tmp")))

    def test_a_failing_resolve_or_ratchet_is_no_verdict(self) -> None:
        code = self.store_run(5, {"STUB_DUMP_EXIT": "1"})
        self.assertEqual((2, 1), (code, self.count("the resolve exited 1", self.log)), self.log)
        self.clear_release()
        code = self.store_run(5, {"STUB_RATCHET_EXIT": "1"})
        self.assertEqual((2, 1), (code, self.count("the ratchet exited 1", self.log)), self.log)

    def test_no_temporary_directory_loses_no_verdict_and_the_population_line_is_reported(self) -> None:
        no_tmp = self.path("no-tmp")
        os.makedirs(no_tmp)
        mktemp = self.write("no-tmp/mktemp", '#!/usr/bin/env bash\necho "mktemp: no temporary directory" >&2; exit 1\n')
        os.chmod(mktemp, 0o755)
        code = self.store_run(5, {"PATH": no_tmp + os.pathsep + self.fakes.bin + os.pathsep + os.environ["PATH"]})
        self.assertEqual((0, 1), (code, self.count(r"identity final takes \(pl\): 1 listings", self.out("verdict.txt"))),
                         self.log)
        self.assertEqual(1, self.count("^kind=identity.population_flips country=pl chains=4 ", self.out("verdict.txt")))

    def test_a_measurement_stores_its_base_not_its_head_and_an_upload_race_is_no_warning(self) -> None:
        self.store_run(5, {"GITHUB_REF": "refs/heads/measure/c5-1"})
        self.assertEqual(2, len(self.release()), self.release())
        self.store_run(5, {"GITHUB_REF": "refs/heads/measure/c5-2"})
        self.assertEqual(0, self.base_calls(), self.log)
        self.assertEqual(1, self.count("base-resolve.*reused", self.out("timings.tsv")))
        self.clear_release()
        self.store_run(5, {"UPLOAD_RACES": "1", "GITHUB_REF": "refs/heads/measure/c5-3"})
        self.assertEqual((0, 1), (self.count("could not store", self.log),
                                  self.count("takes-decisions-pl-.* was stored by another run meanwhile", self.log)),
                         self.log)

    def test_the_aa_self_test_resolves_the_head_twice_and_fails_on_a_difference_as_the_harnesss_fault(self) -> None:
        self.store_run(5)
        stored = self.release()
        code = self.store_run(5, {"TAKES_AA": "true"}, base=None)
        self.assertEqual(0, code, self.log)
        self.assertEqual(self.c[5], self.base())
        self.assertEqual((1, 1), (self.count("identity-takes-base dump-tool", self.read("sbt-calls")),
                                  sum(1 for line in self.sbt_calls() if "identity-takes-base" not in line and "dump-tool" in line)))
        self.assertEqual(2, self.count("ratchet-tool", self.read("sbt-calls")))
        self.assertEqual(stored, self.release())
        self.assertEqual(1, self.count(r"harness A/A \(pl\): PASS", self.out("verdict.txt")))
        code = self.store_run(5, {"TAKES_AA": "true", "STUB_HEAD_FILM": "2"}, base=None)
        self.assertEqual(1, code, self.log)
        self.assertEqual(1, self.count("HARNESS FAULT.*1 model take", self.log))
        self.assertEqual(0, self.count("to another film", self.out("verdict.txt")))

    def side_run(self, side: str) -> int:
        return self.store_run(5, {"GITHUB_REF": "refs/heads/measure/c5-side", "TAKES_SIDE": side,
                                  "TAKES_BASE_JOB": "takes-base (poland)", "GITHUB_RUN_ID": "7", "TAKES_BASE_POLL": "0"})

    def test_the_base_row_stores_the_bases_side_and_the_takes_row_waits_for_it(self) -> None:
        code = self.side_run("base")
        self.assertEqual((0, 2, 0), (code, self.base_calls(), self.head_calls()), self.log)
        dirs = [os.path.basename(line.split(" ")[0]) for kind in ("dump", "ratchet") for line in self.sbt_calls()
                if f" {kind}-" in line]
        self.assertEqual(["identity-takes-base", "identity-takes-base-final"], dirs)
        self.assertEqual(2, len(self.release()))
        self.side_run("base")
        self.assertEqual(0, self.base_calls(), "...or, stored already, nothing at all")
        os.makedirs(self.path("pending"))
        for name in self.release():
            shutil.move(self.path("release", name), self.path("pending", name))
        self.write("base-row", "in_progress\n")
        code = self.side_run("head")
        self.assertEqual((0, 2), (self.base_calls(), self.head_calls()), self.log)
        self.assertEqual("base-final:awaited base-resolve:reused ", self.timings("base-"))
        self.assertEqual(0, code)
        for state in ("completed", "queued"):
            self.clear_release()
            self.write("base-row", state + "\n")
            self.side_run("head")
            self.assertEqual(2, self.base_calls(), state)

    def test_a_capture_lacking_a_lookup_still_replays_naming_only_this_countrys_gap(self) -> None:
        code = self.store_run(5, {"STUB_GAPS": "1"})
        self.assertEqual(0, code, self.log)
        verdict = self.out("verdict.txt")
        self.assertEqual(1, self.count(re.escape("the head's final takes read 1 lookup(s) the capture lacks as UNKNOWN")
                                       + ".*" + re.escape("family (RottenTomatoes, record|x)"), verdict), verdict)
        self.assertEqual(0, self.count("search y", verdict))

    def test_stored_results_are_reused_under_their_key_and_only_it(self) -> None:
        self.store_run(5)
        self.store_run(4, base=2)
        code = self.store_run(5)
        self.assertEqual((0, 0, 0), (self.base_calls(), self.head_calls(), code), self.log)
        self.assertEqual(1, self.count("base resolve reused", self.log))
        self.store_run(5, {"KINOWO_CONVERGENCE_PIN_CORPUS_RUN": "43"})
        self.assertEqual(1, self.base_calls(), "another pinned recording is another key")
        self.assertEqual(1, self.count("base final takes reused", self.log))
        self.write("laid-fill", "unpublished\n")
        self.store_run(5)
        self.assertEqual(2, self.base_calls(), "a fill laid but never published names no key")
        os.remove(self.path("laid-fill"))
        self.clear_release()
        self.store_run(4, {"KINOWO_CONVERGENCE_PIN_CORPUS_RUN": ""}, base=2)
        self.store_run(5, {"KINOWO_CONVERGENCE_PIN_CORPUS_RUN": ""})
        self.assertEqual(1, self.base_calls(), "with no recording pinned the base's decisions are resolved")
        self.assertEqual([], [a for a in self.release() if "decisions" in a])

    def test_a_base_that_stored_nothing_reuses_its_identity_codes_results(self) -> None:
        self.commit(6, {"README": "6\n"})
        self.commit(7, {"common/src/main/Identity.scala": "7\n"})
        self.push()
        self.store_run(5)
        self.at(6)
        self.store_run(6, base=5)
        self.assertEqual(1, self.count("no identity code changed", self.out("verdict.txt")), self.log)
        self.at(7)
        code = self.store_run(7, base=6)
        self.assertEqual((1, 1), (self.count("base resolve reused", self.log), self.count("base final takes reused", self.log)),
                         self.log)
        self.assertEqual((0, 0), (self.base_calls(), code))

    def test_the_heads_own_results_stored_by_a_measurement_are_reused(self) -> None:
        self.commit(6, {"README": "6\n"})
        self.commit(7, {"common/src/main/Identity.scala": "7\n"})
        self.push()
        self.at(7)
        self.store_run(7, {"GITHUB_REF": "refs/heads/measure/c7-1"}, base=5)
        self.assertEqual(1, self.count("the base resolve not stored", self.log), self.log)
        code = self.store_run(5)
        self.assertEqual((0, 1), (self.head_calls(), self.count("every head result was stored -- no head session", self.log)),
                         self.log)
        self.assertEqual("head-final:reused head-resolve:reused ", self.timings("head-"))
        self.assertEqual((0, 1), (code, self.count(r"identity final takes \(pl\): 1 listings", self.out("verdict.txt"))))
        code = self.store_run(5, {"KINOWO_CONVERGENCE_PIN_CORPUS_RUN": "43"})
        self.assertEqual((["dump-tool"], 0), (self.head_labels(), code), self.log)
        for name in self.release():
            if name.startswith("takes-final-"):
                os.remove(self.path("release", name))
        self.store_run(5)
        self.assertEqual(["ratchet-tool"], self.head_labels(), self.log)
        ratchet = [os.path.basename(line.split(" ")[0]) for line in self.sbt_calls()
                   if "identity-takes-base" not in line and " ratchet-" in line]
        self.assertEqual((["identity-takes-head-final"], 0), (ratchet, self.count("the head's session ran", self.log)))
        self.clear_release()
        self.store_run(5, {"STUB_GAPS": "1"})
        code = self.store_run(5, {"STUB_GAPS": "1"})
        verdict = self.out("verdict.txt")
        self.assertEqual((0, 0, 1), (code, self.head_calls(), self.count(re.escape(
            "the head's final takes read 1 lookup(s) the capture lacks as UNKNOWN"), verdict)), self.log)
        self.assertEqual(1, self.count(re.escape("the base's final takes read 1 lookup(s) the capture lacks"), verdict))


class SkipGateTest(TakesCase):
    def setUp(self) -> None:
        super().setUp()
        self.store_fixture()
        self.gate = self.write("gate", GATE)
        os.chmod(self.gate, 0o755)

    def gate_run(self, mode: str, env: Optional[Env] = None, marker: Optional[str] = None) -> int:
        self.write("gate.log", "")
        self.clear_release()
        if marker:
            path = self.write("release/takes-full-pl.txt", "marker\n")
            if marker == "old":
                os.utime(path, (1577836800, 1577836800))
        return self.store_run(5, {"TAKES_SKIP_MODE": mode, "TAKES_SKIP_GATE_CMD": self.gate, **(env or {})})

    def test_off_and_shadow(self) -> None:
        code = self.gate_run("off")
        self.assertEqual((0, 0, 0), (code, len(self.read("gate.log").splitlines()), self.count("takes-skip-shadow", self.log)))
        code = self.gate_run("shadow", {"STUB_GATE": "RUN"})
        self.assertEqual((0, 2, 1), (code, self.head_calls(), self.count(
            r"takes-skip-shadow \(pl\): mode=shadow gate=RUN actual=unmoved agree=yes", self.log)), self.log)
        self.assertEqual(1, self.count(f"^shadow {self.c[4]} {self.c[5]} .*/gate/base-trace.jsonl.gz -- "
                                       ".*/scripts/ci/takes-skip-proof.sh$", self.read("gate.log")))
        code = self.gate_run("shadow", {"STUB_GATE": "SKIP"})
        self.assertEqual((0, 2, 1), (code, self.head_calls(), self.count("gate=SKIP actual=unmoved agree=yes", self.log)))
        code = self.gate_run("shadow", {"STUB_GATE": "SKIP", "STUB_HEAD_FILM": "2"})
        self.assertEqual((1, 1, 1), (code, self.count("gate=SKIP actual=moved agree=no", self.log), self.count(
            "the gate said SKIP but the takes were moved -- the gate is unsound", self.log)), self.log)
        self.gate_run("shadow", {"STUB_GATE": "RUN", "STUB_HEAD_FILM": "2"})
        self.assertEqual(1, self.count("gate=RUN actual=moved agree=yes", self.log))
        self.assertIn("takes-full-pl.txt", self.release(), "a full takes row on main refreshes the daily marker")

    def test_enforce(self) -> None:
        never = {"TAKES_SKIP_SAMPLE_EVERY": "1000000000", "STUB_GATE": "SKIP"}
        code = self.gate_run("enforce", never, marker="fresh")
        self.assertEqual((0, 0, 0), (code, self.head_calls(), self.base_calls()), self.log)
        verdict = self.out("verdict.txt")
        self.assertEqual(1, self.count(re.escape("the takes diff skipped -- measure: footprint, 0 readings differ (2 "
                                                 "readings) under the head's rules"), verdict), verdict)
        self.assertEqual(0, self.count("no identity code changed", verdict))
        code = self.gate_run("enforce", never)
        self.assertEqual((0, 2, 1, 1), (code, self.head_calls(), self.count(
            r"no full takes row of pl on main in the last 24 hours \(no marker\)", self.log), self.count(
            "mode=enforce gate=SKIP actual=unmoved agree=yes", self.log)), self.log)
        code = self.gate_run("enforce", never, marker="old")
        self.assertEqual((0, 2, 1), (code, self.head_calls(), self.count(
            r"no full takes row of pl on main in the last 24 hours \([0-9]* s ago\)", self.log)), self.log)
        code = self.gate_run("enforce", {"TAKES_SKIP_SAMPLE_EVERY": "1", "STUB_GATE": "SKIP"}, marker="fresh")
        self.assertEqual((0, 2, 1), (code, self.head_calls(), self.count("mode=enforce gate=SKIP actual=unmoved agree=yes",
                                                                          self.log)))
        code = self.gate_run("enforce", {"STUB_GATE": "RUN"})
        self.assertEqual((0, 2), (code, self.head_calls()))
        code = self.gate_run("enforce", {"TAKES_SKIP_SAMPLE_EVERY": "x", "STUB_GATE": "SKIP"}, marker="fresh")
        self.assertEqual((0, 2), (code, self.head_calls()), "a sample rate that is no number: the takes run")


class CorpusShapeTest(TakesCase):
    def setUp(self) -> None:
        super().setUp()
        self.store_fixture()
        self.commit(6, {"README": "6\n"})
        spec = "e2e/src/test/scala/services/movies/CorpusShapeSpec.scala"
        self.commit(8, {spec: 'ConvergenceDiagnostics.write(s"corpus-shape-...")\n'})
        self.commit(9, {spec: 'ConvergenceDiagnostics.write(s"corpus-shape-...")\n// allowlist edited\n'})
        self.push()

    def shape_run(self, head: int, base: int, head_findings: str, base_findings: str) -> int:
        return self.store_run(head, {"SHAPE_COMMAND": "corpusShapePoland", "SHAPE_HEAD": head_findings,
                                     "SHAPE_BASE": base_findings}, base=base)

    def test_an_allowlist_edit_runs_the_shape_specs_at_both_and_fails_only_a_new_finding(self) -> None:
        self.at(9)
        code = self.shape_run(9, 8, ARRIETTY, ARRIETTY)
        verdict = self.out("verdict.txt")
        self.assertEqual(1, self.count("no identity code changed", verdict), self.log)
        self.assertEqual(2, len(self.sbt_calls()))
        self.assertEqual(0, code)
        self.assertEqual(1, self.count(r"0 new \(the code.s: fail\), 1 standing", verdict))
        self.clear_release()
        code = self.shape_run(9, 8, ARRIETTY + PREVIEW, ARRIETTY)
        self.assertEqual((1, 1), (code, self.count("FAIL SearchQueryMarkers finding: FilmRaum", self.out("verdict.txt"))),
                         self.log)

    def test_a_base_from_before_the_findings_files_and_a_stored_base(self) -> None:
        self.at(8)
        code = self.shape_run(8, 6, ARRIETTY, PREVIEW)
        self.assertEqual((1, 0), (self.count("the base predates the corpus-shape findings", self.out("verdict.txt")), code),
                         self.log)
        self.at(9)
        code = self.shape_run(9, 8, ARRIETTY + PREVIEW, ARRIETTY)
        self.assertEqual((0, 1), (self.base_calls(), code), self.log)

    def test_specs_that_wrote_no_findings_are_no_verdict(self) -> None:
        self.at(9)
        shutil.rmtree(self.path("out"), ignore_errors=True)
        self.write("sbt-calls", "")
        self.done = self.run_tool("pl", self.path("out"), cwd=self.job, env={
            "GITHUB_REPOSITORY": "o/r", "GITHUB_SHA": self.c[9], "RUNNER_TEMP": self.work,
            "SBT_CALLS": self.path("sbt-calls"), "SHAPE_COMMAND": "corpusShapeNowhere", "TAKES_BASE": self.c[8]})
        self.assertEqual(2, self.done.code, self.log)
        self.assertEqual((1, 0), (self.count("the corpus-shape specs wrote no findings", self.log),
                                  self.count("unbound variable", self.log)))


if __name__ == "__main__":
    unittest.main()
