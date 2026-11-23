#!/usr/bin/env python3
"""scripts/venue-replay, black-box: each ref laid out once with the tool's files dropped in and read on ONE recording,
the two refs' listings diffed (a listing only one reads, a field the two read differently); a replay missing a recorded
request exits 3 after printing what it read, any other failure stops at its ref; --prod skipped by name with no local
mirror, and a mirror read that hangs cut off at its bound; --fixtures handed over absolute; usage errors refused. java,
sbt and mongosh are fakes, the recording is already in a scratch cache. Ported from venue-replay-test.sh case for case;
the same cases hold the shell tool and its Python rewrite (the extensionless name, whichever it execs)."""
from __future__ import annotations

import os
import stat
import unittest

from identity_tool_fixture import cli_harness, tool_repo

TOOL_FILES = ("worker/src/test/scala/scripts/VenueReplay.scala", "worker/src/fixtures/scala/tools/RecordedCorpus.scala",
              "worker/src/fixtures/scala/tools/CinemaSlotInvariants.scala",
              "worker/src/fixtures/scala/tools/SearchQueryMarkers.scala")
SCRIPTS = ("venue-replay", "venue_replay.py", "venue-replay-tool-files.txt", "venue-replay-prod.js", "identity-tools.sh",
           "tool-conventions.sh", "jvm-gate.sh", "venue_replay_diff.py")
FIXTURES = "cache/recordings/777/pair/test/resources/fixtures"
# java records its arguments and writes the listings the base and head refs read; JAVA_EXIT ends it so
JAVA = '''echo "java $PWD $*" >> "$STUB/calls"
out="" ref=""
while [ "$#" -gt 0 ]; do case "$1" in --out) out=$2 ;; --ref) ref=$2 ;; esac; shift; done
{ printf 'raw\\tclean\\tsynopsis\\turl\\n'
  if [ "$ref" = HEAD~1 ]; then printf 'Trzy\\tTrzy\\tScenariusz: X\\thttps://k/trzy\\nObcy\\tObcy\\t-\\thttps://k/obcy\\n'
  else printf 'Trzy\\tTrzy\\tPo rozstaniu\\thttps://k/trzy\\nRoza\\tRoza\\t-\\thttps://k/roza\\n'; fi; } > "$out"
echo "listings at $ref"
[ -z "${JAVA_EXIT:-}" ] || { echo "FAIL: the recorded trees lack 1 request(s)" >&2; exit "$JAVA_EXIT"; }'''
NO_MIRROR = '#!/usr/bin/env bash\necho "no mirror" >&2\nexit 2\n'


class VenueReplayTest(cli_harness.ToolCase):
    TOOL = ""

    def setUp(self) -> None:
        super().setUp()
        self.write("repo/scripts/local-mirror/freshness", NO_MIRROR)
        self.executable(self.path("repo/scripts/local-mirror/freshness"))
        self.repo_at = tool_repo(self, SCRIPTS, TOOL_FILES, "build\n", "build\n// head\n")
        self.write(f"{FIXTURES}/corpus/cinema-scrapes-pl.json.gz", "corpus\n")
        os.makedirs(self.path(FIXTURES, "enrichment-pl"))
        self.fakes.script("java", JAVA, shell="/usr/bin/env bash")

    @staticmethod
    def executable(path: str) -> None:
        os.chmod(path, os.stat(path).st_mode | stat.S_IXUSR)

    def replay(self, *args: str, **env: str) -> cli_harness.Done:
        return self.run_tool(*args, tool=os.path.join(self.repo_at.checkout, "scripts", "venue-replay"),
                             env={"STUB": self.work, "KINOWO_IDENTITY_CACHE": self.path("cache"), **env})

    def calls(self) -> str:
        return self.read("calls") if os.path.exists(self.path("calls")) else ""

    def test_two_refs_on_one_recording_diffed(self) -> None:
        done = self.replay("pl", "ziemi rybnickiej", "--recording", "777", "--title", "trzy", "--ref", "HEAD~1",
                           "--ref", "HEAD")
        self.assertEqual(0, done.code, done.output)
        refs = [d for d in os.listdir(self.path("cache", "refs")) if os.path.isdir(self.path("cache", "refs", d))]
        self.assertEqual(2, len(refs), "both refs are laid out once in the cache")
        self.assertEqual(f"// {TOOL_FILES[0]}\n", self.read(f"cache/refs/{self.repo_at.base}/{TOOL_FILES[0]}"))
        self.assertEqual(f"// {TOOL_FILES[3]}\n", self.read(f"cache/refs/{self.repo_at.head}/{TOOL_FILES[3]}"))
        fixtures = self.path(FIXTURES)
        self.assertEqual(2, self.calls().count(f"--corpus {fixtures}/corpus/cinema-scrapes-pl.json.gz --recording {fixtures} "),
                         "each ref reads the SAME recording")
        lines = self.calls().splitlines()
        self.assertEqual(2, len([c for c in lines if "scripts.VenueReplay pl --venue ziemi rybnickiej " in c and "--title trzy" in c]))
        out = done.stdout.splitlines()
        self.assertEqual((1, 1), (out.count("listings at HEAD~1"), out.count("listings at HEAD")))
        self.assertEqual(["- Obcy: only at HEAD~1", "+ Roza: only at HEAD"], [line for line in out if line[:2] in ("- ", "+ ")])
        at = out.index("~ Trzy")
        self.assertEqual(["~ Trzy", "synopsis: 'Scenariusz: X'", "→ 'Po rozstaniu'"], [line.strip() for line in out[at:at + 3]])

    def test_a_replay_missing_a_recorded_request_exits_3_after_printing_what_it_read(self) -> None:
        done = self.replay("pl", "kino", "--recording", "777", JAVA_EXIT="3")
        self.assertEqual((3, "listings at .\n"), (done.code, done.stdout), done.output)

    def test_a_replay_that_fails_otherwise_stops_at_that_ref(self) -> None:
        done = self.replay("pl", "kino", "--recording", "777", "--ref", "HEAD~1", "--ref", "HEAD", JAVA_EXIT="1")
        self.assertEqual((1, 1), (done.code, done.stdout.count("listings at")), done.output)

    def test_prod_with_no_local_mirror_is_skipped_by_name(self) -> None:
        done = self.replay("pl", "kino", "--recording", "777", "--prod")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("no local prod mirror for pl", done.stderr)
        self.assertNotIn("--prod", self.calls())

    def test_a_mongosh_that_hangs_is_cut_off_at_its_bound_leaving_none_running(self) -> None:
        self.write("repo/scripts/local-mirror/freshness", "#!/usr/bin/env bash\nexit 0\n")
        hung = self.write("mongosh-hung", "")
        self.fakes.script("mongosh", f'echo $$ >> "{hung}"\nexec sleep 86400', shell="/usr/bin/env bash")
        done = self.replay("pl", "kino", "--recording", "777", "--prod", KINOWO_VENUE_REPLAY_MONGOSH_SECONDS="1")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("mongosh read failed or took over 1s", done.stderr)
        self.assertEqual(0, cli_harness.still_running(hung), "...leaving no mongosh running")

    def test_a_fixture_tree_is_handed_over_by_its_absolute_path(self) -> None:
        os.makedirs(self.path("tree"))
        done = self.run_tool("pl", "kino", "--recording", "777", "--fixtures", "tree", cwd=self.work,
                             tool=os.path.join(self.repo_at.checkout, "scripts", "venue-replay"),
                             env={"STUB": self.work, "KINOWO_IDENTITY_CACHE": self.path("cache")})
        self.assertEqual(0, done.code, done.output)
        tree = {self.path("tree"), os.path.realpath(self.path("tree"))}
        words = self.calls().split()
        handed = [words[i + 1] for i, word in enumerate(words[:-1]) if word == "--fixtures"]
        self.assertEqual(1, len([path for path in handed if path in tree]), self.calls())

    def test_usage_errors(self) -> None:
        self.assertEqual(2, self.replay("pl", "kino", "--recording", "777", "--fixtures", self.path("nope")).code,
                         "a fixture tree that is not there")
        self.assertEqual(2, self.replay("pl", "kino", "--wat").code, "an unknown argument")
        self.assertEqual(2, self.replay("pl", "kino", "--recording").code, "a flag given last, with no value")


if __name__ == "__main__":
    unittest.main()
