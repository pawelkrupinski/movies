#!/usr/bin/env python3
"""scripts/convergence-local.sh (a country's convergence leg run locally on CI's recorded inputs), black-box: the leg
each code runs, the pinned pair restored first, the TMDB key from the environment or the vault (none: stops before
sbt), the MongoDB it picks (MONGODB_URI, the itAll mongod, one already on the fallback port, else a throwaway container
it waits for -- and gives up on once its deadline passes) and the hermetic environment sbt runs in. The tool runs in a
scratch tree of its own (it cds to its checkout), its restore a fake. Ported from convergence-local-test.sh case for
case; the same cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import shutil
import socket
import sys
import unittest
from typing import Dict, List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/convergence_local.py", "scripts/convergence-local.sh")
# What the tool takes with it into its tree, whichever of them exist: the shell tool and the libraries it sources, or
# the Python tool and the modules it imports.
LAYOUT = ["scripts/convergence-local.sh", "scripts/convergence_local.py", "scripts/tool-conventions.sh",
          "scripts/own-mongod.sh", "scripts/lib"]
RESTORE = '''echo "restore $*" >> "$STUB/calls"
[ ! -e "$STUB/restore-fails" ]'''
IT_LOCAL = 'echo "it-local $*" >> "$STUB/calls"'
# sbt notes the environment the leg would run in
SBT = '''echo "sbt $*" >> "$STUB/calls"
env | grep -E '^(MONGODB_URI|MONGODB_DB|TMDB_API_KEY|OMDB_API_KEY|KINOWO_[A-Z_]+)=' | LC_ALL=C sort > "$STUB/sbt-env"'''
# secrets answers from $STUB/vault-<key>
SECRETS = '[ "$1 $2" = "get movies" ] && [ -f "$STUB/vault-$3" ] && cat "$STUB/vault-$3"'
# docker: a container that answers its ping (or never does while $STUB/mongo-never-up exists) and reports PRIMARY
DOCKER = '''echo "docker $1" >> "$STUB/calls"
echo "$*" >> "$STUB/docker-args"
case "$*" in *rs.initiate*) echo "$*" > "$STUB/initiate" ;; esac
case "$1" in
  exec) [ -e "$STUB/docker-hangs" ] && exec sleep 60
        [ -e "$STUB/mongo-never-up" ] && exit 1
        case "$*" in *myState*) echo 1 ;; esac ;;
esac
exit 0'''
# Ports nothing listens on: privileged ones, which no user process can bind (connecting is refused at once).
NO_MIRROR, NO_FALLBACK = "1", "2"


class ConvergenceLocalTest(cli_harness.ToolCase):
    def setUp(self) -> None:
        super().setUp()
        for relative in LAYOUT:
            source = os.path.join(cli_harness.ROOT, relative)
            target = self.path("tree", relative)
            os.makedirs(os.path.dirname(target), exist_ok=True)
            if os.path.isdir(source):
                shutil.copytree(source, target, ignore=shutil.ignore_patterns("__pycache__"))
            elif os.path.exists(source):
                shutil.copy2(source, target)
        self.script_in_tree("restore-country-corpus.sh", RESTORE)
        self.fakes.script("sbt", SBT, shell="/usr/bin/env bash")
        self.fakes.script("secrets", SECRETS, shell="/usr/bin/env bash")
        self.fakes.script("docker", DOCKER, shell="/usr/bin/env bash")
        self.tool = self.path("tree", os.path.relpath(TOOL, cli_harness.ROOT))
        self.mirror_port = NO_MIRROR

    def script_in_tree(self, name: str, body: str) -> None:
        path = self.write(f"tree/scripts/{name}", "#!/usr/bin/env bash\n" + body + "\n")
        os.chmod(path, 0o755)

    def leg(self, *args: str, bare: bool = False, **env: str) -> cli_harness.Done:
        for name in ("calls", "sbt-env", "docker-args"):
            if os.path.exists(self.path(name)):
                os.remove(self.path(name))
        given: Dict[str, Optional[str]] = {
            "STUB": self.work, "LOCAL_MIRROR_PORT": self.mirror_port, "CONVERGENCE_LOCAL_MONGO_PORT": NO_FALLBACK,
            "CONVERGENCE_LOCAL_MONGO_WAIT": "3", "TMDB_API_KEY": None, "OMDB_API_KEY": None, "MONGODB_URI": None,
            "MONGODB_DB": None, "CONVERGENCE_LOCAL_PAIR": None, "CONVERGENCE_LOCAL_OWN_MONGOD": None}
        given.update(env)
        flag = [] if bare else ["--i-need-a-local-leg"]
        return self.run_tool(*flag, *args, tool=self.tool, env=given, bound=60)

    def calls(self) -> List[str]:
        return self.read("calls").splitlines() if os.path.exists(self.path("calls")) else []

    def sbt_env(self, name: str) -> str:
        if not os.path.exists(self.path("sbt-env")):
            return ""
        values = [line.split("=", 1)[1] for line in self.read("sbt-env").splitlines() if line.startswith(name + "=")]
        return values[0] if values else ""

    def test_without_the_flag_a_leg_is_refused_naming_the_faster_tools(self) -> None:
        done = self.leg("de", bare=True, MONGODB_URI="mongodb://given:1/", TMDB_API_KEY="tmdb")
        self.assertEqual(2, done.code, done.output)
        self.assertEqual([], self.calls())
        self.assertIn("convergence-local: refused -- a local leg runs tens of minutes", done.stderr)
        self.assertIn("identity-measure", done.stderr)
        self.assertIn("identity-slice", done.stderr)

    def test_with_the_flag_the_leg_runs_still_naming_the_faster_tools(self) -> None:
        done = self.leg("de", MONGODB_URI="mongodb://given:1/", TMDB_API_KEY="tmdb")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("running a local leg", done.stderr)
        self.assertIn("identity-measure", done.stderr)

    def test_an_unknown_country_is_a_usage_error_restoring_nothing(self) -> None:
        done = self.leg("xx")
        self.assertEqual(2, done.code, done.output)
        self.assertIn("--i-need-a-local-leg <pl|de|uk|us|es> [sbt alias]", done.stderr)
        self.assertEqual([], self.calls())

    def test_a_countrys_leg_restores_its_pair_then_runs_its_suite_hermetically(self) -> None:
        done = self.leg("de", MONGODB_URI="mongodb://given:1/", TMDB_API_KEY="tmdb")
        self.assertEqual(["restore de", "sbt convergenceGermany"], self.calls())
        self.assertEqual("mongodb://given:1/", self.sbt_env("MONGODB_URI"))
        self.assertEqual(["true", "enrichment-de", "de", "de"],
                         [self.sbt_env(k) for k in ("KINOWO_CONVERGENCE_HERMETIC", "KINOWO_CONVERGENCE_ENRICHMENT_FIXTURES",
                                                    "KINOWO_COUNTRY", "KINOWO_COUNTRIES")])
        self.assertTrue(self.sbt_env("MONGODB_DB").startswith("kinowo_conv_de_"), self.sbt_env("MONGODB_DB"))
        self.assertIn("no OMDB_API_KEY", done.stderr)
        self.assertIn("==> MongoDB from MONGODB_URI", done.stdout)
        self.assertIn("==> sbt convergenceGermany", done.stdout)

    def test_a_named_sbt_alias_replaces_the_legs_and_a_given_database_prefix_is_kept(self) -> None:
        self.leg("us", "convergenceUsQuick", MONGODB_URI="mongodb://given:1/", TMDB_API_KEY="tmdb", MONGODB_DB="mine")
        self.assertEqual(["sbt convergenceUsQuick"], [c for c in self.calls() if c.startswith("sbt")])
        self.assertEqual("mine", self.sbt_env("MONGODB_DB"))

    def test_the_keys_come_from_the_vault_and_no_tmdb_key_anywhere_stops_before_sbt(self) -> None:
        self.write("vault-TMDB_API_KEY", "vault-tmdb\n")
        self.write("vault-OMDB_API_KEY", "vault-omdb\n")
        self.leg("pl", MONGODB_URI="mongodb://given:1/")
        self.assertEqual(["vault-tmdb", "vault-omdb"], [self.sbt_env("TMDB_API_KEY"), self.sbt_env("OMDB_API_KEY")])
        os.remove(self.path("vault-TMDB_API_KEY"))
        done = self.leg("pl", MONGODB_URI="mongodb://given:1/")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual([], [c for c in self.calls() if c.startswith("sbt")])
        self.assertIn("no TMDB_API_KEY in the environment or the vault", done.stderr)

    def test_a_pair_that_cannot_be_restored_stops_the_leg(self) -> None:
        self.write("restore-fails", "")
        done = self.leg("uk", MONGODB_URI="mongodb://given:1/", TMDB_API_KEY="tmdb")
        self.assertNotEqual(0, done.code, done.output)
        self.assertEqual(["restore uk"], self.calls())

    def test_nothing_listening_starts_a_throwaway_replica_set_and_waits_for_it(self) -> None:
        done = self.leg("es", TMDB_API_KEY="tmdb")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(["docker run", "docker exec", "docker exec", "docker exec", "docker rm"],
                         [c for c in self.calls() if c.startswith("docker")])
        self.assertEqual(f"mongodb://127.0.0.1:{NO_FALLBACK}/?directConnection=true", self.sbt_env("MONGODB_URI"))
        self.assertIn("sbt convergenceSpain", self.calls())
        # majority acknowledged on apply, not on a journal sync (scripts/own-mongod.sh)
        self.assertIn("rs.initiate({_id: 'rs0', writeConcernMajorityJournalDefault: false, members: [{_id: 0, host: "
                      "'127.0.0.1:27017'}]})", self.read("initiate"))

    def test_the_container_is_this_runs_own_and_removed_when_sbt_ends_never_a_shared_name(self) -> None:
        self.leg("es", TMDB_API_KEY="tmdb")
        named = [a.split("--name ")[1].split()[0] for a in self.read("docker-args").splitlines() if " --name " in a]
        removed = [a for a in self.read("docker-args").splitlines() if a.startswith("rm ")]
        self.assertEqual(1, len(named), named)
        self.assertRegex(named[0], r"^convergence-local-mongo-[0-9]+$")
        self.assertEqual([f"rm -f {named[0]}"], removed)
        self.assertLess(self.calls().index("sbt convergenceSpain"), self.calls().index("docker rm"))
        # an unset port is a free one of its own, never one fixed port every agent's leg fights over
        self.leg("es", TMDB_API_KEY="tmdb", CONVERGENCE_LOCAL_MONGO_PORT="")
        self.assertNotIn(":27117/", self.sbt_env("MONGODB_URI"))
        self.assertNotEqual("", self.sbt_env("MONGODB_URI"))

    def test_a_container_that_never_answers_is_given_up_on(self) -> None:
        self.write("mongo-never-up", "")
        done = self.leg("es", TMDB_API_KEY="tmdb")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("the throwaway MongoDB never answered (ping) in 3s", done.stderr)
        self.assertEqual([], [c for c in self.calls() if c.startswith("sbt")])

    def test_a_docker_call_that_stalls_ends_the_wait_at_its_wall_time_deadline(self) -> None:
        self.write("docker-hangs", "")
        done = self.leg("es", TMDB_API_KEY="tmdb", CONVERGENCE_LOCAL_DOCKER_TIMEOUT="1", CONVERGENCE_LOCAL_MONGO_WAIT="2")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("the throwaway MongoDB never answered (ping) in 2s", done.stderr)

    def test_a_laid_pair_is_copied_in_for_its_country_alone(self) -> None:
        self.write("laid/corpus/cinema-scrapes-de.json.gz", "corpus\n")
        self.write("laid/corpus/cinema-scrapes-de-sample.json.gz", "sample\n")
        self.write("laid/corpus/cinema-scrapes-pl.json.gz", "other\n")
        self.write("laid/enrichment-de/api.themoviedb.org/movie", "tree\n")
        done = self.leg("de", MONGODB_URI="mongodb://given:1/", TMDB_API_KEY="tmdb",
                        CONVERGENCE_LOCAL_PAIR=self.path("laid"))
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(["sbt convergenceGermany"], self.calls())
        fixtures = "tree/test/resources/fixtures"
        self.assertEqual("corpus\n", self.read(f"{fixtures}/corpus/cinema-scrapes-de.json.gz"))
        self.assertEqual("sample\n", self.read(f"{fixtures}/corpus/cinema-scrapes-de-sample.json.gz"))
        self.assertEqual("tree\n", self.read(f"{fixtures}/enrichment-de/api.themoviedb.org/movie"))
        self.assertFalse(os.path.exists(self.path(f"{fixtures}/corpus/cinema-scrapes-pl.json.gz")))
        done = self.leg("uk", MONGODB_URI="mongodb://given:1/", TMDB_API_KEY="tmdb",
                        CONVERGENCE_LOCAL_PAIR=self.path("laid"))
        self.assertEqual(1, done.code, done.output)
        self.assertIn("convergence-local: no laid pair for uk at ", done.stderr)
        self.assertEqual([], self.calls())

    def test_own_mongod_runs_the_suite_through_it_local(self) -> None:
        self.script_in_tree("it-local.sh", IT_LOCAL)
        done = self.leg("de", TMDB_API_KEY="tmdb", CONVERGENCE_LOCAL_OWN_MONGOD="1")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(["restore de", "it-local convergenceGermany"], self.calls())

    def test_the_itall_mongod_answering_is_reused_no_container(self) -> None:
        with socket.socket() as listener:
            listener.bind(("127.0.0.1", 0))
            listener.listen(5)
            self.mirror_port = str(listener.getsockname()[1])
            done = self.leg("pl", TMDB_API_KEY="tmdb")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(f"mongodb://127.0.0.1:{self.mirror_port}/?directConnection=true", self.sbt_env("MONGODB_URI"))
        self.assertEqual([], [c for c in self.calls() if c.startswith("docker")])
        self.assertIn(f"==> reusing the itAll MongoDB on :{self.mirror_port}", done.stdout)


if __name__ == "__main__":
    unittest.main()
