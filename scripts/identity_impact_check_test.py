#!/usr/bin/env python3
"""scripts/identity-impact-check, black-box, in a scratch clone of its own: the tool, scripts/ci/identity_impact.py and
scripts/lib copied in beside a stand-in scripts/sbt-warm (identityImpact writes the selection a case asks for; a
testOnly line writes each class's JUnit report, failing the classes the case names) and a stand-in pre-push hook (the
plan a case asks for, or none).

  mutations  a mutation's selection, then every identity test run: SOUND when each failure was selected, UNSOUND (1)
             naming a failure left out, or a country-scoped spec failing over the countries left out, VACUOUS when
             nothing failed, ALL when identityImpact gave every test; the checkout left at HEAD, the table written
             beside it as printed; refused (2) over uncommitted tooling, a mutation that changes nothing or an
             identityImpact that fails.
  hook       each mutation committed in the checkout, the hook's plan read: AGREES, DISAGREES, NO SELECTION, NO AREA; a
             hook that fails is no verdict (2).
  replay     the last -n identity commits, oldest first, the tooling laid over each: SOUND when the left-out tests pass,
             or fail at the parent too; UNSOUND (1) when one fails only at the commit; NOT MADE when its selection
             fails; --from resumes, appending to the table.

The tool had no test before this one; the same cases hold it across its rewrite."""
from __future__ import annotations

import glob
import os
import shutil
import sys
import unittest
from typing import Dict, List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from harness import cli_harness  # noqa: E402

ROOT = cli_harness.ROOT
TAB = "\t"
A, B, C = "services.identity.ASpec", "services.identity.BSpec", "scripts.CSpec"
RATCHET = "services.identity.UnmatchedClustersRatchetSpec"
EXPECTED = "test/resources/fixtures/identity-unmatched/expected-matches.tsv"
AGREEMENT = "identity/src/main/scala/services/identity/agreement/AgreementStage.scala"
MUTATIONS_HEADER = ["mutation", "verdict", "selected", "of", "countries", "failed", "missed", "outOfScopeFailed"]
HOOK_HEADER = ["mutation", "verdict", "hookClasses", "toolClasses", "hookSummary"]
REPLAY_HEADER = ["commit", "verdict", "selected", "of", "countries", "leftOutFailed", "changedOutcome", "subject"]
# scripts/sbt-warm: `identityImpact <changed> <out>` writes <out> (FAKE_IMPACT) and <out>.tests (FAKE_TESTS), `;` a
# line; `<module>/testOnly <classes> [-- -D...]` writes each class's report, failing those FAKE_FAIL (a run over some
# countries: FAKE_FAIL_SCOPED) or the checkout's fails.txt names
SBT = '''#!{python} -B
import os, sys
env = os.environ
def note(text):
    with open(env["FAKE_LOG"], "a") as f:
        f.write(text + "\\n")
def write(path, text):
    with open(path, "w") as f:
        f.write(text)
def failing(scoped):
    names = env.get("FAKE_FAIL_SCOPED" if scoped else "FAKE_FAIL", "").split()
    if os.path.exists("fails.txt"):
        names += open("fails.txt").read().split()
    return set(names)
code = 0
for command in sys.argv[1:]:
    words = command.split()
    if words[0] == "identityImpact":
        note("identityImpact tooling=" + open("identity-impact.sbt").read().strip())
        if env.get("FAKE_IMPACT_FAIL"):
            sys.exit(1)
        write(words[2], "\\n".join(env.get("FAKE_IMPACT", "identity {a}").split(";")) + "\\n")
        write(words[2] + ".tests", "\\n".join(env.get("FAKE_TESTS", "identity {a};identity {b};worker {c}").split(";")) + "\\n")
        continue
    note(command)
    scoped = "--" in words
    os.makedirs("target/test-reports/unit", exist_ok=True)
    for name in [w for w in words[1:] if w != "--" and not w.startswith("-D")]:
        failed = name in failing(scoped)
        code = code or int(failed)
        write(f"target/test-reports/unit/TEST-{{name}}.xml",
              f'<testsuite name="{{name}}" failures="{{int(failed)}}" errors="0"></testsuite>')
sys.exit(code)
'''.format(python=sys.executable, a=A, b=B, c=C)
HOOK = '''#!/usr/bin/env bash
printf '%s\\n' "pre-push $* only=$PREPUSH_ONLY skip=$PREPUSH_SKIP tmp=$TMPDIR head=$(git log -1 --format=%s)" >> "$FAKE_LOG"
[ -z "${FAKE_HOOK_EXIT:-}" ] || exit "$FAKE_HOOK_EXIT"
[ -z "${FAKE_HOOK_PLAN:-}" ] || cp "$FAKE_HOOK_PLAN" "$PREPUSH_IDENTITY_PLAN"
'''


class ImpactCheckCase(cli_harness.ToolCase):
    """A scratch clone holding the tool, with the stand-ins committed; its checkout and results under `cache/`."""

    def setUp(self) -> None:
        super().setUp()
        self.repo_dir = self.repo("repo", "-b", "main")
        self.tree = self.path("cache", "impact-check")
        self.results = self.tree + ".results"
        self.copy_tool()
        self.put("identity-impact.sbt", "tooling v1\n")
        self.put("project/TestImpact.scala", "object TestImpact\n")
        self.put("project/SameFileEdges.scala", "object SameFileEdges\n")
        self.put("scripts/hooks/local-tests.txt", "identity first scripts/identity-slice-coverage\n"
                 "identity sbt identity/test\nidentity sbt worker/testOnly scripts.*\nother sbt web/test\n")
        self.put("scripts/sbt-warm", SBT, executable=True)
        self.put("scripts/hooks/pre-push", HOOK, executable=True)
        self.put(EXPECTED, "country\tvenue\trawTitle\tfilm\ttitle\npl\tMuza\tFilm\ttmdb:2\tFilm (2020)\n")
        self.put(AGREEMENT, "object AgreementStage {\n  val CorrectionsPerApply = 200\n}\n")
        self.commit_tree(self.repo_dir, "the tool and its stand-ins")

    def copy_tool(self) -> None:
        """The tool as it stands in this checkout -- its name and any identity_impact_check*.py beside it --, with the
        selection it proves and the shared modules it imports."""
        names = ["scripts/identity-impact-check", "scripts/ci/identity_impact.py"]
        names += [os.path.relpath(p, ROOT) for p in glob.glob(os.path.join(ROOT, "scripts", "identity_impact_check*.py"))
                  + glob.glob(os.path.join(ROOT, "scripts", "lib", "**", "*.py"), recursive=True) if not p.endswith("_test.py")]
        for name in names:
            os.makedirs(os.path.dirname(os.path.join(self.repo_dir, name)), exist_ok=True)
            shutil.copy2(os.path.join(ROOT, name), os.path.join(self.repo_dir, name))

    def put(self, relative: str, text: str, executable: bool = False) -> None:
        path = self.write(os.path.join("repo", relative), text)
        if executable:
            os.chmod(path, 0o755)

    def check(self, *args: str, **env: str) -> cli_harness.Done:
        return self.run_tool(*args, "--tree", self.tree, tool=os.path.join(self.repo_dir, "scripts", "identity-impact-check"),
                             env={"FAKE_LOG": self.path("sbt.log"), **env})

    def log(self) -> List[str]:
        return self.read("sbt.log").splitlines() if os.path.exists(self.path("sbt.log")) else []

    def rows(self, done: cli_harness.Done) -> List[List[str]]:
        return [line.split(TAB) for line in done.stdout.splitlines()]


class MutationsTest(ImpactCheckCase):
    def mutation(self, *only: str, **env: str) -> cli_harness.Done:
        return self.check("mutations", "--only", ",".join(only or ("expected-uk",)), **env)

    def test_a_failure_the_selection_holds_is_sound_and_the_table_is_written_as_printed(self) -> None:
        done = self.mutation(FAKE_FAIL=A)
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([MUTATIONS_HEADER, ["expected-uk", "SOUND", "1", "3", "uk", A, "-", "-"]], self.rows(done))
        self.assertEqual(done.stdout, open(os.path.join(self.results, "mutations.tsv")).read())
        self.assertIn("identity-impact-check: mutation expected-uk: a British take expected that no capture makes",
                      done.stderr)
        self.assertTrue(done.stderr.rstrip().endswith(f"SOUND (1 mutations; {self.results}/mutations.tsv)"),
                        done.stderr)
        self.assertIn(f"identity/testOnly {A} {B}", self.log())
        self.assertIn(f"worker/testOnly {C}", self.log())

    def test_the_mutation_reaches_the_selection_uncommitted_and_the_checkout_is_left_at_head(self) -> None:
        self.mutation()
        self.assertEqual(f"{EXPECTED}\n", open(os.path.join(self.results, "expected-uk.changed")).read())
        self.assertEqual(open(os.path.join(self.repo_dir, EXPECTED)).read(),
                         open(os.path.join(self.tree, EXPECTED)).read())
        self.assertEqual(self.git(self.repo_dir, "rev-parse", "HEAD"), self.git(self.tree, "rev-parse", "HEAD"))

    def test_a_failure_the_selection_left_out_is_unsound_naming_it(self) -> None:
        done = self.mutation(FAKE_FAIL=B)
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(["expected-uk", "UNSOUND", "1", "3", "uk", B, B, "-"], self.rows(done)[1])
        self.assertTrue(done.stderr.rstrip().endswith(f"UNSOUND: expected-uk (1 mutations; {self.results}/mutations.tsv)"),
                        done.stderr)

    def test_nothing_failing_is_vacuous_not_failed(self) -> None:
        done = self.mutation()
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(["expected-uk", "VACUOUS", "1", "3", "uk", "-", "-", "-"], self.rows(done)[1])

    def test_a_country_scoped_spec_runs_again_over_the_countries_left_out(self) -> None:
        impact = {"FAKE_IMPACT": f"identity {RATCHET}", "FAKE_TESTS": f"identity {RATCHET};identity {B}"}
        done = self.mutation(FAKE_FAIL=RATCHET, **impact)
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(["expected-uk", "SOUND", "1", "2", "uk", RATCHET, "-", "-"], self.rows(done)[1])
        self.assertIn(f"identity/testOnly {RATCHET} -- -Didentity.countries=pl,de,us,es", self.log())
        failed = self.mutation(FAKE_FAIL=RATCHET, FAKE_FAIL_SCOPED=RATCHET, **impact)
        self.assertEqual(1, failed.code, failed.output)
        self.assertEqual(["expected-uk", "UNSOUND", "1", "2", "uk", RATCHET, "-", f"{RATCHET} (over pl,de,us,es)"],
                         self.rows(failed)[1])

    def test_every_test_selected_is_all_and_each_mutation_a_row_in_order(self) -> None:
        done = self.mutation("agreement", "expected-uk", FAKE_IMPACT="ALL a build file changed", FAKE_FAIL=C)
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([["expected-uk", "SOUND", "ALL", "3", "uk", C, "-", "-"],
                          ["agreement", "SOUND", "ALL", "3", "pl,uk,de,us,es", C, "-", "-"]], self.rows(done)[1:])
        self.assertNotIn(True, ["-Didentity.countries" in line for line in self.log()])

    def test_uncommitted_tooling_is_refused(self) -> None:
        self.put("identity-impact.sbt", "tooling, edited\n")
        done = self.mutation()
        self.assertEqual(2, done.code, done.output)
        self.assertIn("could not be made: commit the selection's tooling first", done.stderr)

    def test_a_mutation_that_changes_nothing_is_no_verdict(self) -> None:
        self.put(AGREEMENT, "object AgreementStage\n")
        self.commit_tree(self.repo_dir, "the constant gone")
        done = self.mutation("agreement")
        self.assertEqual(2, done.code, done.output)
        self.assertIn(f"could not be made: the mutation changed nothing in {AGREEMENT}", done.stderr)

    def test_an_identity_impact_that_fails_is_no_verdict(self) -> None:
        done = self.mutation(FAKE_IMPACT_FAIL="1")
        self.assertEqual(2, done.code, done.output)
        self.assertIn(f"could not be made: identityImpact failed: {self.results}/expected-uk.impact.log", done.stderr)


class HookTest(ImpactCheckCase):
    def hook(self, plan: Optional[str], *only: str, **env: str) -> cli_harness.Done:
        extra: Dict[str, str] = dict(env)
        if plan is not None:
            extra["FAKE_HOOK_PLAN"] = self.write("hook.plan", plan)
        return self.check("hook", "--only", ",".join(only or ("expected-uk",)), **extra)

    def test_a_hook_naming_the_same_classes_agrees(self) -> None:
        done = self.hook(f"# test-impact selection: 1 class\nidentity/testOnly {A} -- -Didentity.countries=uk\n")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([HOOK_HEADER, ["expected-uk", "AGREES", "1", "1", "test-impact selection: 1 class"]],
                         self.rows(done))
        hook_tmp = os.path.join(self.results, "expected-uk.hook-tmp")
        self.assertIn(f"pre-push --checks-only HEAD~1..HEAD only=identity skip=eslint lintspecs tmp={hook_tmp} "
                      "head=identity-impact-check: expected-uk", self.log())
        self.assertEqual(self.git(self.repo_dir, "rev-parse", "HEAD"), self.git(self.tree, "rev-parse", "HEAD"))

    def test_a_hook_naming_other_classes_disagrees(self) -> None:
        done = self.hook(f"# test-impact selection: 2 classes\nidentity/testOnly {A} {B}\n")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(["expected-uk", "DISAGREES", "2", "1", "test-impact selection: 2 classes"], self.rows(done)[1])
        self.assertIn("UNSOUND: expected-uk", done.stderr)

    def test_a_hook_falling_back_to_every_test_is_no_selection(self) -> None:
        done = self.hook("# no test-impact selection (identityImpact failed): every identity test\nidentity/test\n")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(["expected-uk", "NO SELECTION", "1", "1",
                          "no test-impact selection (identityImpact failed): every identity test"], self.rows(done)[1])

    def test_a_hook_running_no_identity_area_is_no_area(self) -> None:
        done = self.hook(None)
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(["expected-uk", "NO AREA", "0", "1", "the hook runs no identity area for this change"],
                         self.rows(done)[1])

    def test_a_hook_that_fails_is_no_verdict(self) -> None:
        done = self.hook(None, FAKE_HOOK_EXIT="3")
        self.assertEqual(2, done.code, done.output)
        self.assertIn(f"could not be made: the hook failed (exit 3): {self.results}/expected-uk.hook.log", done.stderr)


class ReplayTest(ImpactCheckCase):
    def history(self, breaking: bool) -> List[str]:
        """Two identity commits after the first, the second failing BSpec when `breaking`, then a tooling change no
        replay walks: the identity commits' shas."""
        self.put("identity/src/main/scala/A.scala", "object A\n")
        shas = [self.commit_tree(self.repo_dir, "identity: a first change")]
        self.put("identity/src/main/scala/A.scala", "object A { val b = 1 }\n")
        if breaking:
            self.put("fails.txt", f"{B}\n")
        shas.append(self.commit_tree(self.repo_dir, "identity: a second change"))
        self.put("identity-impact.sbt", "tooling v2\n")
        self.commit_tree(self.repo_dir, "the selection's tooling, later")
        return shas

    def replay(self, *args: str, **env: str) -> cli_harness.Done:
        return self.check("replay", "--ref", "main", *args, **env)

    def test_a_left_out_test_failing_only_at_the_commit_is_unsound(self) -> None:
        first, second = self.history(breaking=True)
        done = self.replay("-n", "2")
        self.assertEqual(1, done.code, done.output)
        every = "pl,uk,de,us,es"
        self.assertEqual([REPLAY_HEADER, [first[:9], "SOUND", "1", "3", every, "-", "-", "identity: a first change"],
                          [second[:9], "UNSOUND", "1", "3", every, B, B, "identity: a second change"]], self.rows(done))
        self.assertEqual(["identityImpact tooling=tooling v2"] * 2,
                         [line for line in self.log() if line.startswith("identityImpact")])
        self.assertEqual([f"identity/testOnly {B}", f"worker/testOnly {C}"] * 2 + [f"identity/testOnly {B}"],
                         [line for line in self.log() if not line.startswith("identityImpact")])
        self.assertIn(f"UNSOUND: {second[:9]} (2 replay; {self.results}/replay.tsv)", done.stderr)

    def test_a_left_out_test_failing_at_the_parent_too_is_sound(self) -> None:
        second = self.history(breaking=False)[1]
        done = self.replay("-n", "1", FAKE_FAIL=C)
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([second[:9], "SOUND", "1", "3", "pl,uk,de,us,es", C, "-", "identity: a second change"],
                         self.rows(done)[1])
        self.assertEqual(f"worker/testOnly {C}", self.log()[-1])

    def test_a_selection_that_fails_is_not_made(self) -> None:
        second = self.history(breaking=False)[1]
        done = self.replay("-n", "1", FAKE_IMPACT_FAIL="1")
        self.assertEqual(0, done.code, done.output)
        row = self.rows(done)[1]
        self.assertEqual([second[:9], "NOT MADE", "-", "-", "-", "-"], row[:6])
        self.assertTrue(row[6].startswith("identityImpact failed: "), row)
        self.assertEqual("identity: a second change", row[7])

    def test_from_resumes_the_replay_appending_to_its_table(self) -> None:
        first, second = self.history(breaking=False)
        self.assertEqual(0, self.replay("-n", "2").code)
        done = self.replay("-n", "2", "--from", "1")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([REPLAY_HEADER, [second[:9], "SOUND", "1", "3", "pl,uk,de,us,es", "-", "-",
                                          "identity: a second change"]], self.rows(done))
        table = [line.split(TAB)[0] for line in open(os.path.join(self.results, "replay.tsv")).read().splitlines()]
        self.assertEqual(["commit", first[:9], second[:9], second[:9]], table)


class UsageTest(ImpactCheckCase):
    def test_help_names_the_modes_and_an_unknown_mode_is_a_usage_error(self) -> None:
        done = self.check("--help")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("THE PROOF OF THE IDENTITY TEST-IMPACT SELECTION", done.stdout)
        self.assertIn("{mutations,hook,replay}", done.stdout)
        unknown = self.check("bogus")
        self.assertEqual(2, unknown.code, unknown.output)
        self.assertIn("invalid choice: 'bogus'", unknown.stderr)


if __name__ == "__main__":
    unittest.main()
