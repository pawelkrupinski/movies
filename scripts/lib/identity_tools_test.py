"""The identity tools' cache functions in Python (scripts/lib/identity_*.py), beside scripts/identity-tools-test.sh, which
holds the shim black-box: what that script does not reach -- the older-ref export files, the verdict record, the push's
reason, a held lock, a convergence run's takes, the recording key."""
from __future__ import annotations

import os
import re
import shutil
import stat
import sys
import tempfile
import unittest
from typing import Dict

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import identity_measure_refs  # noqa: E402
import identity_older_refs  # noqa: E402
import identity_what_if_record  # noqa: E402
from identity_classpath import classpath_line  # noqa: E402
from identity_context import Context, Failed  # noqa: E402
from identity_lock import Locks  # noqa: E402
from identity_recordings import Recordings  # noqa: E402
from jvm_gate import FileLock  # noqa: E402
from tool_files import Files  # noqa: E402

GH_STUB = """#!/bin/sh
while [ "$#" -gt 0 ]; do case "$1" in --dir) dir="$2" ;; --name) name="$2" ;; esac; shift; done
[ -d "$STUB/artifacts/$name" ] || { echo "no artifact matches any of the names or patterns provided" >&2; exit 1; }
mkdir -p "$dir"; cp -R "$STUB/artifacts/$name/." "$dir/"
"""


class IdentityToolsTest(unittest.TestCase):
    def setUp(self) -> None:
        self.work = os.path.realpath(tempfile.mkdtemp(prefix="identity-tools-test-"))
        self.addCleanup(shutil.rmtree, self.work, ignore_errors=True)
        self.env: Dict[str, str] = {"KINOWO_IDENTITY_CACHE": os.path.join(self.work, "cache"), "PATH": os.environ["PATH"],
                                    "STUB": self.work, "GH_DOWNLOAD_BACKOFF": "0"}

    def context(self, **more: str) -> Context:
        self.env.update(more)
        return Context(self.env, os.path.join(self.work, "repo"), Files.here())

    @staticmethod
    def read(path: str) -> str:
        with open(path) as f:
            return f.read()

    def write(self, relative: str, text: str) -> str:
        path = os.path.join(self.work, relative)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "w") as f:
            f.write(text)
        return path

    def test_older_titles_exports_what_the_ref_declares_under_the_new_package(self) -> None:
        main = "tree/common/src/main/scala/services"
        self.write(f"{main}/movies/TitleNormalizer.scala", "package services.movies\nfinal class TitleNormalizer\n")
        self.write(f"{main}/movies/TitleWords.scala", "package services.movies\n// no declaration here\n")
        self.write(f"{main}/billing/Billing.scala", "package services.billing\nobject Billing\n")
        identity_older_refs.older_titles(Files.here(), os.path.join(self.work, "tree"))
        text = self.read(os.path.join(self.work, main, "titles", "OlderRefTitles.scala"))
        self.assertEqual(text, "package services.titles\n\n"
                         "// identity-tools.sh (identity_tree_older_titles): this ref predates services.titles\n"
                         "export services.movies.TitleNormalizer\n\npackage billing {\n  export services.billing.Billing\n}\n")

    def test_a_ref_after_the_move_gets_no_older_titles_file(self) -> None:
        main = "tree/common/src/main/scala/services"
        self.write(f"{main}/movies/TitleNormalizer.scala", "final class TitleNormalizer\n")
        self.write(f"{main}/titles/TitleNormalizer.scala", "final class TitleNormalizer\n")
        identity_older_refs.older_titles(Files.here(), os.path.join(self.work, "tree"))
        self.assertFalse(os.path.exists(os.path.join(self.work, main, "titles", "OlderRefTitles.scala")))

    def test_older_models_exports_the_value_types_the_ref_still_has_in_services_movies(self) -> None:
        movies = "tree/common/src/main/scala/services/movies"
        self.write(f"{movies}/ListingKey.scala", "case class ListingKey()\n")
        self.write(f"{movies}/EmbeddedYear.scala", "case class EmbeddedYear()\n")
        identity_older_refs.older_models(Files.here(), os.path.join(self.work, "tree"))
        text = self.read(os.path.join(self.work, "tree/common/src/main/scala/models/OlderRefModels.scala"))
        self.assertEqual(text, "package models\n\n// identity-tools.sh (identity_tree_older_models): this ref predates "
                         "models.ListingKey\nexport services.movies.ListingKey\nexport services.movies.EmbeddedYear\n")

    def test_the_classpath_line_is_the_last_one_that_is_a_classpath(self) -> None:
        log = "[info] x\n/a/worker/target/s/test-classes:/old.jar\n[info] y\n/b/worker/target/s/test-classes:/new.jar\nlast\n"
        self.assertEqual(classpath_line(log), "/b/worker/target/s/test-classes:/new.jar")
        self.assertEqual(classpath_line("[info] nothing\n"), "")

    def test_a_push_is_explained_in_the_remotes_words(self) -> None:
        rejected = " ! [remote rejected] abc -> measure/x (unable to migrate objects to permanent storage)\n"
        self.assertEqual(identity_measure_refs.reason("noise\n" + rejected, 1),
                         "remote rejected (unable to migrate objects to permanent storage)")
        self.assertEqual(identity_measure_refs.reason("a\nfatal: unable to access\n", 128), "fatal: unable to access")
        self.assertEqual(identity_measure_refs.reason("", 7), "git push exited 7")

    def test_a_measure_branch_names_the_user_the_short_sha_the_tag_and_six_random_hex_digits(self) -> None:
        name = identity_measure_refs.measure_branch("0123456789abcdef", "drift")
        self.assertRegex(name, r"^measure/[A-Za-z0-9._-]+/012345678-drift-[0-9a-f]{6}$")
        self.assertNotEqual(name, identity_measure_refs.measure_branch("0123456789abcdef", "drift"))

    def test_the_verdict_record_keeps_each_countrys_newest_in_the_order_first_finished(self) -> None:
        files, cache = Files.here(), os.path.join(self.work, "cache")
        self.assertIsNone(identity_what_if_record.verdicts(files, cache, "h", "b"))
        identity_what_if_record.record_verdict(files, cache, "h", "b", "pl", "done", "/x/pl.tsv")
        identity_what_if_record.record_verdict(files, cache, "h", "b", "de", "verify-failed", "/x/de.tsv", "3", "whole", "asset")
        identity_what_if_record.record_verdict(files, cache, "h", "b", "pl", "verify-failed", "/x/pl.tsv")
        self.assertEqual(identity_what_if_record.verdicts(files, cache, "h", "b"), "pl verify-failed, de verify-failed")
        lines = self.read(identity_what_if_record.verdicts_file(cache, "h", "b")).splitlines()
        self.assertEqual([line.split("\t")[0] for line in lines], ["pl", "de", "pl"])
        self.assertRegex(lines[0], r"^pl\tdone\t\d{4}-\d\d-\d\dT\d\d:\d\d:\d\dZ\t/x/pl.tsv\t-\t-\t-$")
        self.assertTrue(lines[1].endswith("\t3\twhole\tasset"))

    def test_a_lock_held_by_a_live_process_is_waited_for_until_the_deadline_then_named(self) -> None:
        ctx = self.context(KINOWO_IDENTITY_LOCK_WAIT="0")
        holder = FileLock(os.path.join(ctx.cache, "locks", "busy.flock"))
        os.makedirs(os.path.dirname(holder.path))
        self.assertTrue(holder.take(os.getpid()))
        self.addCleanup(holder.give)
        with self.assertRaises(Failed):
            with Locks(ctx, lambda _s: None).held("busy"):
                self.fail("the lock is held by another description")

    def test_a_lock_is_free_again_and_its_pid_cleared_once_its_block_has_ended(self) -> None:
        ctx = self.context()
        path = os.path.join(ctx.cache, "locks", "free.flock")
        with Locks(ctx).held("free"):
            self.assertEqual(self.read(path).strip(), str(os.getpid()))
        self.assertEqual(self.read(path), "")
        with Locks(ctx).held("free"):
            pass

    def test_a_convergence_runs_takes_are_downloaded_beside_the_diagnostics_and_moved_in_whole(self) -> None:
        self.write("artifacts/convergence-diagnostics-pl-convergence/pl-takes/pl.json.gz", "takes\n")
        gh = self.write("bin/gh", GH_STUB)
        os.chmod(gh, os.stat(gh).st_mode | stat.S_IXUSR)
        ctx = self.context(KINOWO_GH=gh)
        diag = os.path.join(self.work, "diag")
        identity_what_if_record.TakesDownloads(ctx).download("123456", "pl", diag)
        self.assertEqual(self.read(os.path.join(diag, "pl-takes", "pl.json.gz")), "takes\n")
        self.assertEqual([n for n in os.listdir(self.work) if ".dl." in n], [])
        with self.assertRaises(Failed):
            identity_what_if_record.TakesDownloads(ctx).download("123456", "de", os.path.join(self.work, "other"))

    def test_the_recording_key_is_the_run_alone_or_the_run_with_a_digest_of_its_fills(self) -> None:
        bare = Recordings(self.context(), Locks(self.context()))
        self.assertEqual(bare.key("500"), "500")
        filled = Recordings(self.context(IDENTITY_FILLS="b,a"), Locks(self.context()))
        reordered = Recordings(self.context(IDENTITY_FILLS="a b"), Locks(self.context()))
        self.assertRegex(filled.key("500"), r"^500-fills-\d+$")
        self.assertEqual(filled.key("500"), reordered.key("500"))
        self.assertTrue(re.match(r"^.*/recordings/500-fills-\d+/pair/test/resources/fixtures$", filled.fixtures("500")))


if __name__ == "__main__":
    unittest.main()
