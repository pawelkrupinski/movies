#!/usr/bin/env python3
"""runner_cache.py: an entry mirrored by one job restores the same files in the next, and never anything else.

Run: python3 scripts/ci/runner_cache_test.py
"""
import contextlib
import io
import os
import shutil
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import runner_cache  # noqa: E402

TARGET_PATHS = """project/target
project/project/target
*/target/scala-*
!*/target/scala-*/*.jar
"""


def write(path, text):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w") as f:
        f.write(text)


def tree(root):
    found = {}
    for parent, _dirs, files in os.walk(root):
        for name in files:
            path = os.path.join(parent, name)
            with open(path) as f:
                found[os.path.relpath(path, root)] = f.read()
    return found


class RunnerCacheTest(unittest.TestCase):
    def setUp(self):
        self.base = os.path.realpath(tempfile.mkdtemp())
        self.mirror = os.path.join(self.base, "mirror")
        self.workspace = os.path.join(self.base, "work", "movies")
        os.makedirs(self.mirror)
        os.makedirs(self.workspace)
        self.environment = dict(os.environ)
        os.environ["MOVIES_RUNNER_CACHE"] = self.mirror
        os.environ["HOME"] = os.path.join(self.base, "home")
        self.cwd = os.getcwd()
        os.chdir(self.workspace)

    def tearDown(self):
        os.chdir(self.cwd)
        os.environ.clear()
        os.environ.update(self.environment)
        shutil.rmtree(self.base)

    def run_tool(self, *argv):
        with contextlib.redirect_stdout(io.StringIO()) as out:
            rc = runner_cache.main(list(argv))
        return rc, out.getvalue()

    def job_build(self):
        write("project/target/streams/a", "build definition")
        write("common/target/scala-3.8/classes/A.class", "class A")
        write("common/target/scala-3.8/common.jar", "a packaged jar")
        write("common/target/scala-3.8/nested/kept.jar", "a jar one level down: not the pattern's")
        write("common/src/main/scala/A.scala", "source, not cached")

    def wipe_workspace(self):  # a fresh job's workspace: the same directory, emptied
        for name in os.listdir(self.workspace):
            shutil.rmtree(os.path.join(self.workspace, name))

    def test_a_saved_entry_restores_exactly_its_files_into_the_next_workspace(self):
        self.job_build()
        expected = {k: v for k, v in tree(self.workspace).items()
                    if not k.startswith("common/src/") and k != "common/target/scala-3.8/common.jar"}
        self.assertEqual((0, "runner-cache saved K1\n"), self.run_tool("save", "--key", "K1", "--paths", TARGET_PATHS))
        self.wipe_workspace()
        self.assertEqual((0, "runner-cache hit K1\n"), self.run_tool("restore", "--key", "K1", "--paths", TARGET_PATHS))
        self.assertEqual(expected, tree(self.workspace))

    def test_another_key_or_path_list_is_a_miss(self):
        self.job_build()
        self.run_tool("save", "--key", "K1", "--paths", TARGET_PATHS)
        self.wipe_workspace()
        self.assertEqual(1, self.run_tool("restore", "--key", "K2", "--paths", TARGET_PATHS)[0])
        self.assertEqual(1, self.run_tool("restore", "--key", "K1", "--paths", "project/target\n")[0])
        self.assertEqual({}, tree(self.workspace))

    def test_an_entry_is_written_once_and_never_rewritten(self):
        self.job_build()
        self.run_tool("save", "--key", "K1", "--paths", TARGET_PATHS)
        write("common/target/scala-3.8/classes/A.class", "a later compile")
        self.assertEqual("runner-cache present K1\n", self.run_tool("save", "--key", "K1", "--paths", TARGET_PATHS)[1])
        self.wipe_workspace()
        self.run_tool("restore", "--key", "K1", "--paths", TARGET_PATHS)
        self.assertEqual("class A", tree(self.workspace)["common/target/scala-3.8/classes/A.class"])
        self.assertEqual([], [n for n in os.listdir(self.mirror) if n.startswith(".writing-")])

    def test_a_corrupt_entry_is_a_miss_and_leaves_nothing_behind(self):
        self.job_build()
        self.run_tool("save", "--key", "K1", "--paths", TARGET_PATHS)
        (entry,) = os.listdir(self.mirror)
        with open(os.path.join(self.mirror, entry), "r+b") as f:
            f.seek(os.path.getsize(f.name) // 2)
            f.write(b"\0" * 4096)
        self.wipe_workspace()
        self.assertEqual(1, self.run_tool("restore", "--key", "K1", "--paths", TARGET_PATHS)[0])
        self.assertEqual({}, tree(self.workspace))

    def test_home_paths_and_their_exclusions(self):
        write(os.path.expanduser("~/.cache/coursier/v1/a.jar"), "a dependency")
        write(os.path.expanduser("~/.sbt/boot/sbt.boot.lock"), "")
        write(os.path.expanduser("~/.sbt/global.lock"), "a lock")
        paths = "~/.cache/coursier\n~/.sbt\n!~/.sbt/*.lock\n"
        self.run_tool("save", "--key", "deps", "--paths", paths)
        shutil.rmtree(os.path.expanduser("~"))
        self.run_tool("restore", "--key", "deps", "--paths", paths)
        self.assertEqual({".cache/coursier/v1/a.jar": "a dependency", ".sbt/boot/sbt.boot.lock": ""},
                         tree(os.path.expanduser("~")))

    def test_no_mirror_directory_means_no_mirror(self):
        os.environ["MOVIES_RUNNER_CACHE"] = os.path.join(self.base, "absent")
        self.assertEqual((0, "\n"), self.run_tool("dir"))
        self.assertEqual(1, self.run_tool("restore", "--key", "K1", "--paths", TARGET_PATHS)[0])
        del os.environ["MOVIES_RUNNER_CACHE"]
        self.assertEqual((0, "\n"), self.run_tool("dir"))
        self.assertEqual((0, ""), self.run_tool("save", "--key", "K1", "--paths", TARGET_PATHS))


class IncludedUnderTest(unittest.TestCase):
    def test_an_excluded_entry_is_dropped_and_an_excluded_directory_not_entered(self):
        tree = {"/t": [("/t/a", True), ("/t/x", True), ("/t/f", False)], "/t/a": [("/t/a/g", False)],
                "/t/x": [("/t/x/h", False)]}
        self.assertEqual(runner_cache.included_under("/t", tree.__getitem__, ["/t/x"]), {"/t/a", "/t/a/g", "/t/f"})

    def test_an_entry_not_to_descend_into_is_kept_but_not_listed(self):
        tree = {"/t": [("/t/link", False)]}
        self.assertEqual(runner_cache.included_under("/t", tree.__getitem__, []), {"/t/link"})


if __name__ == "__main__":
    unittest.main()
