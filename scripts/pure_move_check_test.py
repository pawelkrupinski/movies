#!/usr/bin/env python3
"""pure_move_check.py: a package move is pure; a move that also edits a line is not, naming the hunk; a moved class's old
name in a production string literal is a name risk; a tool's path list updated to the moved paths is pure, reported
apart; and the edges between -- a move over two commits, visibility widened, a splice that is code, a wildcard that
shadows, a range git cannot judge."""

import os
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
TOOL = os.path.join(HERE, "pure_move_check.py")
SCRATCH_GIT = os.path.join(HERE, "scratch-git.sh")
# the tool reads its repository with every GIT_* variable dropped: under the pre-push hook a GIT_DIR would point it at
# the shared repository, not the scratch one
CLEAN_ENV = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
sys.path.insert(0, HERE)
import pure_move_check  # noqa: E402

FOO = """package services.old

/** The thing that moves. */
final case class Foo(value: Int) {
  private[old] def twice: Int = value * 2
}

object Foo {
  def parse(text: String): Option[Foo] = text.toIntOption.map(Foo(_))
}
"""

USER = """package services.users

import services.old.Foo

object User {
  def of(text: String): Option[Foo] = Foo.parse(text)
  def inline: Int = services.old.Foo(1).value
}
"""

SIBLING = """package services.old

object Sibling {
  def foo: Foo = Foo(3)
}
"""

TOOL_LIST = """# the files the tool lays out
common/src/main/scala/services/old/Foo.scala
common/src/main/scala/tools/Other.scala
"""


class Repo:
    """A throwaway repository, made and committed to through scripts/scratch-git.sh, never a raw git (which under the
    pre-push hook inherits its GIT_DIR and writes into the repository every worktree shares)."""

    def __init__(self, root: str):
        self.root = root
        self.scratch("scratch_repo", "-q")

    def scratch(self, helper: str, *args: str) -> str:
        return subprocess.run(["bash", "-c", '. "$1"; "$2" "$3" "${@:4}"', "_", SCRATCH_GIT, helper, self.root, *args],
                              check=True, capture_output=True, text=True).stdout

    def git(self, *args: str) -> str:
        return self.scratch("scratch_git", *args)

    def write(self, path: str, text: str):
        full = os.path.join(self.root, path)
        os.makedirs(os.path.dirname(full), exist_ok=True)
        with open(full, "w") as f:
            f.write(text)

    def read(self, path: str) -> str:
        with open(os.path.join(self.root, path)) as f:
            return f.read()

    def move(self, old: str, new: str):
        os.makedirs(os.path.dirname(os.path.join(self.root, new)), exist_ok=True)
        self.git("mv", old, new)

    def commit(self, message: str = "c") -> str:
        self.git("add", "-A")
        self.git("commit", "-qm", message)
        return self.git("rev-parse", "HEAD").strip()

    def check(self, rng: str) -> tuple[int, str]:
        done = subprocess.run([sys.executable, "-I", TOOL, rng], cwd=self.root, capture_output=True, text=True, env=CLEAN_ENV)
        return done.returncode, done.stdout + done.stderr


FOO_PATH = "common/src/main/scala/services/old/Foo.scala"
NEW_FOO_PATH = "common/src/main/scala/models/Foo.scala"


class PureMoveCheckTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.repo = Repo(self.tmp.name)
        self.repo.write(FOO_PATH, FOO)
        self.repo.write("common/src/main/scala/services/users/User.scala", USER)
        self.repo.write("common/src/main/scala/services/old/Sibling.scala", SIBLING)
        self.repo.write("scripts/tool-files.txt", TOOL_LIST)
        self.base = self.repo.commit("base")

    def tearDown(self):
        self.tmp.cleanup()

    def move_foo(self):
        """services.old.Foo -> models.Foo, every user following it (an import, an inline name, the sibling's new import)."""
        r = self.repo
        r.move(FOO_PATH, NEW_FOO_PATH)
        r.write(NEW_FOO_PATH, FOO.replace("package services.old", "package models").replace("private[old]", "private[models]"))
        r.write("common/src/main/scala/services/users/User.scala",
                USER.replace("import services.old.Foo", "import models.Foo").replace("services.old.Foo(1)", "models.Foo(1)"))
        r.write("common/src/main/scala/services/old/Sibling.scala", SIBLING.replace("object Sibling", "import models.Foo\n\nobject Sibling"))

    def test_a_package_move_is_pure(self):
        self.move_foo()
        head = self.repo.commit("move")
        code, out = self.repo.check(f"{self.base}..{head}")
        self.assertEqual(0, code, out)
        self.assertIn("PURE MOVE", out)
        self.assertIn("1 file(s) moved", out)

    def test_a_move_that_also_changes_a_line_is_not_pure(self):
        self.move_foo()
        self.repo.write(NEW_FOO_PATH, self.repo.read(NEW_FOO_PATH).replace("value * 2", "value * 3"))
        head = self.repo.commit("move and change")
        code, out = self.repo.check(f"{self.base}..{head}")
        self.assertEqual(1, code, out)
        self.assertIn(f"{FOO_PATH} -> {NEW_FOO_PATH}: code differs", out)
        self.assertIn("+  private[models] def twice: Int = value * 3", out)
        self.assertIn("NOT PURE: 1 non-move hunk(s)", out)

    def test_a_moved_name_in_a_production_string_literal_is_a_name_risk(self):
        self.repo.write("worker/src/main/scala/services/Codecs.scala",
                        'package services\n\nobject Codecs {\n  val Discriminator = "services.old.Foo"\n}\n')
        base = self.repo.commit("a codec naming the class")
        self.move_foo()
        head = self.repo.commit("move")
        code, out = self.repo.check(f"{base}..{head}")
        self.assertEqual(1, code, out)
        self.assertIn("NAME RISK", out)
        self.assertIn("head worker/src/main/scala/services/Codecs.scala:4: a string literal names services.old.Foo", out)

    def test_a_production_literal_rewritten_to_the_new_name_is_no_move(self):
        self.repo.write("worker/src/main/scala/services/Codecs.scala",
                        'package services\n\nobject Codecs {\n  val Discriminator = "services.old.Foo"\n}\n')
        base = self.repo.commit("a codec naming the class")
        self.move_foo()
        self.repo.write("worker/src/main/scala/services/Codecs.scala",
                        'package services\n\nobject Codecs {\n  val Discriminator = "models.Foo"\n}\n')
        head = self.repo.commit("move, the stored name too")
        code, out = self.repo.check(f"{base}..{head}")
        self.assertEqual(1, code, out)
        self.assertIn("worker/src/main/scala/services/Codecs.scala: code differs", out)
        self.assertIn("base worker/src/main/scala/services/Codecs.scala:4: a string literal names services.old.Foo", out)

    def test_a_path_list_following_the_move_is_pure_and_reported_apart(self):
        self.move_foo()
        self.repo.write("scripts/tool-files.txt", TOOL_LIST.replace(FOO_PATH, NEW_FOO_PATH))
        self.repo.write("docs/design.md", "services.old.Foo became models.Foo\n")
        head = self.repo.commit("move and the tool's list")
        code, out = self.repo.check(f"{self.base}..{head}")
        self.assertEqual(0, code, out)
        self.assertIn("reference edits (pure, 2)", out)
        self.assertIn("scripts/tool-files.txt (names the moved paths)", out)
        self.assertIn("docs/design.md (reference list, added)", out)

    def test_a_path_list_changed_beyond_the_moved_paths_is_not_pure(self):
        self.move_foo()
        self.repo.write("scripts/tool-files.txt", TOOL_LIST.replace("common/src/main/scala/tools/Other.scala\n", ""))
        head = self.repo.commit("move, and a list entry dropped")
        code, out = self.repo.check(f"{self.base}..{head}")
        self.assertEqual(1, code, out)
        self.assertIn("scripts/tool-files.txt: differs beyond the moved names", out)

    def test_a_move_over_two_commits_is_followed_hop_by_hop(self):
        r = self.repo
        r.move(FOO_PATH, "common/src/main/scala/services/middle/Foo.scala")
        r.write("common/src/main/scala/services/middle/Foo.scala", FOO.replace("package services.old", "package services.middle")
                .replace("private[old]", "private[middle]"))
        r.write("common/src/main/scala/services/users/User.scala",
                USER.replace("services.old.Foo", "services.middle.Foo"))
        r.write("common/src/main/scala/services/old/Sibling.scala",
                SIBLING.replace("object Sibling", "import services.middle.Foo\n\nobject Sibling"))
        r.commit("hop 1")
        r.move("common/src/main/scala/services/middle/Foo.scala", NEW_FOO_PATH)
        r.write(NEW_FOO_PATH, FOO.replace("package services.old", "package models").replace("private[old]", "private[models]"))
        r.write("common/src/main/scala/services/users/User.scala",
                USER.replace("import services.old.Foo", "import models.Foo").replace("services.old.Foo(1)", "models.Foo(1)"))
        r.write("common/src/main/scala/services/old/Sibling.scala", SIBLING.replace("object Sibling", "import models.Foo\n\nobject Sibling"))
        head = r.commit("hop 2")
        code, out = r.check(f"{self.base}..{head}")
        self.assertEqual(0, code, out)

    def test_comments_and_whitespace_are_not_code(self):
        self.move_foo()
        self.repo.write(NEW_FOO_PATH, self.repo.read(NEW_FOO_PATH).replace("/** The thing that moves. */", "/** It moved. */")
                        .replace("value * 2", "value  *  2"))
        head = self.repo.commit("move, a comment reworded")
        code, out = self.repo.check(f"{self.base}..{head}")
        self.assertEqual(0, code, out)

    def test_a_widened_qualifier_is_visibility_and_pure(self):
        self.move_foo()
        self.repo.write(NEW_FOO_PATH, self.repo.read(NEW_FOO_PATH).replace("private[models]", "private[services]"))
        head = self.repo.commit("move, widened")
        code, out = self.repo.check(f"{self.base}..{head}")
        self.assertEqual(0, code, out)
        self.assertIn("visibility only (pure, 1 file(s))", out)
        self.assertIn("private[old] -> private[services] (twice)", out)

    def test_a_widened_given_is_not_visibility(self):
        r = self.repo
        r.write(FOO_PATH, FOO.replace("object Foo {", "object Foo {\n  private[old] given Ordering[Foo] = Ordering.by(_.value)"))
        base = r.commit("an ordering")
        self.move_foo()
        r.write(NEW_FOO_PATH, r.read(NEW_FOO_PATH).replace("private[models]", "private[services]")
                .replace("object Foo {", "object Foo {\n  private[services] given Ordering[Foo] = Ordering.by(_.value)"))
        head = r.commit("move, the given widened")
        code, out = r.check(f"{base}..{head}")
        self.assertEqual(1, code, out)
        self.assertIn("visibility changed on an implicit/given/extension line", out)

    def test_an_added_import_of_an_unmoved_name_is_not_pure(self):
        self.move_foo()
        self.repo.write("common/src/main/scala/services/users/User.scala",
                        USER.replace("import services.old.Foo", "import models.Foo\nimport services.other.Foo2")
                        .replace("services.old.Foo(1)", "models.Foo(1)"))
        head = self.repo.commit("move, and an import more")
        code, out = self.repo.check(f"{self.base}..{head}")
        self.assertEqual(1, code, out)
        self.assertIn("import added: services.other.Foo2 (neither from the file's old package nor a moved name)", out)

    def test_a_dropped_private_is_not_visibility(self):
        self.move_foo()
        self.repo.write(NEW_FOO_PATH, self.repo.read(NEW_FOO_PATH).replace("private[models] def", "def"))
        head = self.repo.commit("move, made public")
        code, out = self.repo.check(f"{self.base}..{head}")
        self.assertEqual(1, code, out)

    def test_an_added_wildcard_bringing_other_names_is_not_pure(self):
        self.repo.write("common/src/main/scala/models/Bar.scala", "package models\n\nfinal case class Bar(x: Int)\n")
        base = self.repo.commit("a models class")
        self.move_foo()
        self.repo.write("common/src/main/scala/services/users/User.scala",
                        USER.replace("import services.old.Foo", "import models.*").replace("services.old.Foo(1)", "models.Foo(1)"))
        head = self.repo.commit("move, by a wildcard")
        code, out = self.repo.check(f"{base}..{head}")
        self.assertEqual(1, code, out)
        self.assertIn("import added: models.* (a wildcard bringing names that did not move: Bar)", out)

    def wildcard_user(self, body: str) -> str:
        """A user that reached the old package by a wildcard: committed at a base of its own, returned."""
        self.repo.write("common/src/main/scala/services/users/Wild.scala",
                        f"package services.users\n\nimport services.old.*\n\nobject Wild {{\n{body}\n}}\n")
        return self.repo.commit("a wildcard user")

    def test_a_wildcard_left_with_no_use_by_the_move_is_dropped_purely(self):
        base = self.wildcard_user("  def foo: Foo = Foo(1)")
        self.move_foo()
        self.repo.write("common/src/main/scala/services/users/Wild.scala",
                        "package services.users\n\nimport models.Foo\n\nobject Wild {\n  def foo: Foo = Foo(1)\n}\n")
        head = self.repo.commit("move; the wildcard brought only Foo")
        code, out = self.repo.check(f"{base}..{head}")
        self.assertEqual(0, code, out)

    def test_a_wildcard_dropped_while_the_file_names_a_member_left_behind_is_not_pure(self):
        base = self.wildcard_user("  def foo: Foo = Foo(1)\n  def sibling = Sibling.foo")
        self.move_foo()
        self.repo.write("common/src/main/scala/services/users/Wild.scala",
                        "package services.users\n\nimport models.Foo\n\nobject Wild {\n  def foo: Foo = Foo(1)\n  def sibling = Sibling.foo\n}\n")
        head = self.repo.commit("move; the wildcard dropped under Sibling")
        code, out = self.repo.check(f"{base}..{head}")
        self.assertEqual(1, code, out)
        self.assertIn("import removed: services.old.*", out)

    def test_a_range_moving_nothing_is_not_a_move(self):
        self.repo.write("scripts/tool-files.txt", TOOL_LIST + "x\n")
        head = self.repo.commit("an edit, no move")
        code, out = self.repo.check(f"{self.base}..{head}")
        self.assertEqual(1, code, out)
        self.assertIn("moves no file", out)

    def test_an_added_file_is_not_pure(self):
        self.move_foo()
        self.repo.write("common/src/main/scala/models/Alias.scala", "package models\n\nobject Alias\n")
        head = self.repo.commit("move and add")
        code, out = self.repo.check(f"{self.base}..{head}")
        self.assertEqual(1, code, out)
        self.assertIn("common/src/main/scala/models/Alias.scala: added", out)

    def test_a_range_git_cannot_judge_is_undecidable(self):
        self.move_foo()
        head = self.repo.commit("move")
        code, out = self.repo.check(f"{head}..{self.base}")
        self.assertEqual(2, code, out)
        self.assertIn("UNDECIDABLE", out)
        code, out = self.repo.check(f"{self.base}..nosuchref")
        self.assertEqual(2, code, out)

    def test_an_interpolated_splice_is_code_not_a_literal(self):
        segs = pure_move_check.segments('val k = s"p|${services.old.Foo.parse("1")}" // c\nval c = \'"\'')
        self.assertEqual([("code", "val k = "), ("str", 's"p|${'), ("code", "services.old.Foo.parse("), ("str", '"1"'),
                          ("code", ")"), ("str", '}"'), ("code", " "), ("comment", "// c"), ("code", "\nval c = "),
                          ("str", "'\"'")], segs)
        self.assertEqual([], pure_move_check.literal_hits('val k = s"p|${services.old.Foo.parse(x)}"', {"services.old.Foo"}))
        self.assertEqual([(1, "services.old.Foo", 'val k = "services.old.Foo"')],
                         pure_move_check.literal_hits('val k = "services.old.Foo"', {"services.old.Foo"}))


if __name__ == "__main__":
    unittest.main()
