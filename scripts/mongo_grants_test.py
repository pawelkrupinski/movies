#!/usr/bin/env python3
"""scripts/mongo_grants.py over fakes: the grants list and the nix file in memory, mongosh a scripted runner holding each
user's roles -- what is required, what is missing, what a grant leaves (a check grants nothing, an apply exactly the gap,
a missing user is reported and never created)."""
from __future__ import annotations

import contextlib
import io
import json
import os
import sys
import unittest
from typing import Dict, List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import mongo_grants as grants  # noqa: E402
import tool_process  # noqa: E402
from seams.tool_files import Files  # noqa: E402

LIST = "# principal\trole\tdatabase\napp\treadWrite\tkinowo\napp\tread\tkinowo_de\nci-read\tread\tkinowo\n"
NIX = 'username = lib.mkOption { type = str; default = "ci-reader"; };\n'
ENV = {"MONGODB_URI": "mongodb://app:pw@h/kinowo", "MONGO_ROOT_URI": "mongodb://root@h", "MONGO_GRANTS_LIST": "/work/list.tsv"}


class FakeMongo:
    """mongosh as root: `users` maps a user to its roles (None: no such user); a grant adds the roles it is handed."""

    def __init__(self, users: Dict[str, Optional[List[Dict[str, str]]]]) -> None:
        self.users, self.granted = users, []  # type: Dict[str, Optional[List[Dict[str, str]]]], List[str]

    def capture(self, command: tool_process.Command) -> tool_process.Captured:
        js, user = command.argv[-1], command.env["G_USER"]
        if "grantRolesToUser" in js:
            self.granted.append(user)
            self.users[user] = (self.users[user] or []) + json.loads(command.env["G_ROLES"])
            return tool_process.Captured(0, "", "")
        roles = self.users.get(user)
        return tool_process.Captured(0, json.dumps(None if roles is None else {"roles": roles}), "")

    def run(self, command: tool_process.Command) -> int:
        raise AssertionError("never run")


def role(name: str, db: str) -> Dict[str, str]:
    return {"role": name, "db": db}


class GrantsTest(unittest.TestCase):
    def run_with(self, users: Dict[str, Optional[List[Dict[str, str]]]], apply: bool) -> "tuple[int, str, FakeMongo]":
        files = Files.in_memory()
        files.write_text("/work/list.tsv", LIST)
        files.write_text(os.path.join(grants.REPO, grants.CI_READ_NIX), NIX)
        mongo = FakeMongo(users)
        out = io.StringIO()
        with contextlib.redirect_stdout(out), contextlib.redirect_stderr(io.StringIO()):
            code = grants.Grants(grants.Mongo(mongo, ENV), files, ENV).run(apply)
        return code, out.getvalue(), mongo

    def test_roles_held_or_carried_by_a_stronger_one_are_ok(self) -> None:
        code, out, mongo = self.run_with({"app": [role("dbOwner", "kinowo"), role("readWrite", "kinowo_de")],
                                          "ci-reader": [role("read", "kinowo")]}, apply=False)
        self.assertEqual((0, []), (code, mongo.granted))
        self.assertIn("ok app app@kinowo: 2 roles", out)
        self.assertIn("ok ci-read ci-reader@", out)

    def test_a_check_names_what_is_missing_and_grants_nothing(self) -> None:
        code, out, mongo = self.run_with({"app": [role("readWrite", "kinowo")], "ci-reader": [role("read", "kinowo")]}, False)
        self.assertEqual((1, []), (code, mongo.granted))
        self.assertIn("missing app app@kinowo: read on kinowo_de", out)

    def test_an_apply_grants_exactly_the_gap_and_reads_the_user_back(self) -> None:
        code, out, mongo = self.run_with({"app": [role("readWrite", "kinowo")], "ci-reader": [role("read", "kinowo")]}, True)
        self.assertEqual((0, ["app"]), (code, mongo.granted))
        self.assertIn("granted app app@kinowo: 1 roles", out)

    def test_a_missing_user_is_reported_and_never_created(self) -> None:
        code, out, mongo = self.run_with({"app": None, "ci-reader": [role("read", "kinowo")]}, True)
        self.assertEqual((1, []), (code, mongo.granted))
        self.assertIn("MISSING USER app app@kinowo", out)

    def test_the_lists_rows_and_a_uris_user_are_read(self) -> None:
        self.assertEqual({"app": [role("readWrite", "kinowo"), role("read", "kinowo_de")], "ci-read": [role("read", "kinowo")]},
                         grants.required(LIST))
        self.assertEqual(("app", "kinowo"), grants.user_of("mongodb://app:pw@h/kinowo"))
        self.assertEqual(("u", "admin"), grants.user_of("mongodb://u@h/?authSource=admin"))
        self.assertEqual("ci-reader", grants.default_username(NIX))


if __name__ == "__main__":
    unittest.main()
