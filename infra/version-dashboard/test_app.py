#!/usr/bin/env python3
"""What the fleet table promises, checked without a flake, a Prometheus or a host.

`render` is a pure function of the dict `build` returns, which is the whole reason this file can
exist: every branch that decides what a machine's row SAYS is reachable from a literal, so the
cases worth pinning are the ones nobody can produce on demand on a three-machine fleet -- a host
that has gone silent, one built from a dirty tree, one whose auto-apply is refusing a change.

Run:  python3 infra/version-dashboard/test_app.py
"""
import importlib.util
import pathlib
import re
import sys
import time
import unittest

APP = pathlib.Path(__file__).with_name("app.py")
_spec = importlib.util.spec_from_file_location("nixos_dashboard_app", APP)
app = importlib.util.module_from_spec(_spec)
sys.modules[_spec.name] = app
_spec.loader.exec_module(app)


# The seven columns the sibling dashboard at :8787 renders, in its order. THE POINT OF PINNING THEM
# IS THAT THE TWO SCREENS ARE READ BY THE SAME PERSON, minutes apart, and a column that moves
# between them is read as a different fact rather than the same one in a different place.
REFERENCE_COLUMNS = ["machine", "role · env", "address", "closure", "nixpkgs", "auto-apply",
                     "state"]

CLOSURE = "6idh361s36gw9zqk2p0v4x8n1m7c3jt5-nixos-system-mongo-1-26.05.20260827.d57af92"


def machine(**over):
    """A reporting, current, unremarkable host. Each test names only what it changes."""
    row = {
        "name": "mongo-1", "hostname": "mongo-1", "role": "mongo", "env": "prod",
        "private": "10.20.0.10", "public": "1.2.3.4", "reporting": True,
        "closure": CLOSURE, "booted": CLOSURE, "nixpkgs": "26.05.20260827.d57af92",
        "revision": "aaa92e6e2", "revision_short": "aaa92e6e2", "staged_revision": "",
        "staged_short": "", "dirty": False, "behind": 0,
        "auto_apply": "up_to_date", "blocked_reason": "", "detail": "", "excluded_reason": "",
        "apply_covered": True, "last_verdict": time.time() - 600,
        "state": "current", "state_key": "current", "severity": "ok", "actionable": False,
    }
    row.update(over)
    return row


def page(*rows):
    return app.render({
        "built_at": time.time(), "took": 1.2, "rows": list(rows), "undeclared": [],
        "head": "aaa92e6e2", "origin": "aaa92e6e2", "dirty_checkout": False, "errors": [],
    })


def headers(html_text):
    row = re.search(r"<table><tr>(.*?)</tr>", html_text, re.S).group(1)
    return [re.sub(r"<[^>]+>", "", c).replace("&middot;", "·").strip()
            for c in re.findall(r"<th>(.*?)</th>", row, re.S)]


class Columns(unittest.TestCase):
    def test_columns_match_the_reference_dashboard(self):
        self.assertEqual(headers(page(machine())), REFERENCE_COLUMNS)

    def test_role_cell_carries_the_environment_as_a_coloured_pill(self):
        html_text = page(machine(role="mongo", env="prod"))
        self.assertIn("<td>mongo <span class=env style='background:#e5484d'>prod</span></td>",
                      html_text)

    def test_an_unrecognised_environment_gets_the_neutral_colour_not_an_invented_one(self):
        html_text = page(machine(env="staging-2"))
        self.assertIn("background:#5b6472'>staging-2<", html_text)

    def test_closure_is_twelve_characters_with_the_whole_store_name_on_hover(self):
        html_text = page(machine())
        self.assertIn(f"title='{CLOSURE}'>6idh361s36gw", html_text)

    def test_a_booted_closure_that_differs_is_still_shown(self):
        html_text = page(machine(booted="aaaaaaaaaaaabbbb2p0v4x8n1m7c3jt5-nixos-system-mongo-1-26.05"))
        self.assertIn("booted aaaaaaaaaaaa", html_text)

    def test_auto_apply_is_a_badge_and_a_last_pass_line(self):
        html_text = page(machine(auto_apply="applied"))
        self.assertIn(">on</span><span class=hint>last pass 10m ago</span>", html_text)
        # The applier's own verdict is not thrown away just because the badge does not say it.
        self.assertIn("Last verdict: applied.", html_text)

    def test_dry_run_is_not_reported_as_auto_apply_being_on(self):
        html_text = page(machine(auto_apply="blocked", blocked_reason="dry_run"))
        self.assertIn(">dry-run</span>", html_text)
        self.assertNotIn(">on</span>", html_text)

    def test_a_host_excluded_on_purpose_is_not_a_host_nobody_wired_up(self):
        excluded = page(machine(excluded_reason="k3s drains its own workloads"))
        self.assertIn(">excluded</span>", excluded)
        self.assertIn("k3s drains its own workloads", excluded)
        self.assertIn(">not covered</span>", page(machine(apply_covered=False)))


class RevisionFoldedIntoState(unittest.TestCase):
    """The `revision` column is gone; nothing it said may have gone with it."""

    def test_there_is_no_revision_column(self):
        self.assertNotIn("revision", headers(page(machine())))

    def test_the_revision_and_its_distance_from_main_are_under_the_state_badge(self):
        html_text = page(machine(revision_short="aaa92e6e2", behind=0))
        self.assertIn("<span class=sha>aaa92e6e2</span> on main", html_text)

    def test_being_behind_main_says_how_far(self):
        self.assertIn("</span> 3 behind main", page(machine(behind=3)))

    def test_an_unmeasurable_distance_is_not_rendered_as_being_on_main(self):
        html_text = page(machine(behind=None))
        self.assertIn("distance from main unknown", html_text)
        self.assertNotIn("on main", html_text)

    def test_a_closure_with_no_commit_says_so_rather_than_nothing(self):
        self.assertIn("unmeasurable", page(machine(revision_short="", revision="")))

    def test_a_staged_revision_is_still_named(self):
        html_text = page(machine(staged_short="9446b50f", state="staged, not activated",
                                 state_key="staged", severity="warn"))
        self.assertIn("staged <span class=sha>9446b50f</span>", html_text)

    def test_built_dirty_survives_as_a_state_badge(self):
        html_text = page(machine(dirty=True, state="built dirty", state_key="dirty",
                                 severity="warn"))
        self.assertIn(">built dirty</span>", html_text)


class SilentHost(unittest.TestCase):
    def test_a_host_that_reports_nothing_renders_dashes_not_blanks(self):
        html_text = page(machine(reporting=False, closure="", booted="", nixpkgs="",
                                 revision="", revision_short="", staged_revision="",
                                 staged_short="", auto_apply="", apply_covered=False,
                                 last_verdict=0, state="not reporting",
                                 state_key="notreporting", severity="alarm",
                                 detail="publishes no nixos_* metrics"))
        self.assertEqual(html_text.count("<td class=none>&mdash;</td>"), 3)
        self.assertIn("what it runs is UNKNOWN, which is not the same as behind", html_text)

    def test_unknown_is_never_phrased_as_a_distance_from_main(self):
        html_text = page(machine(reporting=False, revision_short="", staged_short="",
                                 apply_covered=False, state="not reporting",
                                 state_key="notreporting", severity="alarm"))
        self.assertNotIn("behind main", html_text)


class ActionRow(unittest.TestCase):
    def test_the_console_row_spans_exactly_the_table(self):
        html_text = page(machine(actionable=True, state="staged, not activated",
                                 state_key="staged", severity="warn"))
        self.assertEqual(app.FLEET_COLUMNS, len(headers(html_text)))
        self.assertIn(f"colspan='{len(headers(html_text))}'", html_text)


class Escaping(unittest.TestCase):
    def test_a_hostile_label_cannot_close_the_cell_it_is_rendered_into(self):
        html_text = page(machine(role="<script>x</script>", excluded_reason="a'b"))
        self.assertNotIn("<script>x</script>", html_text)
        self.assertIn("&lt;script&gt;", html_text)
        self.assertIn("a&#x27;b", html_text)


if __name__ == "__main__":
    unittest.main(verbosity=2)
