#!/usr/bin/env python3
"""What the fleet table promises, checked without a flake, a Prometheus or a host.

`render` is a pure function of the dict `build` returns, which is the whole reason this file can
exist: every branch that decides what a machine's row SAYS is reachable from a literal, so the
cases worth pinning are the ones nobody can produce on demand on a three-machine fleet -- a host
that has gone silent, one built from a dirty tree, one whose auto-apply is refusing a change.

Run:  python3 infra/version-dashboard/test_app.py
"""
import importlib.util
import os
import pathlib
import re
import socket
import ssl
import subprocess
import sys
import tempfile
import threading
import time
import unittest
import urllib.error

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


class BulkButton(unittest.TestCase):
    """The fleet-wide "Bring all to latest… (N)" button."""

    def test_no_button_when_nothing_is_staged_anywhere(self):
        # The id also appears in the SCRIPT, which always ships; it is the BUTTON that must not.
        self.assertNotIn("id=fleetbulkbtn", page(machine(), machine(name="monitoring-1")))

    def test_the_count_is_the_number_of_machines_with_something_staged(self):
        html_text = page(machine(name="mongo-1", actionable=True),
                         machine(name="monitoring-1", actionable=True),
                         machine(name="k3s-worker-1"))
        self.assertIn("Bring all to latest&hellip; (2)</button>", html_text)

    def test_the_count_can_never_disagree_with_the_buttons_below_it(self):
        # BOTH READ THE SAME `actionable` FLAG. A second, independently-derived count is exactly
        # how the header comes to promise a fleet needs three switches while the table offers two.
        rows = [machine(name=f"h{i}", actionable=i % 2 == 0) for i in range(6)]
        html_text = page(*rows)
        self.assertIn("Bring all to latest&hellip; (3)</button>", html_text)
        self.assertEqual(html_text.count("Bring to latest&hellip;</button>"), 3)

    def test_the_bulk_run_has_a_console_of_its_own(self):
        html_text = page(machine(actionable=True))
        self.assertIn("<details id=fleetbulkcons", html_text)


class PerMachineButton(unittest.TestCase):
    def test_it_is_named_the_same_as_on_the_sibling_dashboard(self):
        html_text = page(machine(actionable=True))
        self.assertIn("Bring to latest&hellip;</button>", html_text)
        # The words survive in the SECOND press's confirm() dialog, which is the reference's
        # wording too; it is the BUTTON that must not still be called that.
        self.assertNotIn(">Activate the staged closure", html_text)

    def test_the_cell_carries_what_the_bulk_run_selects_on(self):
        # `bringAllToLatest` picks its machines out of the DOM by these attributes; a row missing
        # one is a machine the bulk run silently skips.
        html_text = page(machine(actionable=True, public="1.2.3.4", env="prod"))
        self.assertIn("data-machine='mongo-1'", html_text)
        self.assertIn("data-address='1.2.3.4'", html_text)
        self.assertIn("data-env='prod'", html_text)

    def test_the_production_database_is_still_marked_dangerous(self):
        html_text = page(machine(name="mongo-1", role="mongo", actionable=True))
        self.assertIn("data-danger='1'", html_text)
        self.assertIn("production database", html_text)

    def test_an_ordinary_host_is_not(self):
        self.assertIn("data-danger=''", page(machine(role="k3s-worker", actionable=True)))


class OneJobPerMachine(unittest.TestCase):
    """The concurrency rule the bulk run depends on: per machine, not fleet-wide.

    A fleet-wide slot makes "bring all to latest" strictly serial, which is the one thing the bulk
    button exists not to be."""

    def setUp(self):
        self.started = []
        app._apply_jobs.clear()
        app._apply_seq[0] = 0
        self._worker = app._apply_worker
        app._apply_worker = lambda *a, **k: self.started.append(a[0])
        app._cache["data"] = {"rows": [
            machine(name="mongo-1", role="mongo", public="1.1.1.1", actionable=True),
            machine(name="monitoring-1", role="monitoring", public="2.2.2.2", actionable=True),
        ]}

    def tearDown(self):
        app._apply_worker = self._worker
        app._apply_jobs.clear()
        app._cache["data"] = None

    def test_two_machines_can_be_checked_at_the_same_time(self):
        first, code_a = app.handle_fleet_apply({"machine": "mongo-1", "phase": "check"})
        second, code_b = app.handle_fleet_apply({"machine": "monitoring-1", "phase": "check"})
        self.assertEqual((code_a, code_b), (200, 200))
        self.assertNotEqual(first["job"], second["job"])

    def test_the_same_machine_cannot_be_checked_twice_at_once(self):
        app.handle_fleet_apply({"machine": "mongo-1", "phase": "check"})
        payload, code = app.handle_fleet_apply({"machine": "mongo-1", "phase": "check"})
        self.assertEqual(code, 409)
        self.assertIn("already running against mongo-1", payload["error"])

    def test_a_finished_job_frees_its_machine(self):
        first, _ = app.handle_fleet_apply({"machine": "mongo-1", "phase": "check"})
        app._apply_jobs[first["job"]]["done"] = True
        _, code = app.handle_fleet_apply({"machine": "mongo-1", "phase": "check"})
        self.assertEqual(code, 200)

    def test_a_bulk_run_still_cannot_switch_the_database_without_the_typed_name(self):
        # THE GUARD THE BULK RUN MUST NOT BE A WAY AROUND. It posts the same endpoint as the single
        # button, so the server-side check is what makes collecting the confirmation up front a
        # courtesy rather than the only thing standing there.
        closure = "/nix/store/" + "a" * 32 + "-nixos-system-mongo-1"
        started, _ = app.handle_fleet_apply({"machine": "mongo-1", "phase": "check"})
        app._apply_jobs[started["job"]].update(done=True, can_switch=closure)
        payload, code = app.handle_fleet_apply(
            {"machine": "mongo-1", "phase": "switch", "closure": closure})
        self.assertEqual(code, 400)
        self.assertIn("typed back as confirmation", payload["error"])
        _, ok = app.handle_fleet_apply({"machine": "mongo-1", "phase": "switch",
                                        "closure": closure, "confirm": "mongo-1"})
        self.assertEqual(ok, 200)

    def test_an_ordinary_machine_needs_no_typed_name(self):
        closure = "/nix/store/" + "b" * 32 + "-nixos-system-monitoring-1"
        started, _ = app.handle_fleet_apply({"machine": "monitoring-1", "phase": "check"})
        app._apply_jobs[started["job"]].update(done=True, can_switch=closure)
        _, code = app.handle_fleet_apply({"machine": "monitoring-1", "phase": "switch",
                                          "closure": closure})
        self.assertEqual(code, 200)


class OneRowBuilder(unittest.TestCase):
    """/fleet-apply/machine splices a row into a live table, so it must build it the same way."""

    def test_the_endpoint_and_the_page_render_the_same_markup(self):
        row = machine(actionable=True)
        self.assertIn(app.machine_rows(row), page(row))

    def test_an_unknown_machine_is_an_error_not_an_empty_row(self):
        app._cache["data"] = {"rows": []}
        try:
            self.assertIn("error", app.fleet_machine_reading("nope"))
        finally:
            app._cache["data"] = None


class RosterIsReadOnlyWhenTheFlakeChanges(unittest.TestCase):
    """The roster half of a build is a `nix eval` over three whole NixOS configurations -- 86
    seconds on an idle laptop, measured, almost none of it CPU. Running it on every 30-second build
    meant one was always in flight, each queueing behind the previous one's git-fetch and
    eval-cache locks until every one of them hit the 240s timeout and the page announced
    `roster unavailable -- these counts are not a picture of the fleet` while all three machines
    were healthy. These pin what stopped that: an unchanged flake is not read again, and a read
    that fails is neither retried on the next build nor allowed to empty the table."""

    CLEAN = {"fingerprint": None, "machines": None, "evaluated_at": 0.0,
             "error": None, "failed_at": 0.0, "recalled": True, "evaluating": False}

    def setUp(self):
        self.reads = []
        self.store = tempfile.TemporaryDirectory()
        self._cache_path = app.ROSTER_CACHE
        app.ROSTER_CACHE = os.path.join(self.store.name, "roster.json")
        self.answer = ({"mongo-1": {"hostName": "mongo-1", "privateAddress": "10.20.0.13"}}, None)
        self.fingerprint = "flake-as-committed"
        self._machines, self._stamp = app.flake_machines, app.flake_fingerprint
        self._refresh = app._refresh_in_background
        app.flake_machines = self._read
        app.flake_fingerprint = lambda *a, **k: self.fingerprint
        # A re-read runs on its own thread in production. Run it straight through here, so that what
        # is being asserted is WHETHER one happened, not whether it happened in time.
        app._refresh_in_background = app._evaluate_roster
        app._roster.update(self.CLEAN)

    def tearDown(self):
        # WAIT FOR THE ONE TEST THAT USES A REAL THREAD BEFORE PUTTING ANYTHING BACK. A refresh
        # still running when `ROSTER_CACHE` is restored writes its fixture -- a fleet of one
        # invented host -- straight into the operator's own cache file, and the dashboard reads that
        # at its next start. That is not hypothetical: it happened, and the live page came back
        # reading "1 declared host(s)" until the next evaluation replaced it.
        self._settle()
        app.flake_machines, app.flake_fingerprint = self._machines, self._stamp
        app._refresh_in_background = self._refresh
        app.ROSTER_CACHE = self._cache_path
        self.store.cleanup()
        app._roster.update(self.CLEAN)

    @staticmethod
    def _settle(seconds=5):
        deadline = time.time() + seconds
        while app._roster["evaluating"] and time.time() < deadline:
            time.sleep(0.01)

    def _read(self):
        self.reads.append(self.fingerprint)
        return self.answer

    def test_an_unchanged_flake_is_read_once_however_many_builds_run(self):
        for _ in range(3):
            machines, err = app.roster()
            self.assertIsNone(err)
            self.assertEqual(list(machines), ["mongo-1"])
        self.assertEqual(len(self.reads), 1)

    def test_an_edited_flake_is_read_again(self):
        app.roster()
        self.fingerprint = "flake-with-a-fourth-host"
        app.roster()
        self.assertEqual(len(self.reads), 2)

    def test_a_failed_read_keeps_the_machines_the_last_one_returned(self):
        app.roster()
        self.fingerprint, self.answer = "edited", ({}, "nix eval failed: timed out after 900s")
        app.roster()                       # starts the re-read, which fails
        machines, err = app.roster()       # the next build, once it has
        self.assertEqual(list(machines), ["mongo-1"])
        self.assertIn("timed out after 900s", err)
        self.assertIn("last one that evaluated", err)

    def test_a_failed_read_is_not_repeated_on_the_next_build(self):
        self.answer = ({}, "nix eval failed: timed out after 240s")
        first, err = app.roster()
        second, err_again = app.roster()
        self.assertEqual(len(self.reads), 1)
        self.assertEqual((first, second), ({}, {}))
        self.assertEqual(err, err_again)

    def test_the_retry_floor_does_expire(self):
        self.answer = ({}, "nix eval failed: timed out after 240s")
        app.roster()
        app._roster["failed_at"] = time.time() - app.ROSTER_RETRY - 1
        self.answer = ({"mongo-1": {"privateAddress": "10.20.0.13"}}, None)
        machines, err = app.roster()
        self.assertEqual((len(self.reads), list(machines), err), (2, ["mongo-1"], None))

    def test_a_roster_that_never_loaded_reports_the_error_and_no_machines(self):
        # The state the page was stuck in. It must still say why rather than show an empty fleet.
        self.answer = ({}, "nix eval failed: timed out after 240s")
        machines, err = app.roster()
        self.assertEqual(machines, {})
        self.assertEqual(err, "nix eval failed: timed out after 240s")

    def test_the_roster_outlives_the_process_that_read_it(self):
        # The restart case, which is most of them: launchd's KeepAlive, a laptop rebooting, an edit
        # to this file. A restarted process that has to evaluate before it can show anything is a
        # page with no machines on it for 86 seconds -- and for the whole retry floor if that one
        # evaluation times out, which is exactly what a busy laptop does to it.
        app.roster()
        app._roster.update(self.CLEAN)  # as if this were a new process
        app._roster["recalled"] = False
        machines, err = app.roster()
        self.assertEqual((list(machines), err, len(self.reads)), (["mongo-1"], None, 1))

    def test_a_remembered_roster_is_re_read_when_the_flake_has_moved_on_since(self):
        app.roster()
        app._roster.update(self.CLEAN)
        app._roster["recalled"] = False
        self.fingerprint = "edited-while-the-dashboard-was-down"
        app.roster()
        self.assertEqual(len(self.reads), 2)

    def test_an_unreadable_cache_file_is_not_an_error(self):
        with open(app.ROSTER_CACHE, "w") as fh:
            fh.write("{not json")
        app._roster["recalled"] = False
        machines, err = app.roster()
        self.assertEqual((list(machines), err), (["mongo-1"], None))

    def test_a_re_read_does_not_hold_up_the_page(self):
        # The freeze this is here to prevent: the roster evaluation used to run inside the build, so
        # a slow one stopped the Prometheus half from being refreshed too -- the page went stale as
        # a whole and said so ("the last rebuild did not finish") while nothing was wrong with the
        # fleet. Only the first read of all is worth waiting for.
        app._refresh_in_background = self._refresh  # a real thread, as in production
        app.roster()
        started = threading.Event()
        release = threading.Event()

        def slow():
            started.set()
            release.wait(5)
            return self.answer

        app.flake_machines = slow
        self.fingerprint = "edited"
        began = time.time()
        machines, err = app.roster()
        took = time.time() - began
        self.assertTrue(started.wait(5), "the re-read never started")
        self.assertLess(took, 1.0, f"the caller waited {took:.1f}s for a re-read")
        self.assertEqual(list(machines), ["mongo-1"])
        self.assertIn("pending", err)
        release.set()
        self._settle()
        self.assertEqual(app._roster["fingerprint"], "edited", "the re-read never landed")

    def test_only_one_re_read_runs_at_a_time(self):
        app.roster()
        self.fingerprint = "edited"
        app._refresh_in_background = lambda fingerprint: None  # as if a thread were still running
        app.roster()
        app.roster()
        self.assertEqual(len(self.reads), 1)


class FlakeFingerprint(unittest.TestCase):
    """What counts as "the flake changed". It has to notice an edit to anything the evaluation
    reads, and -- the reason it is not simply the repository's HEAD -- ignore the ~18k application
    files that share this checkout with infra/."""

    def _flake(self, root):
        os.makedirs(os.path.join(root, "nix", "hosts", "mongo-1"))
        os.makedirs(os.path.join(root, "web", "src"))
        for rel, text in [("flake.nix", "{ outputs = _: {}; }"),
                          ("flake.lock", "{}"),
                          ("nix/hosts/mongo-1/default.nix", "{ fleet.role = \"mongo\"; }"),
                          ("web/src/Application.scala", "object Application")]:
            with open(os.path.join(root, rel), "w") as fh:
                fh.write(text)

    def test_an_edit_to_a_host_changes_the_stamp(self):
        with tempfile.TemporaryDirectory() as root:
            self._flake(root)
            before = app.flake_fingerprint(root)
            self.assertEqual(before, app.flake_fingerprint(root))
            host = os.path.join(root, "nix", "hosts", "mongo-1", "default.nix")
            with open(host, "w") as fh:
                fh.write("{ fleet.role = \"mongo\"; fleet.environment = \"prod\"; }")
            os.utime(host, (time.time() + 10, time.time() + 10))
            self.assertNotEqual(before, app.flake_fingerprint(root))

    def test_a_commit_to_the_application_sharing_the_checkout_does_not(self):
        with tempfile.TemporaryDirectory() as root:
            self._flake(root)
            before = app.flake_fingerprint(root)
            scala = os.path.join(root, "web", "src", "Application.scala")
            with open(scala, "w") as fh:
                fh.write("object Application { val changed = true }")
            os.utime(scala, (time.time() + 10, time.time() + 10))
            self.assertEqual(before, app.flake_fingerprint(root))


class _TempRepo:
    """A throwaway git repo, never this one -- release_commit_for/unreleased_commits shell out to
    real `git log`, and the point of these tests is the exact-match anchoring and the per-directory
    scoping, neither of which this repo's own history can be relied on to exercise on demand."""

    def __init__(self):
        self.dir = tempfile.TemporaryDirectory()
        self.root = self.dir.name
        self._git("init", "-q")
        self._git("config", "user.email", "t@t")
        self._git("config", "user.name", "t")

    def _git(self, *args, capture=False):
        result = subprocess.run(["git", *args], cwd=self.root, check=True,
                                 capture_output=capture, text=capture)
        return result.stdout.strip() if capture else None

    def commit(self, path, message):
        full = os.path.join(self.root, path)
        os.makedirs(os.path.dirname(full), exist_ok=True)
        with open(full, "a") as fh:
            fh.write("x")
        self._git("add", path)
        self._git("-c", "commit.gpgsign=false", "commit", "-q", "-m", message)
        return self._git("rev-parse", "HEAD", capture=True)

    def cleanup(self):
        self.dir.cleanup()

    def tag(self, name, sha):
        self._git("tag", "-f", name, sha)

    def current_branch(self):
        return self._git("rev-parse", "--abbrev-ref", "HEAD", capture=True)

    def checkout_new_branch(self, name):
        self._git("checkout", "-q", "-b", name)

    def checkout(self, ref):
        self._git("checkout", "-q", ref)


class MobileReleaseBaseline(unittest.TestCase):
    """release_commit_for / unreleased_commits -- the git half of the mobile-releases page. What's
    worth pinning is the exact-match anchoring: a loose match would credit '2.0.17' or 'Release
    mobile 2.0.7 hotfix' as the commit that shipped 2.0.7, which it did not, and the whole page's
    baseline would be silently wrong."""

    def setUp(self):
        self.repo = _TempRepo()
        self._orig_root = app.ROOT_DIR
        app.ROOT_DIR = self.repo.root

    def tearDown(self):
        app.ROOT_DIR = self._orig_root
        self.repo.cleanup()

    def test_finds_the_exact_release_commit(self):
        self.repo.commit("README.md", "init")
        target = self.repo.commit("ios/a.swift", "Release mobile 2.0.7")
        self.repo.commit("ios/b.swift", "unrelated")
        self.assertEqual(app.release_commit_for("2.0.7"), target)

    def test_does_not_match_a_longer_version_or_a_trailing_suffix(self):
        self.repo.commit("README.md", "init")
        self.repo.commit("ios/a.swift", "Release mobile 2.0.17")
        self.repo.commit("ios/b.swift", "Release mobile 2.0.7 hotfix")
        self.assertIsNone(app.release_commit_for("2.0.7"))

    def test_a_missing_version_looks_up_nothing(self):
        self.assertIsNone(app.release_commit_for(None))

    def test_a_mobile_platform_tag_wins_over_the_bump_commit(self):
        # Mirrors 2.0.8: the bump commit alone never produced a working upload -- a compile-bug
        # fix landed one commit later, and THAT is what actually got archived and uploaded. The
        # tag `ios-release.sh` pushes at upload time must be what the dashboard trusts, not the
        # "Release mobile" commit message, or the fix commit shows up as "not yet released"
        # forever even though it has been live since the day it was made.
        self.repo.commit("README.md", "init")
        self.repo.commit("ios/a.swift", "Release mobile 2.0.8")
        real_build = self.repo.commit("ios/b.swift", "Fix ambiguous LocalizedStringKey.init")
        self.repo.tag("mobile-ios-2.0.8", real_build)
        self.assertEqual(app.release_commit_for("2.0.8", "ios"), real_build)

    def test_no_tag_falls_back_to_the_bump_commit(self):
        # Versions released before this tagging scheme existed have no tag at all -- the page
        # must not go blank for every historical release, so it falls back to the old anchor.
        self.repo.commit("README.md", "init")
        target = self.repo.commit("ios/a.swift", "Release mobile 2.0.7")
        self.assertEqual(app.release_commit_for("2.0.7", "ios"), target)

    def test_a_tag_off_head_falls_back_to_the_bump_commit(self):
        # A release built from a worktree branch that never got merged back to what this
        # checkout runs from -- tag-mobile-release.sh pushes the tag regardless, so trusting it
        # here would hand unreleased_commits()'s `{sha}..HEAD` a baseline outside HEAD's own
        # history, which git log answers with a misleading commit list instead of an error.
        self.repo.commit("README.md", "init")
        bump = self.repo.commit("ios/a.swift", "Release mobile 2.0.9")
        main_branch = self.repo.current_branch()
        self.repo.checkout_new_branch("never-merged")
        orphan_build = self.repo.commit("ios/b.swift", "built and tagged, never merged back")
        self.repo.checkout(main_branch)
        self.repo.tag("mobile-ios-2.0.9", orphan_build)
        self.assertEqual(app.release_commit_for("2.0.9", "ios"), bump)

    def test_a_tag_for_the_other_platform_is_not_matched(self):
        # `mobile-ios-2.0.8` must not satisfy an Android lookup for the same version -- the two
        # platforms ship from different commits whenever they don't release in lockstep.
        self.repo.commit("README.md", "init")
        bump = self.repo.commit("ios/a.swift", "Release mobile 2.0.8")
        ios_build = self.repo.commit("ios/b.swift", "ios-only fix")
        self.repo.tag("mobile-ios-2.0.8", ios_build)
        self.assertEqual(app.release_commit_for("2.0.8", "android"), bump)

    def test_unreleased_commits_scoped_to_the_platform_directory(self):
        base = self.repo.commit("README.md", "Release mobile 1.0.0")
        self.repo.commit("web/x.scala", "web-only change")
        ios_commit = self.repo.commit("ios/a.swift", "ios change")
        self.assertEqual([c["sha"] for c in app.unreleased_commits(base, "ios")], [ios_commit])
        self.assertEqual(app.unreleased_commits(base, "android"), [])

    def test_up_to_date_is_an_empty_list_not_none(self):
        base = self.repo.commit("README.md", "Release mobile 1.0.0")
        self.assertEqual(app.unreleased_commits(base, "ios"), [])


class MobileBuildAssembly(unittest.TestCase):
    """build_mobile()'s only real job is wiring each store's own live_version to ITS OWN baseline
    commit and diffing ITS OWN directory from there. That independence is the whole point of the
    page -- see the module-level comment above build_mobile -- so the case worth pinning is a
    repo where iOS has shipped past a point Android has not: a shared baseline would wrongly call
    Android's still-unreleased changes released the moment iOS alone moved past them."""

    def setUp(self):
        self.repo = _TempRepo()
        self._orig_root = app.ROOT_DIR
        app.ROOT_DIR = self.repo.root
        self._orig_ios, self._orig_android = app.ios_release_state, app.android_release_state

    def tearDown(self):
        app.ROOT_DIR = self._orig_root
        app.ios_release_state, app.android_release_state = self._orig_ios, self._orig_android
        self.repo.cleanup()

    @staticmethod
    def _by_name(data):
        return {p["name"]: p for p in data["platforms"]}

    def test_each_platform_diffs_from_its_own_release_not_the_others(self):
        self.repo.commit("README.md", "init")
        self.repo.commit("ios/a.swift", "Release mobile 2.0.6")
        self.repo.commit("android/a.kt", "Release mobile 2.0.6")
        self.repo.commit("ios/b.swift", "Release mobile 2.0.7")  # iOS-only bump, like the real one
        self.repo.commit("ios/c.swift", "ios-only follow-up")
        self.repo.commit("android/b.kt", "android-only follow-up")

        app.ios_release_state = lambda: {"error": None, "live_version": "2.0.7",
                                          "live_extra": "READY_FOR_SALE", "pending": None}
        app.android_release_state = lambda: {"error": None, "live_version": "2.0.6",
                                              "live_extra": "309", "pending": None}
        by_name = self._by_name(app.build_mobile())
        self.assertEqual([c["subject"] for c in by_name["iOS"]["commits"]], ["ios-only follow-up"])
        self.assertEqual([c["subject"] for c in by_name["Android"]["commits"]],
                         ["android-only follow-up"])

    def test_a_fetch_error_is_reported_without_touching_git(self):
        app.ios_release_state = lambda: {"error": "HTTPError 401: unauthorized"}
        app.android_release_state = lambda: {"error": None, "live_version": None,
                                              "live_extra": None, "pending": None}
        by_name = self._by_name(app.build_mobile())
        self.assertEqual(by_name["iOS"], {"name": "iOS", "fetch_failed": True,
                                          "error": "HTTPError 401: unauthorized",
                                          "network_error": False})

    def test_a_version_never_released_is_not_a_fetch_error(self):
        # Distinct states that the earlier shape of this code conflated: both left `error` set and
        # `live_version` falsy, so render_mobile could not tell a real 401 apart from an app that
        # has simply never shipped. `fetch_failed` is what keeps them apart now.
        app.ios_release_state = lambda: {"error": None, "live_version": None,
                                          "live_extra": None, "pending": None}
        app.android_release_state = lambda: {"error": None, "live_version": None,
                                              "live_extra": None, "pending": None}
        by_name = self._by_name(app.build_mobile())
        self.assertFalse(by_name["iOS"]["fetch_failed"])
        self.assertEqual(by_name["iOS"]["error"], "never released to this store yet")

    def test_no_matching_release_commit_is_a_visible_error_not_a_crash(self):
        app.ios_release_state = lambda: {"error": None, "live_version": "9.9.9",
                                          "live_extra": "READY_FOR_SALE", "pending": None}
        app.android_release_state = lambda: {"error": None, "live_version": None,
                                              "live_extra": None, "pending": None}
        by_name = self._by_name(app.build_mobile())
        self.assertIn("9.9.9", by_name["iOS"]["error"])
        self.assertIsNone(by_name["iOS"]["commits"])

    def test_an_unreachable_tag_names_itself_in_the_error_not_a_generic_message(self):
        # Same shape as MobileReleaseBaseline's own unreachable-tag test, but checking what the
        # PAGE says about it -- an operator staring at "no commit found matching 'Release mobile
        # X'" would reasonably conclude nothing was ever tagged, when actually a tag exists and
        # was correctly rejected. The error should say which of those happened.
        self.repo.commit("README.md", "init")
        main_branch = self.repo.current_branch()
        self.repo.checkout_new_branch("never-merged")
        orphan_build = self.repo.commit("ios/a.swift", "built and tagged, never merged back")
        self.repo.checkout(main_branch)
        self.repo.tag("mobile-ios-9.9.9", orphan_build)

        app.ios_release_state = lambda: {"error": None, "live_version": "9.9.9",
                                          "live_extra": "READY_FOR_SALE", "pending": None}
        app.android_release_state = lambda: {"error": None, "live_version": None,
                                              "live_extra": None, "pending": None}
        by_name = self._by_name(app.build_mobile())
        self.assertIn(orphan_build[:10], by_name["iOS"]["error"])
        self.assertIn("reachable", by_name["iOS"]["error"])
        self.assertIsNone(by_name["iOS"]["commits"])

    def test_the_unreachable_tag_lookup_only_shells_out_once(self):
        # build_mobile() needs the tag sha both to try it as a baseline and, on the error path,
        # to name it in the message -- release_commit_for's tag_sha parameter exists so the
        # second use doesn't re-run the same `git rev-parse` subprocess.
        self.repo.commit("README.md", "init")
        main_branch = self.repo.current_branch()
        self.repo.checkout_new_branch("never-merged")
        orphan_build = self.repo.commit("ios/a.swift", "built and tagged, never merged back")
        self.repo.checkout(main_branch)
        self.repo.tag("mobile-ios-9.9.9", orphan_build)

        app.ios_release_state = lambda: {"error": None, "live_version": "9.9.9",
                                          "live_extra": "READY_FOR_SALE", "pending": None}
        app.android_release_state = lambda: {"error": None, "live_version": None,
                                              "live_extra": None, "pending": None}
        calls = []
        real_mobile_tag_sha = app.mobile_tag_sha
        app.mobile_tag_sha = lambda *args: (calls.append(args), real_mobile_tag_sha(*args))[1]
        try:
            app.build_mobile()
        finally:
            app.mobile_tag_sha = real_mobile_tag_sha
        # Android's own (unrelated) lookup with a None version is fine and expected -- what
        # this pins is that the iOS lookup, used twice (as a baseline, then in the error
        # message), runs its subprocess only once.
        self.assertEqual(calls.count(("ios", "9.9.9")), 1)

    def test_the_no_tag_at_all_lookup_also_only_shells_out_once(self):
        # The sibling of the test above, for the OTHER path through the same error branch:
        # no tag exists at all, so mobile_tag_sha's own result is None -- and build_mobile
        # passes that None through to release_commit_for as tag_sha=None explicitly. A
        # plain `tag_sha=None` default on release_commit_for (rather than the _UNSET
        # sentinel it actually uses) could not tell "caller passed None on purpose" apart
        # from "caller didn't pass anything", so it would re-derive the lookup right here --
        # this is the case that would have silently regressed without the sentinel.
        self.repo.commit("README.md", "Release mobile 1.0.0")  # deliberately NOT "9.9.9"

        app.ios_release_state = lambda: {"error": None, "live_version": "9.9.9",
                                          "live_extra": "READY_FOR_SALE", "pending": None}
        app.android_release_state = lambda: {"error": None, "live_version": None,
                                              "live_extra": None, "pending": None}
        calls = []
        real_mobile_tag_sha = app.mobile_tag_sha
        app.mobile_tag_sha = lambda *args: (calls.append(args), real_mobile_tag_sha(*args))[1]
        try:
            by_name = self._by_name(app.build_mobile())
        finally:
            app.mobile_tag_sha = real_mobile_tag_sha
        self.assertEqual(calls.count(("ios", "9.9.9")), 1)
        self.assertEqual(by_name["iOS"]["error"], "no commit found matching 'Release mobile 9.9.9'")


class TransientNetworkErrorClassification(unittest.TestCase):
    """_is_transient_network_error draws the line _with_network_retries, MOBILE_RETRY_FLOOR and
    MOBILE_SELF_RESTART_AFTER all key off of: DNS/connection/timeout failures the OS raised before
    any server answered, versus the server answering with a status the same request will just get
    again immediately (401, 429, a real outage)."""

    def test_dns_failure_is_transient(self):
        exc = urllib.error.URLError(socket.gaierror(8, "nodename nor servname provided, or not known"))
        self.assertTrue(app._is_transient_network_error(exc))

    def test_bare_gaierror_is_transient(self):
        self.assertTrue(app._is_transient_network_error(socket.gaierror(8, "nodename nor servname")))

    def test_connection_and_timeout_errors_are_transient(self):
        self.assertTrue(app._is_transient_network_error(ConnectionRefusedError()))
        self.assertTrue(app._is_transient_network_error(socket.timeout()))
        self.assertTrue(app._is_transient_network_error(TimeoutError()))

    def test_an_http_error_is_not_transient_even_though_it_subclasses_urlerror(self):
        exc = urllib.error.HTTPError("https://api.appstoreconnect.apple.com/x", 401,
                                      "Unauthorized", {}, None)
        self.assertFalse(app._is_transient_network_error(exc))

    def test_an_unrelated_exception_is_not_transient(self):
        self.assertFalse(app._is_transient_network_error(KeyError("access_token")))
        self.assertFalse(app._is_transient_network_error(ValueError("bad json")))


class NetworkRetries(unittest.TestCase):
    """_with_network_retries is what turns a DNS blip that would otherwise sit as a red box for a
    whole MOBILE_CACHE_TTL into something that clears inside one page build."""

    def setUp(self):
        self._orig_sleep = app.time.sleep
        self.slept = []
        app.time.sleep = lambda s: self.slept.append(s)

    def tearDown(self):
        app.time.sleep = self._orig_sleep

    def test_succeeds_without_retrying_when_the_first_attempt_works(self):
        calls = []

        def fn():
            calls.append(1)
            return "ok"

        self.assertEqual(app._with_network_retries(fn), "ok")
        self.assertEqual(len(calls), 1)
        self.assertEqual(self.slept, [])

    def test_retries_a_transient_failure_and_returns_the_eventual_success(self):
        attempts = []

        def fn():
            attempts.append(1)
            if len(attempts) < 3:
                raise socket.gaierror(8, "nodename nor servname provided, or not known")
            return "ok"

        self.assertEqual(app._with_network_retries(fn), "ok")
        self.assertEqual(len(attempts), 3)
        self.assertEqual(self.slept, list(app.MOBILE_NETWORK_RETRY_DELAYS))

    def test_gives_up_after_exhausting_every_attempt(self):
        def fn():
            raise socket.gaierror(8, "nodename nor servname provided, or not known")

        with self.assertRaises(socket.gaierror):
            app._with_network_retries(fn)
        self.assertEqual(len(self.slept), app.MOBILE_NETWORK_RETRY_ATTEMPTS - 1)

    def test_an_http_error_propagates_on_the_first_attempt_without_retrying(self):
        calls = []

        def fn():
            calls.append(1)
            raise urllib.error.HTTPError("https://x", 401, "Unauthorized", {}, None)

        with self.assertRaises(urllib.error.HTTPError):
            app._with_network_retries(fn)
        self.assertEqual(len(calls), 1)
        self.assertEqual(self.slept, [])


class MobileNetworkFailureWiring(unittest.TestCase):
    """ios_release_state/android_release_state must both retry a transient failure through
    _with_network_retries and tag the resulting error as network_error, since that flag is what
    _all_platforms_network_failed (and through it, MOBILE_RETRY_FLOOR / MOBILE_SELF_RESTART_AFTER)
    reads to tell a DNS blip apart from a real 401."""

    def setUp(self):
        self._orig_sleep = app.time.sleep
        app.time.sleep = lambda s: None
        self._orig_asc_get = app._asc_get
        self._orig_play_access_token = app._play_access_token
        self._orig_play_post = app._play_post
        self._orig_play_get = app._play_get

    def tearDown(self):
        app.time.sleep = self._orig_sleep
        app._asc_get = self._orig_asc_get
        app._play_access_token = self._orig_play_access_token
        app._play_post = self._orig_play_post
        app._play_get = self._orig_play_get

    def test_ios_retries_a_dns_failure_and_recovers(self):
        calls = []

        def fake_asc_get(path):
            calls.append(path)
            if len(calls) < 2:
                raise socket.gaierror(8, "nodename nor servname provided, or not known")
            return {"data": [{"attributes": {"versionString": "2.0.7",
                                              "appStoreState": "READY_FOR_SALE",
                                              "createdDate": "2026-09-01"}}]}

        app._asc_get = fake_asc_get
        state = app.ios_release_state()
        self.assertEqual(len(calls), 2)
        self.assertEqual(state["live_version"], "2.0.7")

    def test_ios_a_persistent_dns_failure_is_reported_as_network_error(self):
        app._asc_get = lambda path: (_ for _ in ()).throw(
            socket.gaierror(8, "nodename nor servname provided, or not known"))
        state = app.ios_release_state()
        self.assertTrue(state["network_error"])
        self.assertIn("gaierror", state["error"])

    def test_ios_an_auth_failure_is_not_a_network_error_and_is_not_retried(self):
        calls = []

        def fake_asc_get(path):
            calls.append(path)
            raise urllib.error.HTTPError("https://x", 401, "Unauthorized", {}, None)

        app._asc_get = fake_asc_get
        state = app.ios_release_state()
        self.assertEqual(len(calls), 1)
        self.assertFalse(state["network_error"])

    def test_android_retries_across_the_whole_token_edit_track_sequence(self):
        calls = []

        def fake_token():
            calls.append("token")
            if len(calls) < 2:
                raise socket.gaierror(8, "nodename nor servname provided, or not known")
            return "tok"

        app._play_access_token = fake_token
        app._play_post = lambda path, token: {"id": "edit-1"}
        app._play_get = lambda path, token: {"releases": [
            {"status": "completed", "name": "2.0.6", "versionCodes": ["309"]}]}
        state = app.android_release_state()
        self.assertEqual(calls, ["token", "token"])
        self.assertEqual(state["live_version"], "2.0.6")

    def test_android_a_persistent_dns_failure_is_reported_as_network_error(self):
        app._play_access_token = lambda: (_ for _ in ()).throw(
            socket.gaierror(8, "nodename nor servname provided, or not known"))
        state = app.android_release_state()
        self.assertTrue(state["network_error"])


class TransientNetworkErrorClassification(unittest.TestCase):
    """A certificate or TLS-handshake refusal is a configuration fault, not a blip: retrying it
    seconds later gives the same answer, and counting it towards MOBILE_SELF_RESTART_AFTER would
    restart the process every few builds over something no fresh process fixes. A TLS connection
    the far end just DROPPED (an EOF mid-handshake) is still the network, though."""

    def test_a_certificate_failure_is_not_transient(self):
        cert = ssl.SSLCertVerificationError(1, "certificate verify failed: certificate has expired")
        self.assertFalse(app._is_transient_network_error(urllib.error.URLError(cert)))
        self.assertFalse(app._is_transient_network_error(cert))

    def test_a_tls_protocol_failure_is_not_transient(self):
        err = ssl.SSLError(1, "wrong version number")
        self.assertFalse(app._is_transient_network_error(urllib.error.URLError(err)))

    def test_a_tls_connection_dropped_mid_handshake_still_is(self):
        eof = ssl.SSLEOFError(8, "EOF occurred in violation of protocol")
        self.assertTrue(app._is_transient_network_error(urllib.error.URLError(eof)))

    def test_dns_and_connection_failures_still_are(self):
        self.assertTrue(app._is_transient_network_error(
            urllib.error.URLError(socket.gaierror(8, "nodename nor servname provided"))))
        self.assertTrue(app._is_transient_network_error(ConnectionResetError()))

    def test_a_local_os_error_outside_the_round_trip_is_not(self):
        self.assertFalse(app._is_transient_network_error(FileNotFoundError("AuthKey_X.p8")))


class MobileRetryFloorAndSelfHeal(unittest.TestCase):
    """The two backstops _all_platforms_network_failed feeds: cached_mobile treats an all-network
    failure as stale after MOBILE_RETRY_FLOOR rather than the full MOBILE_CACHE_TTL, and the
    process restarts itself once that failure has repeated MOBILE_SELF_RESTART_AFTER times in a
    row -- see 2026-09-22's incident note on those constants."""

    def setUp(self):
        self._orig_cache = dict(app._mobile_cache)
        self._orig_build_mobile = app.build_mobile
        self._orig_exit = app.os._exit
        self._orig_grace = app.MOBILE_SELF_RESTART_GRACE
        app.MOBILE_SELF_RESTART_GRACE = 0.2

    def tearDown(self):
        app.MOBILE_SELF_RESTART_GRACE = self._orig_grace
        app._mobile_cache.clear()
        app._mobile_cache.update(self._orig_cache)
        app.build_mobile = self._orig_build_mobile
        app.os._exit = self._orig_exit

    @staticmethod
    def _all_failed(error="URLError: <urlopen error ...>"):
        return {"built_at": time.time(), "took": 0.1, "platforms": [
            {"name": "iOS", "fetch_failed": True, "error": error, "network_error": True},
            {"name": "Android", "fetch_failed": True, "error": error, "network_error": True},
        ]}

    @staticmethod
    def _one_ok():
        return {"built_at": time.time(), "took": 0.1, "platforms": [
            {"name": "iOS", "fetch_failed": False, "live_version": "2.0.7", "live_extra": None,
             "pending": None, "baseline": "abc", "commits": [], "error": None},
            {"name": "Android", "fetch_failed": True,
             "error": "HTTPError 401: unauthorized", "network_error": False},
        ]}

    def test_all_platforms_network_failed_requires_every_platform_to_be_a_network_error(self):
        self.assertTrue(app._all_platforms_network_failed(self._all_failed()))
        self.assertFalse(app._all_platforms_network_failed(self._one_ok()))
        self.assertFalse(app._all_platforms_network_failed({"platforms": []}))

    def test_an_all_network_failure_is_stale_again_after_the_retry_floor_not_the_full_ttl(self):
        app._mobile_cache["data"] = self._all_failed()
        # Well past MOBILE_RETRY_FLOOR but nowhere near MOBILE_CACHE_TTL -- the case that used to
        # keep serving the stale error box for the rest of the full ten minutes.
        app._mobile_cache["built_at"] = time.time() - app.MOBILE_RETRY_FLOOR - 1
        calls = []
        fresh = self._one_ok()
        app.build_mobile = lambda: calls.append(1) or fresh
        data = app.cached_mobile()
        self.assertEqual(calls, [1])
        self.assertIs(data, fresh)

    def test_a_healthy_cache_is_not_rebuilt_before_the_full_ttl(self):
        app._mobile_cache["data"] = self._one_ok()
        app._mobile_cache["built_at"] = time.time() - app.MOBILE_RETRY_FLOOR - 1
        calls = []
        app.build_mobile = lambda: calls.append(1) or self._one_ok()
        app.cached_mobile()
        self.assertEqual(calls, [])  # still within MOBILE_CACHE_TTL, so no rebuild

    def test_consecutive_network_failures_reset_on_a_success(self):
        app._mobile_cache["consecutive_network_failures"] = app.MOBILE_SELF_RESTART_AFTER - 1
        app._mobile_cache["built_at"] = 0.0
        app.build_mobile = lambda: self._one_ok()
        app.cached_mobile()
        self.assertEqual(app._mobile_cache["consecutive_network_failures"], 0)

    def test_self_restarts_once_the_threshold_is_reached(self):
        exits = []
        app.os._exit = lambda code: exits.append(code)
        app._mobile_cache["consecutive_network_failures"] = app.MOBILE_SELF_RESTART_AFTER - 1
        app._mobile_cache["built_at"] = 0.0
        failed = self._all_failed()
        app.build_mobile = lambda: failed
        self.assertIs(app.cached_mobile(), failed)
        self._wait_for(lambda: exits)
        self.assertEqual(exits, [1])

    def test_the_self_restart_waits_for_the_triggering_response_to_be_sent(self):
        # The build that trips the threshold usually runs INSIDE a GET /mobile; exiting on the
        # spot dropped that visitor's connection. The exit is deferred by the grace instead, so
        # cached_mobile returns (and the handler writes its response) before the process goes.
        exits = []
        app.os._exit = lambda code: exits.append(code)
        app._mobile_cache["consecutive_network_failures"] = app.MOBILE_SELF_RESTART_AFTER - 1
        app._mobile_cache["built_at"] = 0.0
        app.build_mobile = lambda: self._all_failed()
        app.cached_mobile()
        self.assertEqual(exits, [], "exited before the caller got its data back")
        self._wait_for(lambda: exits)
        self.assertEqual(exits, [1])

    @staticmethod
    def _wait_for(condition, timeout=5.0):
        deadline = time.time() + timeout
        while not condition() and time.time() < deadline:
            time.sleep(0.01)

    def test_does_not_self_restart_before_the_threshold(self):
        exits = []
        app.os._exit = lambda code: exits.append(code)
        app._mobile_cache["consecutive_network_failures"] = 0
        app._mobile_cache["built_at"] = 0.0
        app.build_mobile = lambda: self._all_failed()
        app.cached_mobile()
        self.assertEqual(exits, [])

    def test_a_non_network_failure_never_advances_the_self_restart_counter(self):
        exits = []
        app.os._exit = lambda code: exits.append(code)
        app._mobile_cache["consecutive_network_failures"] = app.MOBILE_SELF_RESTART_AFTER - 1
        app._mobile_cache["built_at"] = 0.0
        app.build_mobile = lambda: self._one_ok()  # Android 401s -- not a network error
        app.cached_mobile()
        self.assertEqual(app._mobile_cache["consecutive_network_failures"], 0)
        self.assertEqual(exits, [])


class MobileRefreshPacing(unittest.TestCase):
    """_mobile_refresh_delay is what lets refresh_mobile_forever notice a cleared DNS blip within
    about a minute instead of the full 10-minute cadence."""

    def test_a_raised_exception_uses_the_retry_floor(self):
        self.assertEqual(app._mobile_refresh_delay(None), app.MOBILE_RETRY_FLOOR)

    def test_an_all_network_failure_uses_the_retry_floor(self):
        data = {"platforms": [{"fetch_failed": True, "network_error": True}]}
        self.assertEqual(app._mobile_refresh_delay(data), app.MOBILE_RETRY_FLOOR)

    def test_a_clean_build_uses_the_full_ttl(self):
        data = {"platforms": [{"fetch_failed": False}]}
        self.assertEqual(app._mobile_refresh_delay(data), app.MOBILE_CACHE_TTL)


class RenderMobile(unittest.TestCase):
    """render_mobile is a pure function of the dict build_mobile returns, same discipline as
    `render`/`page` above -- every branch is reachable from a literal dict without a network call
    or a git repo."""

    @staticmethod
    def _data(*platforms):
        return {"built_at": time.time(), "took": 0.8, "platforms": list(platforms)}

    def test_a_fetch_failure_is_an_err_box(self):
        out = app.render_mobile(self._data(
            {"name": "iOS", "fetch_failed": True, "error": "HTTPError 401: unauthorized"}))
        self.assertIn("err", out)
        self.assertIn("HTTPError 401: unauthorized", out)

    def test_up_to_date_says_so(self):
        out = app.render_mobile(self._data({
            "name": "iOS", "fetch_failed": False, "live_version": "2.0.7", "live_extra": None,
            "pending": None, "baseline": "abc1234", "commits": [], "error": None,
        }))
        self.assertIn("2.0.7", out)
        self.assertIn("up to date", out)

    def test_unreleased_commits_render_as_table_rows(self):
        out = app.render_mobile(self._data({
            "name": "Android", "fetch_failed": False, "live_version": "2.0.6", "live_extra": "309",
            "pending": None, "baseline": "abc1234", "error": None,
            "commits": [{"sha": "deadbeef" * 5, "short": "deadbee", "date": "2026-09-14",
                        "subject": "Flatten the city picker's search"}],
        }))
        self.assertIn("deadbee", out)
        self.assertIn("Flatten the city picker&#x27;s search", out)  # html-escaped
        self.assertIn("1 commit(s) not yet released", out)

    def test_a_pending_submission_is_shown_separately_from_live(self):
        out = app.render_mobile(self._data({
            "name": "iOS", "fetch_failed": False, "live_version": "2.0.6", "live_extra": None,
            "pending": {"version": "2.0.7", "state": "WAITING_FOR_REVIEW"},
            "baseline": "abc1234", "commits": [], "error": None,
        }))
        self.assertIn("2.0.6", out)
        self.assertIn("2.0.7", out)
        self.assertIn("WAITING_FOR_REVIEW", out)

    def test_never_released_is_a_note_not_an_err_box(self):
        out = app.render_mobile(self._data({
            "name": "Android", "fetch_failed": False, "live_version": None, "live_extra": None,
            "pending": None, "baseline": None, "commits": None,
            "error": "never released to this store yet",
        }))
        self.assertIn("note", out)
        self.assertNotIn("class='err'", out)

    def test_the_nav_bar_links_to_the_nixos_page(self):
        out = app.render_mobile(self._data())
        self.assertIn("href='/nixos'", out)


if __name__ == "__main__":
    unittest.main(verbosity=2)
