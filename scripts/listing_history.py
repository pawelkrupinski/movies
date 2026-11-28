#!/usr/bin/env python3
"""ONE LISTING'S IDENTITY TAKE OVER TIME (scripts/listing-history).

    listing_history.py <cc> --venue S --title S [--since 7d] [--replay [--recording RUN] [--replay-limit N]]

Prints, for the listings at venues whose name holds --venue and whose title holds --title (case and accents aside):

  * every take change production logged (VictoriaLogs, `identity take change:` lines — the JSON fields kind, basis,
    film_before/after, build, projection_id, reason; lines from before 2026-10-14 are text only and read from their
    message), GROUPED BY THE DEPLOY that logged them — the worker image tags Prometheus saw
    (`kube_pod_container_info`, `main-<stamp>-<sha>` → the commit and its subject), with the run of takes per listing
    and every deploy in which a listing changed more than once (a flip — per pod start, per projection mode);
  * the labels that judge each spelling it went by (scripts/label-lookup, prod's take left to the section below);
  * prod's CURRENT state from the LOCAL prod mirror (:28017, read-only), after its freshness and lag
    (scripts/local-mirror/freshness): the per-listing trace (identity_traces), the family's agreement verdicts
    (identity_model_families) and the venue's scraped listing (identity_listings);
  * with --replay: scripts/identity-slice at each deploy's commit that logged a change, all on ONE pinned recording
    (--recording, default the newest green one holding the country's corpus) — whether CI's view of the input reproduces each change. A change the
    replay does not reproduce at its commit is prod's state (filed answers, restored model, arrival order), not code.
    The mirror readable, a third view replays with prod's state NOW (identity-slice --prod-mirror), and each change is
    shown beside prod's reason for the listing now (its identity_traces).

Every outside call goes through one runner (ssh monitoring-1 for VictoriaLogs and Prometheus, mongosh on the local mirror,
git, identity-slice), so the tests answer them all. Tests: listing_history_test.py beside it.
"""
import argparse
import datetime
import json
import os
import re
import shlex
import subprocess
import sys
import unicodedata
from collections import defaultdict
from dataclasses import dataclass, field
from typing import Callable, Dict, List, Optional, Set, Tuple
REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(REPO, "scripts", "lib"))
sys.path.insert(0, os.path.join(REPO, "scripts"))
from processes import machine  # noqa: E402
from mongo import mongo_local  # noqa: E402
from listing_mirror import MIRROR_QUERY, State, mirror_lines, trace_reason  # noqa: E402

LOGS = "http://10.20.0.11:9428/select/logsql/query"
PROMETHEUS = "http://10.20.0.11:9090/api/v1/query_range"
MONITORING_HOST = machine.environment().get("LISTING_HISTORY_MONITORING_HOST", "monitoring-1")
MIRROR = "mongodb://127.0.0.1:{port}/{db}?directConnection=true&serverSelectionTimeoutMS=5000"
MIRROR_NEEDS = "identity_traces,identity_model_families,identity_listings"   # agreements and pins may hold nothing
LOG_LIMIT = 2000
# the identity-slice every replay runs: another checkout's when this one's tool files no longer compile at old refs
SLICE = machine.environment().get("LISTING_HISTORY_SLICE", os.path.join(REPO, "scripts", "identity-slice"))
TOOLS = os.path.join(REPO, "scripts", "identity-tools.sh")
DEPLOY_SLACK = 600   # a pod of the previous image logs on for a few minutes while the rollout passes


def fold(s):
    return "".join(c for c in unicodedata.normalize("NFKD", s or "") if not unicodedata.combining(c)).casefold()


def seconds(since):
    match = re.fullmatch(r"(\d+)([dhm])", since)
    if not match:
        raise ValueError(f"--since: Nd, Nh or Nm, not {since}")
    return int(match.group(1)) * {"d": 86400, "h": 3600, "m": 60}[match.group(2)]


def _ts(iso):
    text = iso.replace("Z", "")
    if "." in text:
        head, frac = text.split(".", 1)
        text = f"{head}.{frac[:6]}"
    return datetime.datetime.fromisoformat(text).replace(tzinfo=datetime.timezone.utc).timestamp()


def _clock(ts, with_seconds=True):
    return datetime.datetime.fromtimestamp(ts, datetime.timezone.utc).strftime("%m-%d %H:%M:%S" if with_seconds else "%m-%d %H:%M")


# ---------------------------------------------------------------------------------------------------------------------
# Take-change lines

@dataclass
class Change:
    time: float
    kind: str
    venue: str
    listing: str
    before: str
    after: str
    basis: str
    build: str = ""
    title_after: str = ""
    confidence: str = ""
    projection: str = ""
    mode: str = ""
    pod: str = ""
    reason: str = ""

    @property
    def key(self):
        return f"{self.venue} | {self.listing}"


TEXT = re.compile(r"identity take change: (lost|changed|gained) (.+?) \| (.+?): (none|tmdb \d+)(?: \((.*?)\))? -> "
                  r"(none|tmdb \d+)(?: \((.*?)\))?(?: \(([0-9a-f]+)\))?; reason (\w+)(?:: (.*))?$")


def parse_change(line):
    """A take-change log line (VictoriaLogs JSON): its fields when it has them, else its message (lines before 10-14)."""
    when = _ts(line["_time"])
    if line.get("kind") and line.get("venue"):
        tmdb = lambda side: f"tmdb {line[f'tmdb_{side}']}" if line.get(f"tmdb_{side}") else "none"
        return Change(when, line["kind"], line["venue"], line.get("listing", ""), tmdb("before"), tmdb("after"),
                      line.get("basis", ""), line.get("build", ""), line.get("title_after") or "", line.get("confidence") or "",
                      line.get("projection_id") or "", line.get("projection_mode") or "", line.get("pod") or "",
                      line.get("reason_first") or line.get("reason_last") or "")
    match = TEXT.search(line.get("_msg", ""))
    if not match:
        return None
    kind, venue, listing, before, _, after, title_after, _, basis, reason = match.groups()
    return Change(when, kind, venue, listing, before, after, basis, title_after=title_after or "", reason=reason or "",
                  pod=line.get("pod") or "")


def re2_escape(text):
    """A literal for Go's RE2 (VictoriaLogs, and Mongo's PCRE alike): its metacharacters escaped, nothing else —
    RE2 refuses an escaped space or letter, which Python's re.escape writes."""
    return re.sub(r"([\\.+*?()|\[\]{}^$])", r"\\\1", text)


def logsql(cc, venue, title, since):
    """Take-change lines of the country whose message names a venue and a listing holding the two substrings. A regex
    over the message, so the older text-only lines are found too; the caller keeps only the exact matches."""
    pattern = f"(?i)identity take change: (lost|changed|gained) [^|]*{re2_escape(venue)}[^|]* \\| .*{re2_escape(title)}"
    return (f'_time:{since} "identity take change" country:{cc} _msg:~{json.dumps(pattern, ensure_ascii=False)} | sort by (_time)')


# ---------------------------------------------------------------------------------------------------------------------
# Deploys

@dataclass
class Deploy:
    sha7: str
    tag: str
    first: float
    last: float
    sha: str = ""
    subject: str = ""
    changes: list = field(default_factory=list)


def deploys(prometheus_json):
    """The worker images Prometheus saw, in the order they started: `main-<stamp>-<sha7>` tags."""
    out = []
    for series in json.loads(prometheus_json).get("data", {}).get("result", []):
        tag = series["metric"].get("image", "").rsplit(":", 1)[-1]
        match = re.fullmatch(r"main-\d+-([0-9a-f]{7,})", tag)
        if not match or not series.get("values"):
            continue
        times = [float(t) for t, _ in series["values"]]
        out.append(Deploy(match.group(1)[:7], tag, min(times), max(times)))
    return sorted(out, key=lambda d: d.first)


def deploy_of(change, known):
    """The deploy that logged a change: its build when the line names one, else the deploy running at its time."""
    if change.build:
        for deploy in known:
            if change.build.startswith(deploy.sha7):
                return deploy
        deploy = Deploy(change.build[:7], "(not seen by Prometheus)", change.time, change.time)
        known.append(deploy)
        return deploy
    running = [d for d in known if d.first - DEPLOY_SLACK <= change.time <= d.last + DEPLOY_SLACK]
    if running:
        return max(running, key=lambda d: d.first if d.first <= change.time else -1)
    return None


# ---------------------------------------------------------------------------------------------------------------------
# The report

def run_command(argv, timeout=900):
    try:
        done = subprocess.run(argv, capture_output=True, text=True, timeout=timeout)
    except (OSError, subprocess.TimeoutExpired) as error:
        return 1, str(error)
    return done.returncode, done.stdout + (done.stderr if done.returncode else "")


def _remote_curl(url: str, params: list[tuple[str, object]]) -> list[str]:
    args = " ".join(f"--data-urlencode {shlex.quote(f'{k}={v}')}" for k, v in params)
    return ["ssh", "-oBatchMode=yes", "-oConnectTimeout=15", MONITORING_HOST, f"curl -sS --max-time 60 {shlex.quote(url)} {args}"]


DOCSTRING = __doc__ or ""  # None under python -OO


def parse_args(argv):
    parser = argparse.ArgumentParser(description=DOCSTRING.split("\n")[0])
    parser.add_argument("cc")
    parser.add_argument("--venue", default="", help="a venue substring; none: every venue")
    parser.add_argument("--title", required=True)
    parser.add_argument("--since", default="7d")
    parser.add_argument("--replay", action="store_true")
    parser.add_argument("--recording")
    parser.add_argument("--replay-limit", type=int, default=6)
    args = parser.parse_args(argv)
    args.cc = args.cc.lower()
    return args


Run = Callable[..., Tuple[int, str]]  # run_command's shape: argv (and a timeout) -> (exit code, output)


def _logged_changes(args: argparse.Namespace, run: Run) -> "Tuple[List[Change], List[str]]":
    """The listing's take changes VictoriaLogs holds (oldest first), and the line saying so when it did not answer."""
    code, text = run(_remote_curl(LOGS, [("query", logsql(args.cc, args.venue, args.title, args.since)), ("limit", LOG_LIMIT)]))
    if code != 0:
        return [], [f"VictoriaLogs did not answer (ssh {MONITORING_HOST}): {text.strip()[:300]}"]
    changes = []
    for raw in text.splitlines():
        try:
            change = parse_change(json.loads(raw))
        except (ValueError, KeyError):
            continue
        if change and fold(args.venue) in fold(change.venue) and fold(args.title) in fold(change.listing):
            changes.append(change)
    return sorted(changes, key=lambda c: c.time), []


def _known_deploys(args: argparse.Namespace, run: Run, now: float) -> "Tuple[List[Deploy], List[str]]":
    """The worker images Prometheus saw in the window, and the line saying so when it did not answer."""
    window = seconds(args.since)
    query = (f'max by (image) (kube_pod_container_info{{namespace="kinowo",container="worker",'
             f'pod=~"worker-{args.cc}-.*"}})')
    code, prom = run(_remote_curl(PROMETHEUS, [("query", query), ("start", int(now - window)), ("end", int(now)),
                                               ("step", max(300, window // 2000))]))
    if code != 0:
        return [], [f"Prometheus did not answer: {prom.strip()[:300]}"]
    return deploys(prom), []


def _place_changes(changes: "List[Change]", known: "List[Deploy]", run: Run) -> "List[Change]":
    """Each change given to the deploy that logged it (the commit subject read for those that logged any); the
    changes no deploy owns are returned."""
    unplaced: "List[Change]" = []
    for change in changes:
        deploy = deploy_of(change, known)
        (deploy.changes if deploy else unplaced).append(change)
    known.sort(key=lambda d: d.first)
    for deploy in (d for d in known if d.changes):
        code, line = run(["git", "-C", REPO, "log", "-1", "--format=%H%x09%s", deploy.sha7])
        if code == 0 and "\t" in line:
            deploy.sha, deploy.subject = line.strip().split("\t", 1)
        else:
            deploy.subject = "(commit not in this clone: git fetch)"
    return unplaced


def _takes_lines(changes: "List[Change]", known: "List[Deploy]") -> "List[str]":
    """One line per billing that went through the same takes in the same deploys (its venues counted), then the
    deploys that flipped one listing's take more than once."""
    by_key: "Dict[str, List[Change]]" = defaultdict(list)
    for change in changes:
        by_key[change.key].append(change)
    sha = lambda c: next((d.sha7 for d in known if c in d.changes), "?")  # noqa: E731
    alike: "Dict[Tuple[str, str], List[str]]" = defaultdict(list)
    for seq in by_key.values():
        run_of = f"{seq[0].before} " + " ".join(f"→ {c.after} ({_clock(c.time, False)}, {sha(c)})" for c in seq)
        alike[(seq[0].listing, run_of)].append(seq[0].venue)
    lines = [f"TAKES {_venues_said(venues)} | {listing}: {run_of}" for (listing, run_of), venues in alike.items()]
    return lines + [line for deploy in known for line in _flip_lines(deploy)]


def _venues_said(venues: "List[str]") -> str:
    return venues[0] if len(venues) == 1 else f"{len(venues)} venues ({', '.join(venues[:3])}, …)"


def _flip_lines(deploy: "Deploy") -> "List[str]":
    per_key: "Dict[str, List[Change]]" = defaultdict(list)
    for change in deploy.changes:
        per_key[change.key].append(change)
    return [f"FLIPS within deploy {deploy.sha7}: {len(seq)} changes, {len({c.pod for c in seq if c.pod}) or '?'} pods — {key}"
            for key, seq in per_key.items() if len(seq) > 1]


def _deploy_lines(known: "List[Deploy]", unplaced: "List[Change]") -> "List[str]":
    lines: "List[str]" = []
    for deploy in (d for d in known if d.changes):
        lines += ["", f"DEPLOY {deploy.sha7} {deploy.tag} · running {_clock(deploy.first, False)}–{_clock(deploy.last, False)} · "
                      f"{deploy.subject}", *_change_lines(deploy.changes)]
    if unplaced:
        lines += ["", "DEPLOY unknown (no build field, outside every image Prometheus saw)", *_change_lines(unplaced)]
    quiet = [d.sha7 for d in known if not d.changes]
    if quiet:
        lines += ["", f"{len(quiet)} other deploy(s) in the window logged no change of it: {', '.join(quiet[:12])}"
                  + (" …" if len(quiet) > 12 else "")]
    return lines


def _mirror_state(args: argparse.Namespace, run: Run) -> "Tuple[List[str], Optional[State]]":
    """The prod-now lines, and the mirror's state (None when the mirror is not readable)."""
    code, text = run([os.path.join(REPO, "scripts/local-mirror/freshness"), args.cc, "--need", MIRROR_NEEDS, "--quiet"])
    prod = [f"  {line}" for line in text.strip().splitlines()]
    if code == 2:
        return prod + ["  the mirror is not readable: its state is not shown"], None
    if code != 0:
        prod.append("  the mirror is not fresh: what follows is as of its sync, not now")
    script = f"const VENUE = {json.dumps(re2_escape(args.venue))}, TITLE = {json.dumps(re2_escape(args.title))};" + MIRROR_QUERY
    port = mongo_local.local_mongo_port(machine.environment())
    code, text = run(["mongosh", "--quiet", MIRROR.format(port=port, db=mongo_local.mirror_database(args.cc)), "--eval", script])
    try:
        state = json.loads(text.strip().splitlines()[-1])
    except (ValueError, IndexError):
        return prod + [f"  mirror query failed: {text.strip()[:300]}"], {}
    return prod + [f"  {line}" for line in mirror_lines(state)], state


def _label_lines(args: argparse.Namespace, run: Run, titles: "Set[str]") -> "List[str]":
    """scripts/label-lookup for every spelling the listing went by."""
    lines = ["", "LABELS (scripts/label-lookup):"]
    for title in sorted(titles) or [args.title]:
        code, text = run([os.path.join(REPO, "scripts", "label-lookup"), args.cc, "--title", title, "--no-prod"]
                         + (["--venue", args.venue] if args.venue else []))
        lines += [f"  {line}" for line in text.rstrip().splitlines()] if text.strip() else [f"  none for «{title}»"]
    return lines


def report(args: argparse.Namespace, run: Run = run_command, now: "Optional[float]" = None) -> str:
    now = now or datetime.datetime.now(datetime.timezone.utc).timestamp()
    out = [f"== {args.cc.upper()} «{args.venue}» | «{args.title}», last {args.since}"]
    changes, said = _logged_changes(args, run)
    out += said
    known, said = _known_deploys(args, run, now)
    out += said
    unplaced = _place_changes(changes, known, run)
    out += _takes_lines(changes, known) if changes else [f"no take change logged for it in the last {args.since} (lines before 2026-10-14 are text; both are searched)"]
    out += _deploy_lines(known, unplaced)
    prod, state = _mirror_state(args, run)
    # the labels of every spelling the listing went by: the logged ones and the one prod bills now
    titles = {c.listing for c in changes} | {t["listing"]["rawTitle"] for t in (state or {}).get("traces", []) if t.get("listing")}
    out += _label_lines(args, run, titles)
    out += ["", f"PROD NOW (local mirror {mongo_local.mirror_database(args.cc)}, read-only):", *prod]
    if args.replay:
        out += ["", *replay(args, known, run, state)]
    return "\n".join(out) + "\n"


def _change_lines(changes):
    """A deploy's changes, those alike but for the venue (one billing moved at many venues by one tick) as one line."""
    alike = defaultdict(list)
    for c in changes:
        alike[(c.kind, c.listing, c.before, c.after, c.basis, c.projection or round(c.time))].append(c)
    lines = []
    for group in alike.values():
        line = _change_line(group[0])
        if len(group) > 1:
            line = line.replace(f" {group[0].key}:", f" {group[0].listing} at {len(group)} venues ({group[0].venue}, …):", 1)
        lines.append(line)
    return lines


def _change_line(c):
    detail = [f"«{c.title_after}»" if c.title_after else "", f"via {c.basis}" + (f" {c.confidence}" if c.confidence else "")]
    where = ", ".join(x for x in (c.mode, f"projection {c.projection}" if c.projection else "", f"pod {c.pod}" if c.pod else "") if x)
    return (f"  {_clock(c.time)} {c.kind:<7} {c.key}: {c.before} → {c.after} " + " ".join(d for d in detail if d)
            + (f" [{where}]" if where else "") + (f"\n      reason: {c.reason[:240]}" if c.reason else ""))


def _film(take_line):
    """`TAKE <venue>: tmdb:581 'Title' via X — verdict` → `tmdb 581`, as prod's lines write a film; `none` stays."""
    head = take_line.split(": ", 1)[1].split(" ", 1)[0] if ": " in take_line else "?"
    return head.replace(":", " ")


# The two views a replay decides under: CI's hermetic one (a recording holds no family answers, so the families agree on
# nothing) and prod's (its filed family answers, from this checkout's unmatched-cluster capture). A change only the
# second reproduces is the CI-vs-prod gap: no hermetic leg could have seen it.
# A third, where the mirror is readable: prod's own state NOW (identity-slice --prod-mirror — its filed answers, the
# stage's stored verdicts and the pins), which prints prod's stored take and why beside the slice's.
VIEWS = (("CI (no filed answers)", ["--families", "none"]), ("with prod's filed answers", []))
MIRROR_VIEW = ("with prod's state now (--prod-mirror)", ["--prod-mirror"])


def _newest_recording(args: argparse.Namespace, run: Run) -> "Tuple[str, str]":
    """The recording a replay runs on: the one named, else the one every identity tool defaults to (the newest green
    run that holds this country's corpus). (recording, why not) -- one of them empty."""
    if args.recording:
        return args.recording, ""
    code, text = run(["bash", "-c", '. "$0" && identity_newest_recording "$1"', TOOLS, args.cc], timeout=300)
    recording = text.strip() if code == 0 else ""
    return recording, "" if recording else (text.strip().splitlines() or ["no answer"])[-1]


class SliceTakes:
    """The take identity-slice makes of a listing at a deploy's commit, in one view, decided once per (deploy, key, view)."""

    def __init__(self, cc: str, recording: str, run: Run) -> None:
        self.cc, self.recording, self.run = cc, recording, run
        self.taken: "Dict[Tuple[str, str, str], Tuple[Optional[str], List[str]]]" = {}

    def commit_of(self, deploy: "Deploy") -> str:
        """The deploy's full commit sha, read once; empty when this clone lacks it."""
        if not deploy.sha:
            code, line = self.run(["git", "-C", REPO, "log", "-1", "--format=%H%x09%s", deploy.sha7])
            deploy.sha = line.split("\t", 1)[0].strip() if code == 0 and "\t" in line else ""
        return deploy.sha

    def slice(self, deploy: "Deploy", key: str, view: "Tuple[str, List[str]]") -> "Tuple[Optional[str], List[str]]":
        venue, listing = key.split(" | ", 1)
        code, text = self.run([SLICE, self.cc, "--key", f"{venue}|{listing}", "--ref", deploy.sha, "--recording", self.recording]
                              + view[1], timeout=1800)
        takes = [t for t in text.splitlines() if t.startswith("TAKE ")]
        if code != 0 or not takes:
            return None, ["no take — " + " | ".join(text.strip().splitlines()[-3:])]
        return _film(takes[0]), takes + [t for t in text.splitlines() if t.startswith(("model:", "agreement:", "PROD ", "  prod "))]

    def take(self, deploy: "Deploy", key: str, view: "Tuple[str, List[str]]") -> "Tuple[Optional[str], List[str]]":
        """(film or None, lines) — the slice's take of `key` at `deploy` in one view."""
        memo = (deploy.sha7, key, view[0])
        if memo not in self.taken:
            self.taken[memo] = self.slice(deploy, key, view) if self.commit_of(deploy) else (None, [f"commit {deploy.sha7} not in this clone"])
        return self.taken[memo]


def _reproduced_line(before: "Deploy", deploy: "Deploy", film: str, film_before: "Optional[str]", shown_before: "List[str]") -> str:
    if film_before is None:
        return f"      at {before.sha7}: {shown_before[0]}"
    if film_before == film:
        return f"      NOT REPRODUCED: {before.sha7} takes {film_before} too — in this view the code did not move it"
    return f"      REPRODUCED: {before.sha7} takes {film_before}, {deploy.sha7} {film}"


def _view_lines(takes: SliceTakes, deploy: "Deploy", before: "Optional[Deploy]", key: str, view: "Tuple[str, List[str]]") -> "List[str]":
    film, shown = takes.take(deploy, key, view)
    lines = [f"  {view[0]}: {shown[0]}", *(f"      {t}" for t in shown[1:])]
    if film is None:
        return lines
    if before is None:
        return lines + ["      no earlier deploy in the window to replay against"]
    film_before, shown_before = takes.take(before, key, view)
    return lines + [_reproduced_line(before, deploy, film, film_before, shown_before)]


def _replay_deploy(takes: SliceTakes, views: "Tuple[Tuple[str, List[str]], ...]", state: "Optional[State]",
                   deploy: "Deploy", before: "Optional[Deploy]") -> "List[str]":
    """One listing per billing that moved alike: its slice holds the family, so it stands for the venues moved with it."""
    keys: "Dict[Tuple[str, str, str], str]" = {}
    for c in deploy.changes:
        keys.setdefault((c.listing, c.before, c.after), c.key)
    lines: "List[str]" = []
    for (_listing, was, now), key in keys.items():
        lines += [f"REPLAY {deploy.sha7} {key} (prod: {was} → {now}):",
                  f"  prod now (mirror identity_traces): {trace_reason(state, key)}"]
        for view in views:
            lines += _view_lines(takes, deploy, before, key, view)
    return lines


def replay(args: argparse.Namespace, known: "List[Deploy]", run: Run, state: "Optional[State]" = None) -> "List[str]":
    """identity-slice at each deploy's commit that logged a change and at the deploy before it, every one on ONE
    recording, in CI's view and in prod's: a change is reproduced where the replayed take moves between the two deploys
    as prod's did. With the mirror readable (`state`), a third view reads prod's state now, and each change is shown
    beside prod's reason for the listing now (its identity_traces)."""
    views = VIEWS + ((MIRROR_VIEW,) if state is not None else ())
    recording, why_not = _newest_recording(args, run)
    if not recording:
        return [f"REPLAY: no recording named, and the newest green one holding a {args.cc} corpus could not be read: {why_not}"]
    lines = [f"REPLAY on recording {recording} (identity-slice, one ref at a time):"]
    takes = SliceTakes(args.cc, recording, run)
    changed = [d for d in known if d.changes]
    for deploy in changed[: args.replay_limit]:
        position = known.index(deploy)
        lines += _replay_deploy(takes, views, state, deploy, known[position - 1] if position > 0 else None)
    skipped = len(changed) - args.replay_limit
    return lines + ([f"REPLAY: {skipped} later deploy(s) not replayed (--replay-limit)"] if skipped > 0 else [])


def main(argv):
    sys.stdout.write(report(parse_args(argv)))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
