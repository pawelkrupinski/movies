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

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
LOGS = "http://10.20.0.11:9428/select/logsql/query"
PROMETHEUS = "http://10.20.0.11:9090/api/v1/query_range"
MONITORING_HOST = os.environ.get("LISTING_HISTORY_MONITORING_HOST", "monitoring-1")
MIRROR = "mongodb://127.0.0.1:{port}/{db}?directConnection=true&serverSelectionTimeoutMS=5000"
MIRROR_NEEDS = "identity_traces,identity_model_families,identity_listings"   # agreements and pins may hold nothing
LOG_LIMIT = 2000
# the identity-slice every replay runs: another checkout's when this one's tool files no longer compile at old refs
SLICE = os.environ.get("LISTING_HISTORY_SLICE", os.path.join(REPO, "scripts", "identity-slice"))
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
# The mirror

MIRROR_QUERY = """
const venue = new RegExp(VENUE, 'i'), title = new RegExp(TITLE, 'i');
const traces = db.identity_traces.find({'listing.venue': venue, 'listing.rawTitle': title}).limit(40).toArray();
const families = db.identity_model_families.find({_id: {$in: traces.map(t => t.family)}},
  {decisions: 1}).toArray();
const venues = db.identity_listings.find({_id: venue}).limit(40).toArray().map(v => ({_id: v._id,
  films: (v.films || []).filter(f => title.test((f.movie || {}).rawTitle || (f.movie || {}).title || ''))}))
  .filter(v => v.films.length > 0);
// the agreement stage's stored verdict (filed under the cluster's key, whose first listing is its family's id; under the
// family's id itself before keys, else one naming the title) and pins
const ids = traces.map(t => t.family);
const keyed = db.identity_agreement_keys.find({first: {$in: ids}}).toArray().map(k => k._id.toString());
const agreements = db.identity_agreements.find({$or: [{_id: {$in: ids.concat(keyed)}}, {_id: title}]}).limit(10).toArray()
  .map(a => ({_id: a._id, agreed: a.agreed, filled: a.filled, correction: a.correction, rules: a.rules}));
const pins = db.identity_pins.find({'listings.rawTitle': title}).limit(10).toArray();
print(EJSON.stringify({traces: traces, families: families, venues: venues, agreements: agreements, pins: pins},
  {relaxed: true}));
"""


def mirror_db(cc):
    return ("kinowo" if cc == "pl" else f"kinowo_{cc}") + "_prod_mirror"


def _family_id(raw):
    return raw.replace("\x00", " · ")


def mirror_lines(state):
    lines = []
    for trace in state.get("traces", []):
        listing = trace.get("listing", {})
        lines.append(f"TRACE {listing.get('venue')} | {listing.get('rawTitle')}: film {trace.get('film') or 'none'} via "
                     f"{trace.get('basis')}" + (f", vetoed by {trace['vetoedBy']}" if trace.get("vetoedBy") else "")
                     + (f"; rules {', '.join(trace['rules'])}" if trace.get("rules") else ""))
        if trace.get("evidence"):
            lines.append(f"    evidence: {'; '.join(trace['evidence'][:5])}")
        for refusal in trace.get("refusals", [])[:3]:
            lines.append(f"    refused: {refusal if isinstance(refusal, str) else json.dumps(refusal, ensure_ascii=False)}")
        if trace.get("candidates"):
            lines.append(f"    candidates: {'; '.join(trace['candidates'][:3])}")
        lines.append(f"    family: {_family_id(trace.get('family', ''))}")
    keys = {(t["listing"].get("venue"), t["listing"].get("rawTitle")) for t in state.get("traces", []) if t.get("listing")}
    for family in state.get("families", []):
        for decision in family.get("decisions", []):
            # a decision's members: its listings, or its listings by node (a list of lists)
            flat = [m for group in decision.get("members", []) for m in (group if isinstance(group, list) else [group])]
            members = {(m.get("venue"), m.get("rawTitle")) for m in flat}
            if keys and not members & keys:
                continue
            confidence = decision.get("confidence")
            lines.append(f"FAMILY {_family_id(family['_id'])}: film {decision.get('film') or 'none'} via {decision.get('basis')}"
                         + (f" at {confidence:.0%}" if isinstance(confidence, (int, float)) else "")
                         + f", {len(members)} member(s)" + (f", agreed {json.dumps(decision['agreed'], ensure_ascii=False)}"
                                                         if decision.get("agreed") else ""))
            lines.extend(f"    {line}" for line in decision.get("explanation", [])[:3])
    for agreement in state.get("agreements", []):
        agreed, filled, correction = agreement.get("agreed"), agreement.get("filled"), agreement.get("correction")
        what = [f"agreed «{agreed.get('title')}» by {', '.join(agreed.get('families', []))}" if agreed else "agreed on nothing",
                f"filled by {filled.get('rule')} (tmdb {filled.get('tmdb')})" if filled else None,
                f"corrected to {correction.get('film')}" if correction and correction.get("film") else None]
        lines.append(f"AGREEMENT {_family_id(agreement['_id'])}: " + "; ".join(w for w in what if w))
    for pin in state.get("pins", []):
        lines.append(f"PIN {pin.get('kind')} {pin.get('tmdbId') or ''} by {pin.get('author')}: {pin.get('reason')}")
    for venue in state.get("venues", []):
        for film in venue.get("films", []):
            movie = film.get("movie", {})
            facts = [f"year {movie['releaseYear']}" if movie.get("releaseYear") else None,
                     f"runtime {movie['runtimeMinutes']}" if movie.get("runtimeMinutes") else None,
                     f"original title «{movie['originalTitle']}»" if movie.get("originalTitle") else None,
                     f"director {', '.join(film['director'])}" if film.get("director") else None,
                     f"{len(film.get('showtimes', []))} showtimes",
                     f"ids {json.dumps(film['externalIds'])}" if film.get("externalIds") else None]
            lines.append(f"LISTING {venue['_id']} | {movie.get('rawTitle') or movie.get('title')}: "
                         + ", ".join(f for f in facts if f))
    return lines or ["the mirror holds no trace, family or listing matching it"]


# ---------------------------------------------------------------------------------------------------------------------
# The report

def run_command(argv, timeout=900):
    try:
        done = subprocess.run(argv, capture_output=True, text=True, timeout=timeout)
    except (OSError, subprocess.TimeoutExpired) as error:
        return 1, str(error)
    return done.returncode, done.stdout + (done.stderr if done.returncode else "")


def _remote_curl(url, params):
    args = " ".join(f"--data-urlencode {shlex.quote(f'{k}={v}')}" for k, v in params)
    return ["ssh", MONITORING_HOST, f"curl -s {url} {args}"]


def parse_args(argv):
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
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


def report(args, run=run_command, now=None):
    now = now or datetime.datetime.now(datetime.timezone.utc).timestamp()
    window = seconds(args.since)
    out = [f"== {args.cc.upper()} «{args.venue}» | «{args.title}», last {args.since}"]

    code, text = run(_remote_curl(LOGS, [("query", logsql(args.cc, args.venue, args.title, args.since)), ("limit", LOG_LIMIT)]))
    changes = []
    if code != 0:
        out.append(f"VictoriaLogs did not answer (ssh {MONITORING_HOST}): {text.strip()[:300]}")
    for raw in text.splitlines() if code == 0 else []:
        try:
            change = parse_change(json.loads(raw))
        except (ValueError, KeyError):
            continue
        if change and fold(args.venue) in fold(change.venue) and fold(args.title) in fold(change.listing):
            changes.append(change)
    changes.sort(key=lambda c: c.time)

    step = max(300, window // 2000)
    query = (f'max by (image) (kube_pod_container_info{{namespace="kinowo",container="worker",'
             f'pod=~"worker-{args.cc}-.*"}})')
    code, prom = run(_remote_curl(PROMETHEUS, [("query", query), ("start", int(now - window)), ("end", int(now)), ("step", step)]))
    known = deploys(prom) if code == 0 else []
    if code != 0:
        out.append(f"Prometheus did not answer: {prom.strip()[:300]}")
    unplaced = []
    for change in changes:
        deploy = deploy_of(change, known)
        (deploy.changes if deploy else unplaced).append(change)
    known.sort(key=lambda d: d.first)
    for deploy in known:
        if deploy.changes:
            code, line = run(["git", "-C", REPO, "log", "-1", "--format=%H%x09%s", deploy.sha7])
            if code == 0 and "\t" in line:
                deploy.sha, deploy.subject = line.strip().split("\t", 1)
            else:
                deploy.subject = "(commit not in this clone: git fetch)"

    if not changes:
        out.append(f"no take change logged for it in the last {args.since} (lines before 2026-10-14 are text; both are searched)")
    else:
        by_key = defaultdict(list)
        for change in changes:
            by_key[change.key].append(change)
        sha = lambda c: next((d.sha7 for d in known if c in d.changes), "?")
        # listings of one billing that went through the same takes in the same deploys: one line, their venues counted
        alike = defaultdict(list)
        for key, seq in by_key.items():
            run_of = f"{seq[0].before} " + " ".join(f"→ {c.after} ({_clock(c.time, False)}, {sha(c)})" for c in seq)
            alike[(seq[0].listing, run_of)].append(seq[0].venue)
        for (listing, run_of), venues in alike.items():
            where = venues[0] if len(venues) == 1 else f"{len(venues)} venues ({', '.join(venues[:3])}, …)"
            out.append(f"TAKES {where} | {listing}: {run_of}")
        for deploy in known:
            per_key = defaultdict(list)
            for change in deploy.changes:
                per_key[change.key].append(change)
            for key, seq in per_key.items():
                if len(seq) > 1:
                    pods = {c.pod for c in seq if c.pod}
                    out.append(f"FLIPS within deploy {deploy.sha7}: {len(seq)} changes, {len(pods) or '?'} pods — {key}")
    quiet = [d.sha7 for d in known if not d.changes]
    for deploy in known:
        if not deploy.changes:
            continue
        out.append("")
        out.append(f"DEPLOY {deploy.sha7} {deploy.tag} · running {_clock(deploy.first, False)}–{_clock(deploy.last, False)} · "
                   f"{deploy.subject}")
        out.extend(_change_lines(deploy.changes))
    if unplaced:
        out.append("")
        out.append("DEPLOY unknown (no build field, outside every image Prometheus saw)")
        out.extend(_change_lines(unplaced))
    if quiet:
        out.append("")
        out.append(f"{len(quiet)} other deploy(s) in the window logged no change of it: {', '.join(quiet[:12])}"
                   + (" …" if len(quiet) > 12 else ""))

    prod, state, readable = [], {}, False
    port = os.environ.get("LOCAL_MIRROR_PORT", "28017")
    code, text = run([os.path.join(REPO, "scripts/local-mirror/freshness"), args.cc, "--need", MIRROR_NEEDS, "--quiet"])
    prod.extend(f"  {line}" for line in text.strip().splitlines())
    if code == 2:
        prod.append("  the mirror is not readable: its state is not shown")
    else:
        readable = True
        if code != 0:
            prod.append("  the mirror is not fresh: what follows is as of its sync, not now")
        script = f"const VENUE = {json.dumps(re2_escape(args.venue))}, TITLE = {json.dumps(re2_escape(args.title))};" + MIRROR_QUERY
        code, text = run(["mongosh", "--quiet", MIRROR.format(port=port, db=mirror_db(args.cc)), "--eval", script])
        try:
            state = json.loads(text.strip().splitlines()[-1])
            prod.extend(f"  {line}" for line in mirror_lines(state))
        except (ValueError, IndexError):
            prod.append(f"  mirror query failed: {text.strip()[:300]}")

    # the labels of every spelling the listing went by: the logged ones and the one prod bills now
    titles = {c.listing for c in changes} | {t["listing"]["rawTitle"] for t in state.get("traces", []) if t.get("listing")}
    out.append("")
    out.append("LABELS (scripts/label-lookup):")
    for title in sorted(titles) or [args.title]:
        code, text = run([os.path.join(REPO, "scripts", "label-lookup"), args.cc, "--title", title, "--no-prod"]
                         + (["--venue", args.venue] if args.venue else []))
        out.extend(f"  {line}" for line in text.rstrip().splitlines()) if text.strip() else out.append(f"  none for «{title}»")

    out.append("")
    out.append(f"PROD NOW (local mirror {mirror_db(args.cc)}, read-only):")
    out.extend(prod)

    if args.replay:
        out.append("")
        out.extend(replay(args, known, run, state if readable else None))
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


def trace_reason(state, key):
    """Prod's reason for `key` (`venue | raw title`) now, from the mirror's identity_traces: film, basis, rules, blocker."""
    if state is None:
        return "the mirror is not readable"
    venue, listing = key.split(" | ", 1)
    for trace in state.get("traces", []):
        if trace.get("listing", {}).get("venue") == venue and trace.get("listing", {}).get("rawTitle") == listing:
            return (f"film {trace.get('film') or 'none'} via {trace.get('basis')}"
                    + (f"; rules {' '.join(trace['rules'])}" if trace.get("rules") else "")
                    + (f"; blocker {trace['blocker']}" if trace.get("blocker") else "")
                    + (f"; vetoed by {trace['vetoedBy']}" if trace.get("vetoedBy") else ""))
    return "the mirror holds no trace of it now (not listed, or billed otherwise)"


def replay(args, known, run, state=None):
    """identity-slice at each deploy's commit that logged a change and at the deploy before it, every one on ONE
    recording, in CI's view and in prod's: a change is reproduced where the replayed take moves between the two deploys
    as prod's did. With the mirror readable (`state`), a third view reads prod's state now, and each change is shown
    beside prod's reason for the listing now (its identity_traces)."""
    views = VIEWS + ((MIRROR_VIEW,) if state is not None else ())
    recording = args.recording
    if not recording:
        # the one every identity tool defaults to: the newest green run that holds this country's corpus
        code, text = run(["bash", "-c", '. "$0" && identity_newest_recording "$1"', TOOLS, args.cc], timeout=300)
        recording = text.strip() if code == 0 else ""
        if not recording:
            reason = (text.strip().splitlines() or ["no answer"])[-1]
            return [f"REPLAY: no recording named, and the newest green one holding a {args.cc} corpus could not be read: {reason}"]
    lines = [f"REPLAY on recording {recording} (identity-slice, one ref at a time):"]
    taken = {}

    def take(deploy, key, view):
        """(film or None, lines) — the slice's take of `key` at `deploy` in one view, decided once."""
        memo = (deploy.sha7, key, view[0])
        if memo not in taken:
            if not deploy.sha:
                code, line = run(["git", "-C", REPO, "log", "-1", "--format=%H%x09%s", deploy.sha7])
                deploy.sha = line.split("\t", 1)[0].strip() if code == 0 and "\t" in line else ""
            if not deploy.sha:
                taken[memo] = (None, [f"commit {deploy.sha7} not in this clone"])
                return taken[memo]
            venue, listing = key.split(" | ", 1)
            code, text = run([SLICE, args.cc, "--key", f"{venue}|{listing}", "--ref", deploy.sha, "--recording", recording]
                             + view[1], timeout=1800)
            takes = [l for l in text.splitlines() if l.startswith("TAKE ")]
            if code != 0 or not takes:
                taken[memo] = (None, ["no take — " + " | ".join(text.strip().splitlines()[-3:])])
            else:
                taken[memo] = (_film(takes[0]), takes + [l for l in text.splitlines()
                                                         if l.startswith(("model:", "agreement:", "PROD ", "  prod "))])
        return taken[memo]

    changed = [d for d in known if d.changes]
    for deploy in changed[: args.replay_limit]:
        # one listing per billing that moved alike: its slice holds the family, so it stands for the venues moved with it
        keys = {}
        for c in deploy.changes:
            keys.setdefault((c.listing, c.before, c.after), c.key)
        before = known[known.index(deploy) - 1] if known.index(deploy) > 0 else None
        for (listing, was, now), key in keys.items():
            lines.append(f"REPLAY {deploy.sha7} {key} (prod: {was} → {now}):")
            lines.append(f"  prod now (mirror identity_traces): {trace_reason(state, key)}")
            for view in views:
                film, shown = take(deploy, key, view)
                lines.append(f"  {view[0]}: {shown[0]}")
                lines.extend(f"      {l}" for l in shown[1:])
                if film is None:
                    continue
                if before is None:
                    lines.append("      no earlier deploy in the window to replay against")
                    continue
                film_before, shown_before = take(before, key, view)
                if film_before is None:
                    lines.append(f"      at {before.sha7}: {shown_before[0]}")
                elif film_before == film:
                    lines.append(f"      NOT REPRODUCED: {before.sha7} takes {film_before} too — in this view the code did not "
                                 "move it")
                else:
                    lines.append(f"      REPRODUCED: {before.sha7} takes {film_before}, {deploy.sha7} {film}")
    skipped = len(changed) - args.replay_limit
    if skipped > 0:
        lines.append(f"REPLAY: {skipped} later deploy(s) not replayed (--replay-limit)")
    return lines


def main(argv):
    sys.stdout.write(report(parse_args(argv)))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
