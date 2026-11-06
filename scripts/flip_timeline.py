#!/usr/bin/env python3
"""ONE LISTING'S FLIP TIMELINE (scripts/flip-timeline).

    flip_timeline.py <cc> --venue S --title S [--since 7d] [--dump FILE | --from FILE]
    flip_timeline.py <cc> --all [--since 7d] [--summary] [--tsv [FILE]]

One timeline, in time order, of what moved a listing's take — and, at the end, why each reversal (a take that went back
to the film it had left) happened:

  PROJ  every projection that wrote a take of it: its trigger (scrape / answer / film-change / boot / periodic / retry),
        start and end, its agreement phase, pod and build (VictoriaLogs `identity projection:` lines);
  TAKE  each take change (identity_history `take`): before -> after, stage, basis, decided_by and the trace's rules,
        the members of its decision (who joined or left), the cause prod logged, and `awaits`/`unanswered` when kept;
  ASK   every family ask filed for its title (identity_history `answer:identity_question_markers`, the agreement
        stage's question memory) and the record asks filed during a projection of it: family, filed or failed, inline
        (filed while a projection of it ran, from its agreement phase on: sent ~ the phase's start) or queued;
  LATE  an ask filed after the projection that needed it gave up waiting (its agreement phase's start +
        AgreementStage.AskNowWithin, 5 s) — the two-step write: that projection wrote without it, the next re-read it;
  LIST  a member listing (re-)published in the window (identity_history `listing`);
  HIST  the inline-ask histogram (kinowo_worker_identity_agreement_inline_ask_seconds) around a dropping projection,
        where it has samples.

REVERSALS classifies each X -> Y -> X of one listing, first match wins:
  DEPLOY/RESTART  a boot (`deploy`) projection on either side, or the build or the pod changed between them;
  LATE-ANSWER     the restoring projection was answer-triggered and an ask of the cluster was filed after the dropping
                  projection gave up waiting, before the restoring one began (inferred, and said so, for a poster
                  vote: poster answers are not kept in identity_history);
  SIBLING/POPULATION  the decision's members moved (a listing joined or left) or a member was re-published between them;
  RULE            none of the above: the same inputs, decided otherwise;
  UNCLASSIFIED    (never silent: it says why) no projection line for a take, or an answer-triggered restore with no ask
                  naming the listing's title.
--all reads every take change of the country (paged), keeps the reversals restored within 24 h and classifies each.

Reads the LOCAL prod mirror (:28017, read-only, after scripts/local-mirror/freshness), VictoriaLogs and Prometheus over
`ssh monitoring-1` (listing_history's helpers). `--dump FILE` keeps what was read, `--from FILE` prints from it (the
self-test's fixtures are such dumps). Tests: flip_timeline_test.py beside it.
"""
import argparse
import datetime
import io
import json
import os
import re
import sys
from collections import defaultdict

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import listing_history as shared  # noqa: E402  (fold, seconds, re2_escape, the runner, the mirror and log endpoints)

REPO = shared.REPO
ASK_NOW_WITHIN = 5.0          # AgreementStage.AskNowWithin: how long a projection waits for its inline asks
PROJECTION_SLACK = 1.5        # a take is written just after its projection's summary line
WINDOW_BEFORE, WINDOW_AFTER = 90, 120   # the projection lines read around each take / ask
MAX_WINDOWS = 40
PROMETHEUS_INSTANT = shared.PROMETHEUS.replace("query_range", "query")
HISTOGRAM = "kinowo_worker_identity_agreement_inline_ask_seconds_bucket"

REVERSAL_WITHIN = 86400     # --all: a take that went back within 24 h is a reversal (the backtest's window); later is a new decision
PAGE = 400                  # take changes read per mirror call: a country's week is far more, so the reads page
CHUNK = 100                 # takes (their windows, their read ids) per asks / answers call; member ids per siblings call
MEMBER_CHUNK = 200

# Every mirror read is one mongosh script under a header of constants; the marker comment names it (tests dispatch on it).
TAKES_QUERY = """// takes
const vre = new RegExp(VENUE, 'i'), tre = new RegExp(TITLE, 'i'), since = new Date(SINCE);
const q = {kind: 'take', at: {$gte: since}, 'after.venue': vre, 'after.listing': tre};
if (AFTER_ID) q.$and = [{$or: [{at: {$gt: new Date(AFTER_AT)}}, {at: new Date(AFTER_AT), _id: {$gt: ObjectId(AFTER_ID)}}]}];
const page = db.identity_history.find(q).sort({at: 1, _id: 1}).limit(PAGE).toArray();
print(JSON.stringify(page.map(t => ({at: t.at.getTime(), id: t.id, oid: t._id.toString(), build: t.build || '',
  readIds: (t.after.reads || []).filter(r => !r.includes('identity_family_answers')).map(r => r.slice(r.indexOf(' ') + 1)),
  after: Object.assign({}, t.after, {reads: undefined})}))));
"""

SIBLINGS_QUERY = """// siblings
const since = new Date(SINCE);
const found = db.identity_history.find({kind: 'take', at: {$gte: since}, id: {$in: IDS}}, {id: 1, at: 1, 'after.members': 1})
  .sort({at: 1}).toArray();
print(JSON.stringify(found.map(t => ({at: t.at.getTime(), id: t.id, members: (t.after || {}).members || []}))));
"""

ASKS_QUERY = """// asks
const since = new Date(SINCE), H = db.identity_history;
const near = WINDOWS.map(w => ({at: {$gte: new Date(w[0]), $lte: new Date(w[1])}}));
const askTitle = TITLE ? new RegExp('\\|title\\|.*' + TITLE, 'i') : null;
const clauses = (askTitle ? [{id: askTitle}] : []).concat(near.length ? [{id: /\\|record\\|/, $or: near}] : []);
const asks = clauses.length ? H.find({kind: 'answer:identity_question_markers', at: {$gte: since}, $or: clauses}).sort({at: 1}).limit(2000).toArray() : [];
const readIds = READ_IDS;
const answers = readIds.length && near.length ? H.find({kind: {$in: ['answer:tmdb_queries', 'answer:tmdb_films']},
  id: {$in: readIds}, $or: near}, {kind: 1, id: 1, at: 1}).sort({at: 1}).limit(500).toArray() : [];
print(JSON.stringify({
  asks: asks.map(a => ({at: a.at.getTime(), id: a.id, outcome: (a.after || {}).outcome || '', failures: (a.after || {}).failures || 0})),
  answers: answers.map(a => ({at: a.at.getTime(), kind: a.kind, id: a.id}))}));
"""

LISTINGS_QUERY = """// listings
const since = new Date(SINCE);
const found = db.identity_history.find({kind: 'listing', at: {$gte: since}, id: {$in: IDS}}, {id: 1, at: 1}).sort({at: 1}).limit(2000).toArray();
print(JSON.stringify(found.map(l => ({at: l.at.getTime(), id: l.id}))));
"""

# --all: the question-marker asks filed in each reversal's window whose subject is its (cleaned) title; and the member
# listings re-published inside a window, for the reversals nothing else explained
ALL_ASKS_QUERY = """// all-asks
const H = db.identity_history, seen = new Set(), out = [];
const add = a => { const k = a.at.getTime() + a.id; if (!seen.has(k)) { seen.add(k);
  out.push({at: a.at.getTime(), id: a.id, outcome: (a.after || {}).outcome || '', failures: (a.after || {}).failures || 0}); } };
WINDOWS.forEach(w => {
  const titled = H.find({kind: 'answer:identity_question_markers', at: {$gte: new Date(w.lo), $lte: new Date(w.hi)}, id: new RegExp(w.needle, 'i')})
    .sort({at: 1}).limit(200).toArray();
  titled.forEach(add);
  // the record asks its family filed after its first title ask (candidates found by the title answers)
  if (titled.length) H.find({kind: 'answer:identity_question_markers', id: /\\|record\\|/, at: {$gte: titled[0].at, $lte: new Date(w.hi)}})
    .sort({at: 1}).limit(300).forEach(add);
});
print(JSON.stringify(out));
"""

ALL_LISTINGS_QUERY = """// all-listings
const H = db.identity_history, seen = new Set(), out = [];
WINDOWS.forEach(w => H.find({kind: 'listing', at: {$gte: new Date(w.lo), $lte: new Date(w.hi)}, id: {$in: w.members}}, {id: 1, at: 1})
  .limit(200).forEach(l => { const k = l.at.getTime() + l.id; if (!seen.has(k)) { seen.add(k); out.push({at: l.at.getTime(), id: l.id}); } }));
print(JSON.stringify(out));
"""


def title_pattern(title):
    """A regex (RE2 and PCRE alike) for a title read the way the resolver reads one: its words, in order, whatever
    punctuation or brackets lie between them — «(IMAX) Sinners» finds «IMAX Sinners» and «(IMAX) Sinners» alike."""
    words = re.findall(r"\w+", title or "")
    return "[^0-9A-Za-z]+".join(shared.re2_escape(w) for w in words) if words else shared.re2_escape(title or "")


def clean_title(title):
    """The title the family asks name: bracketed qualifiers ((IMAX), [3D]) dropped."""
    cleaned = re.sub(r"\([^)]*\)|\[[^\]]*\]", " ", title or "").strip()
    return cleaned or (title or "")


def ask_needle(title):
    """The regex of a family ask id naming `title`: its last `|`-part is the title, or its slug, with an optional year."""
    return r"\|" + title_pattern(clean_title(title)) + r"([^0-9A-Za-z]+[0-9]{4})?$"


def logsql_changes(cc, venue, title, since):
    """listing_history.logsql with the title read as title_pattern does."""
    pattern = f"(?i)identity take change: (lost|changed|gained) [^|]*{shared.re2_escape(venue)}[^|]* \\| .*{title_pattern(title)}"
    return (f'_time:{since} "identity take change" country:{cc} _msg:~{json.dumps(pattern, ensure_ascii=False)} | sort by (_time)')


def _clock(ts, frac=True):
    text = datetime.datetime.fromtimestamp(ts, datetime.timezone.utc).strftime("%m-%d %H:%M:%S.%f")
    return text[:-4] if frac else text[:-7]


def _hms(ts):
    return _clock(ts)[6:]


def _span(seconds):
    if seconds < 120:
        return f"{seconds:.1f} s"
    return f"{seconds / 60:.0f} min" if seconds < 7200 else f"{seconds / 3600:.1f} h"


def _iso(ts):
    return datetime.datetime.fromtimestamp(ts, datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%S.%fZ")


def key_label(key):
    """`P\\0venue\\0title\\0…` / `N\\0venue\\0url\\0title` → `venue | title`."""
    parts = key.split("\x00")
    if len(parts) >= 4 and parts[0] == "N":
        return f"{parts[1]} | {parts[3]}"
    return f"{parts[1]} | {parts[2]}" if len(parts) >= 3 else key


# ---------------------------------------------------------------------------------------------------------------------
# Projections (VictoriaLogs `identity projection:` lines)

def _phase(message, name):
    match = re.search(rf"\b{name} ([0-9.]+)s", message)
    return float(match.group(1)) if match else 0.0


def parse_projection(line):
    """A projection's summary line → its id, trigger, start, end and agreement phase (the phases before it summed)."""
    if not line.get("projection_id") or "identity projection:" not in line.get("_msg", ""):
        return None
    message = line["_msg"]
    start, end = shared._ts(line["projection_id"]), shared._ts(line["_time"])
    agreement_start = start + sum(_phase(message, p) for p in ("listings", "snapshot", "resolve"))
    return {"id": line["projection_id"], "trigger": line.get("projection_trigger") or "?", "mode": line.get("projection_mode") or "",
            "start": start, "end": end, "agreement": (agreement_start, agreement_start + _phase(message, "agreement")),
            "pod": line.get("pod") or "", "build": (line.get("build") or "")[:7]}


def trigger_label(trigger):
    return "+".join("boot" if t == "deploy" else t for t in trigger.split("+"))


def projection_of(at, projections):
    """The projection that wrote a take kept at `at`: the latest begun before it and ended at most a moment before."""
    running = [p for p in projections if p["start"] <= at <= p["end"] + PROJECTION_SLACK]
    return max(running, key=lambda p: p["start"]) if running else None


def projection_windows(times):
    spans = sorted((t - WINDOW_BEFORE, t + WINDOW_AFTER) for t in times)
    merged = []
    for lo, hi in spans:
        if merged and lo <= merged[-1][1]:
            merged[-1][1] = max(merged[-1][1], hi)
        else:
            merged.append([lo, hi])
    return merged


def projections_logsql(cc, windows):
    ranges = " or ".join(f"_time:[{_iso(lo)}, {_iso(hi)}]" for lo, hi in windows)
    return f'country:{cc} "identity projection:" ({ranges}) | sort by (_time)'


# ---------------------------------------------------------------------------------------------------------------------
# Gathering (every outside call through `run`, as listing_history's)

def _header(args, since, **extra):
    consts = {"VENUE": shared.re2_escape(args.venue), "TITLE": title_pattern(args.title), "SINCE": int(since * 1000)}
    consts.update(extra)
    return "const " + ", ".join(f"{k} = {json.dumps(v)}" for k, v in consts.items()) + ";\n"


def _mongo(run, raw, args, header, query):
    """One mirror script's JSON answer, or None (said in raw's notes) when it did not answer."""
    port = os.environ.get("LOCAL_MIRROR_PORT", "28017")
    code, text = run(["mongosh", "--quiet", shared.MIRROR.format(port=port, db=shared.mirror_db(args.cc)), "--eval", header + query])
    try:
        return json.loads(text.strip().splitlines()[-1])
    except (ValueError, IndexError):
        raw["notes"].append(f"mirror query failed: {text.strip()[:300]}")
        return None


def _chunks(items, size):
    return [items[i:i + size] for i in range(0, len(items), size)]


def read_takes(args, run, raw, since):
    """ALL take changes of the window, one page of PAGE after another (the cursor is the last one's time and id)."""
    takes, after = [], {"AFTER_AT": 0, "AFTER_ID": ""}
    while True:
        page = _mongo(run, raw, args, _header(args, since, PAGE=PAGE, **after), TAKES_QUERY)
        if page is None:
            break
        takes.extend(page)
        if len(page) < PAGE:
            break
        after = {"AFTER_AT": page[-1]["at"], "AFTER_ID": page[-1]["oid"]}
    return takes


def read_siblings(args, run, raw, since, members):
    """The other members' take changes (whose decision a listing joined or left), a chunk of ids at a time; a member's
    consecutive changes with the same members are kept once, so what is held is the decision changes only."""
    kept, last = [], {}
    for ids in _chunks(sorted(members), MEMBER_CHUNK):
        for entry in _mongo(run, raw, args, _header(args, since, IDS=ids), SIBLINGS_QUERY) or []:
            if last.get(entry["id"]) != entry["members"]:
                last[entry["id"]] = entry["members"]
                kept.append(entry)
    return kept


def gather(args, run, now):
    since = now - shared.seconds(args.since)
    raw = {"cc": args.cc, "venue": args.venue, "title": args.title, "since": args.since, "now": now, "notes": []}
    code, text = run([os.path.join(REPO, "scripts/local-mirror/freshness"), args.cc, "--need", "identity_history", "--quiet"])
    raw["freshness"] = text.strip()
    empty = {"takes": [], "siblings": [], "asks": [], "answers": [], "listings": []}
    raw["history"] = dict(empty)
    if code == 2:
        raw["notes"].append("the mirror is not readable")
    else:
        if code != 0:
            raw["notes"].append("the mirror is not fresh: the newest history may be missing")
        history = raw["history"]
        takes = read_takes(args, run, raw, since)
        members = set()
        for take in takes:
            members.update(take["after"].get("members") or [])
        for start in range(0, len(takes), CHUNK):
            chunk = takes[start:start + CHUNK]
            windows = [[t["at"] - 30000, t["at"] + 2000] for t in chunk]
            read_ids = sorted({r for t in chunk for r in t["readIds"]})
            found = _mongo(run, raw, args, _header(args, since, WINDOWS=windows, READ_IDS=read_ids), ASKS_QUERY) or {}
            history["asks"].extend(a for a in found.get("asks", []) if a not in history["asks"])
            history["answers"].extend(a for a in found.get("answers", []) if a not in history["answers"])
        for take in takes:
            take.pop("readIds", None)
            take.pop("oid", None)
        history["takes"] = takes
        history["siblings"] = read_siblings(args, run, raw, since, members)
        for ids in _chunks(sorted(members), MEMBER_CHUNK):
            history["listings"].extend(_mongo(run, raw, args, _header(args, since, IDS=ids), LISTINGS_QUERY) or [])
        history["asks"].sort(key=lambda a: a["at"])
    history = raw["history"]
    code, text = run(shared._remote_curl(shared.LOGS, [("query", logsql_changes(args.cc, args.venue, args.title, args.since)),
                                                       ("limit", shared.LOG_LIMIT)]))
    raw["changes"] = _json_lines(text) if code == 0 else []
    if code != 0:
        raw["notes"].append(f"VictoriaLogs did not answer: {text.strip()[:200]}")
    times = [t["at"] / 1000 for t in history["takes"]] + [a["at"] / 1000 for a in history["asks"] if "|title|" in a["id"]]
    raw["projections"] = read_projections(args.cc, times, run) if times and code == 0 else []
    raw["histogram"] = {}
    projections = [p for p in map(parse_projection, raw["projections"]) if p]
    for take in history["takes"]:
        projection = projection_of(take["at"] / 1000, projections)
        if take["after"].get("kind") == "lost" and projection and projection["id"] not in raw["histogram"]:
            span = max(60, int(projection["end"] - projection["start"]) + 30)
            query = (f'sum by (family, part, le) (increase({HISTOGRAM}{{country="{args.cc}"}}[{span}s]))')
            code, text = run(shared._remote_curl(PROMETHEUS_INSTANT, [("query", query), ("time", round(projection["end"] + 30, 3))]))
            raw["histogram"][projection["id"]] = text if code == 0 else ""
    return raw


def read_projections(cc, times, run):
    """The projection summary lines around `times`, a batch of windows per VictoriaLogs call."""
    lines = []
    for batch in _chunks(projection_windows(times), MAX_WINDOWS):
        code, text = run(shared._remote_curl(shared.LOGS, [("query", projections_logsql(cc, batch)), ("limit", 20000)]))
        if code == 0:
            lines.extend(l for l in _json_lines(text) if "identity projection:" in l.get("_msg", ""))
    return lines


def _json_lines(text):
    out = []
    for line in text.splitlines():
        try:
            out.append(json.loads(line))
        except ValueError:
            continue
    return out


# ---------------------------------------------------------------------------------------------------------------------
# The timeline

class Take:
    def __init__(self, entry):
        after = entry["after"]
        self.at = entry["at"] / 1000
        self.key = entry["id"]
        self.venue, self.listing = after.get("venue") or "", after.get("listing") or ""
        self.label = f"{self.venue} | {self.listing}"
        self.logged_only = entry.get("logged_only", False)
        self.kind = after.get("kind", "")
        self.before = after.get("tmdb_before")
        self.after = after.get("tmdb_after")
        self.stage, self.basis, self.decided_by = after.get("stage", ""), after.get("basis", ""), after.get("decided_by", "")
        self.previous_decided_by = after.get("previous_decided_by", "")
        self.rules = after.get("rules", "")
        self.trigger, self.cause = after.get("trigger", ""), after.get("cause", "")
        self.members = list(after.get("members") or []) if isinstance(after.get("members"), list) else []
        self.member_count = len(self.members) or int(after.get("members") or 0)
        self.awaits, self.unanswered = after.get("awaits"), after.get("unanswered")
        self.build = (entry.get("build") or "")[:7]
        self.projection = None
        self.projection_id = after.get("projection_id")
        self.joined, self.left = [], []

    @classmethod
    def logged(cls, line):
        """A take change only VictoriaLogs holds (older than identity_history's week): its line's fields."""
        after = dict(line)
        after["trigger"] = line.get("projection_trigger", "")
        for side in ("before", "after"):
            after[f"tmdb_{side}"] = int(line[f"tmdb_{side}"]) if line.get(f"tmdb_{side}") else None
        key = f"P\x00{line.get('venue')}\x00{line.get('listing')}\x00"
        return cls({"at": shared._ts(line["_time"]) * 1000, "id": key, "build": line.get("build", ""), "after": after,
                    "logged_only": True})

    @property
    def shape(self):
        return (self.kind, self.before, self.after, self.stage, self.decided_by, round(self.at, 1))


def _film(tmdb):
    return f"tmdb {tmdb}" if tmdb else "none"


def merge_logged(takes, lines, raw):
    """The log's take changes of the listing: each one's projection id onto the history's take it logged (same listing,
    within 2 s), and those the history no longer keeps as takes of their own."""
    out = []
    for line in lines:
        if not line.get("kind") or not line.get("venue") or not line.get("_time"):
            continue
        if shared.fold(raw["venue"]) not in shared.fold(line["venue"]) or \
                not re.search(title_pattern(shared.fold(raw["title"])), shared.fold(line.get("listing", "")), re.I):
            continue
        at = shared._ts(line["_time"])
        label = f"{line['venue']} | {line.get('listing', '')}"
        twin = next((t for t in takes if t.label == label and abs(t.at - at) < 2.0), None)
        if twin:
            twin.projection_id = twin.projection_id or line.get("projection_id")
        else:
            out.append(Take.logged(line))
    return out


def population_moves(takes, siblings):
    """Each take's `joined` / `left`: the members its decision gained or lost since the listing's previous decision — or,
    with none kept, the members whose own previous decision did not hold the listing (they joined it)."""
    decisions = defaultdict(list)
    for entry in siblings:
        decisions[entry["id"]].append((entry["at"] / 1000, set(entry["members"])))
    for take in takes:
        decisions[take.key].append((take.at, set(take.members)))
    for history in decisions.values():
        history.sort(key=lambda d: d[0])

    def previous(key, at):
        earlier = [members for when, members in decisions.get(key, []) if when < at - 0.05]
        return earlier[-1] if earlier else None

    for take in takes:
        now = set(take.members)
        if not now:
            continue
        before = previous(take.key, take.at)
        if before is not None:
            take.joined, take.left = sorted(now - before), sorted(before - now)
        else:
            take.joined = sorted(m for m in now - {take.key}
                                 if previous(m, take.at) is not None and take.key not in previous(m, take.at))


def asks_of(history, projections):
    """The asks, each with its family, what it asked, outcome, and whether it was filed inline (while a projection of the
    listing ran, from its agreement phase on) or queued."""
    out = []
    for entry in history["asks"]:
        parts = entry["id"].split("|", 3)
        family, what, subject = (parts + ["", "", ""])[1:4] if len(parts) >= 2 else ("?", "", entry["id"])
        at = entry["at"] / 1000
        inline = next((p for p in projections if p["agreement"][0] <= at <= p["end"]), None)
        out.append({"at": at, "family": family, "what": what, "subject": subject,
                    "outcome": entry.get("outcome") or "filed", "failures": entry.get("failures") or 0, "inline": inline})
    return out


def histogram_line(text):
    """`family part: n asks, k over 5 s` from an increase-by-le query; None when it holds no samples."""
    try:
        result = json.loads(text)["data"]["result"]
    except (ValueError, KeyError, TypeError):
        return None
    buckets = defaultdict(dict)
    for series in result:
        metric = series["metric"]
        buckets[(metric.get("family", "?"), metric.get("part", "?"))][metric.get("le", "")] = float(series["value"][1])
    parts = []
    for (family, part), by_le in sorted(buckets.items()):
        total = by_le.get("+Inf", 0.0)
        if total < 0.5:
            continue
        within = by_le.get("5.0", by_le.get("5", 0.0))
        parts.append(f"{family} {part} {total:.0f} ({total - within:.0f} over 5 s)")
    return "; ".join(parts) if parts else None


def late_asks(asks, dropping, restoring_start):
    """The asks filed after `dropping` gave up waiting (its agreement start + AskNowWithin, or its agreement's end if
    sooner) and before the restoring projection began."""
    gave_up = min(dropping["agreement"][0] + ASK_NOW_WITHIN, dropping["agreement"][1])
    return [a for a in asks if gave_up < a["at"] <= restoring_start and a["at"] >= dropping["start"]]


def reversals(takes, by="label", within=None):
    """Per listing (`by` its label, or its key), each consecutive pair of changes whose second goes back to the film the
    first left."""
    by_key = defaultdict(list)
    for take in takes:
        by_key[getattr(take, by)].append(take)
    out = []
    for seq in by_key.values():
        seq.sort(key=lambda t: t.at)
        for first, second in zip(seq, seq[1:]):
            if first.before != first.after and second.after == first.before and second.before == first.after \
                    and (within is None or second.at - first.at <= within):
                out.append((first, second))
    return out


def classify(first, second, asks, listings):
    """(class, why) of one reversal: `first` took the listing off a film, `second` put it back."""
    p1, p2 = first.projection, second.projection
    trigger1 = p1["trigger"] if p1 else first.trigger
    trigger2 = p2["trigger"] if p2 else second.trigger
    if "deploy" in trigger1.split("+") or "deploy" in trigger2.split("+"):
        side = "the dropping" if "deploy" in trigger1.split("+") else "the restoring"
        return "DEPLOY/RESTART", f"{side} projection was a boot (trigger deploy)"
    if first.build and second.build and first.build != second.build:
        return "DEPLOY/RESTART", f"the build changed between them: {first.build} -> {second.build}"
    if p1 and p2 and p1["pod"] and p2["pod"] and p1["pod"] != p2["pod"]:
        return "DEPLOY/RESTART", f"the pod changed between them: {p1['pod']} -> {p2['pod']}"
    if "answer" in trigger2.split("+"):
        restore_start = p2["start"] if p2 else second.at
        late = late_asks(asks, p1, restore_start) if p1 else [a for a in asks if first.at - 30 <= a["at"] <= restore_start]
        if late:
            last = late[-1]
            gave_up = (f", after projection {_hms(p1['start'])} gave up waiting (agreement {_hms(p1['agreement'][0])} "
                       f"+ {ASK_NOW_WITHIN:.0f} s)" if p1 else "")
            return "LATE-ANSWER", (f"{len(late)} ask(s) of the cluster filed late, the last {last['family']} {last['what']} "
                                   f"«{last['subject']}» at {_hms(last['at'])}{gave_up}; the answer-triggered projection "
                                   f"{_hms(restore_start)} restored it by {second.decided_by}")
        if second.stage == "poster":
            return "LATE-ANSWER", (f"inferred: restored by {second.decided_by} in an answer-triggered projection; poster "
                                   "answers are not kept in identity_history, so the late filing itself is not visible")
    moved = sorted(set(first.joined + first.left + second.joined + second.left))
    if moved:
        return "SIBLING/POPULATION", (f"the decision's members moved: {len(moved)} listing(s) joined or left "
                                      f"({', '.join(key_label(m) for m in moved[:3])}{', …' if len(moved) > 3 else ''})")
    lo = p1["start"] if p1 else first.at - 30
    hi = p2["start"] if p2 else second.at
    published = [l for l in listings if lo - 60 <= l["at"] / 1000 <= hi and l["id"] != first.key]
    if published:
        return "SIBLING/POPULATION", (f"{len(published)} member listing(s) re-published between them "
                                      f"({key_label(published[0]['id'])} at {_hms(published[0]['at'] / 1000)})")
    if not (p1 and p2):
        gap = "the dropping" if not p1 else "the restoring"
        return "UNCLASSIFIED", (f"no projection line found for {gap} take (VictoriaLogs holds none around {_clock(first.at if not p1 else second.at)}): "
                                "its trigger, pod and asks cannot be judged")
    if "answer" in trigger2.split("+"):
        return "UNCLASSIFIED", (f"restored by {second.decided_by or second.basis} in an answer-triggered projection, but no ask "
                                "naming the listing's title was found: asks are keyed by the resolver's search title, which "
                                "the listing's text may not spell")
    return "RULE", (f"same build, members and answers: {first.decided_by or first.basis} then "
                    f"{second.decided_by or second.basis} on the same inputs")


def render(raw):
    history = raw["history"]
    projections = sorted((p for p in map(parse_projection, raw.get("projections", [])) if p), key=lambda p: p["start"])
    takes = [Take(t) for t in history["takes"]]
    takes += merge_logged(takes, raw.get("changes", []), raw)
    by_id = {p["id"]: p for p in projections}
    for take in takes:
        take.projection = by_id.get(take.projection_id) or projection_of(take.at, projections)
    population_moves(takes, history.get("siblings", []))
    asks = asks_of(history, projections)
    keys = {t.key for t in takes}

    out = [f"== {raw['cc'].upper()} «{raw['venue']}» | «{raw['title']}», last {raw['since']}"
           + (f" — {len(keys)} listing(s)" if takes else "")]
    out.extend(f"   {line}" for line in raw.get("freshness", "").splitlines()[:2])
    out.extend(f"   NOTE {note}" for note in raw.get("notes", []))
    if not takes:
        out.append(f"no take change of it kept in identity_history in the last {raw['since']}")
        return "\n".join(out) + "\n"

    rows = []   # (time, order, text)
    touching = {}
    for take in takes:
        if take.projection:
            touching[take.projection["id"]] = take.projection
    for p in touching.values():
        rows.append((p["start"], 0, f"PROJ  {trigger_label(p['trigger']):<18} {_hms(p['start'])} -> {_hms(p['end'])} "
                     f"({p['end'] - p['start']:.1f} s; agreement {_hms(p['agreement'][0])}-{_hms(p['agreement'][1])})"
                     + (f" {p['mode']}" if p["mode"] else "") + (f" pod {p['pod']}" if p["pod"] else "")
                     + (f" build {p['build']}" if p["build"] else "")))
        hist = histogram_line(raw.get("histogram", {}).get(p["id"], ""))
        if hist:
            rows.append((p["end"], 3, f"HIST  inline asks (country-wide, around {_hms(p['start'])}): {hist}"))

    groups = defaultdict(list)
    for take in takes:
        groups[take.shape].append(take)
    for group in groups.values():
        t = group[0]
        who = t.label if len(group) == 1 else f"{len(group)} listings ({', '.join(g.label for g in group[:2])}{', …' if len(group) > 2 else ''})"
        moved = ""
        if t.joined or t.left:
            moved = (f"; joined {', '.join(key_label(k) for k in t.joined[:3])}{' …' if len(t.joined) > 3 else ''}" if t.joined else "") + \
                    (f"; left {', '.join(key_label(k) for k in t.left[:3])}{' …' if len(t.left) > 3 else ''}" if t.left else "")
        extra = "".join([f" · rules {t.rules}" if t.rules else "", f" · awaits {t.awaits}" if t.awaits else "",
                         f" · unanswered {t.unanswered}" if t.unanswered else ""])
        where = f"proj {_hms(t.projection['start'])}" if t.projection else f"trigger {trigger_label(t.trigger)}"
        rows.append((t.at, 2, f"TAKE  {t.kind:<7} {_film(t.before)} -> {_film(t.after)}  {who}\n"
                     f"{'':20}stage {t.stage} · {t.basis} · {t.decided_by or '-'}"
                     + (f" (was {t.previous_decided_by})" if t.previous_decided_by else "")
                     + f"{extra} · {t.member_count} member(s){moved} · cause {t.cause or '-'} · build {t.build or '?'} [{where}]"
                     + (" (VictoriaLogs only)" if t.logged_only else "")))

    lates = {}
    for take in takes:
        if take.projection:
            nxt = [p for p in projections if p["start"] > take.projection["start"]]
            for ask in late_asks(asks, take.projection, nxt[0]["start"] if nxt else take.at + 600):
                lates[id(ask)] = take.projection
    for ask in asks:
        mode = (f"inline in {_hms(ask['inline']['start'])} (sent ~{_hms(ask['inline']['agreement'][0])}, "
                f"+{ask['at'] - ask['inline']['agreement'][0]:.1f} s)" if ask["inline"] else "queued")
        outcome = ask["outcome"] + (f" after {ask['failures']} failure(s)" if ask["failures"] else "")
        late = lates.get(id(ask))
        rows.append((ask["at"], 1, f"ASK   {ask['family']:<10} {ask['what']} «{ask['subject']}» {outcome} · {mode}"
                     + (f"\n{'':20}LATE: after projection {_hms(late['start'])} gave up waiting "
                        f"({_hms(late['agreement'][0])} + {ASK_NOW_WITHIN:.0f} s); it ended {_hms(late['end'])}" if late else "")))
    for answer in history.get("answers", []):
        rows.append((answer["at"] / 1000, 1, f"READ  {answer['kind'].split(':', 1)[-1]} {answer['id']} filed"))
    elsewhere = 0
    for listing in history.get("listings", []):
        at = listing["at"] / 1000
        if not any(p["start"] - 60 <= at <= p["end"] for p in touching.values()):
            elsewhere += 1
            continue
        rows.append((at, 1, f"LIST  {key_label(listing['id'])} published" + (" (this listing)" if listing["id"] in keys else "")))

    for at, _, text in sorted(rows, key=lambda r: (r[0], r[1])):
        out.append(f"{_clock(at)}  {text}")
    if elsewhere:
        out.append(f"({elsewhere} other publication(s) of its members in the window, none within a minute of a projection of it)")

    pairs = reversals(takes)
    out.append("")
    out.append(f"REVERSALS ({len(pairs)})" if pairs else "REVERSALS: none (no take went back to a film it had left)")
    alike = defaultdict(list)
    for first, second in pairs:
        verdict, why = classify(first, second, asks, history.get("listings", []))
        alike[(first.before, first.after, round(first.at, 1), round(second.at, 1), verdict, why)].append((first.label, first.at, second.at))
    for (was, now, _, _, verdict, why), seen in alike.items():
        labels = [label for label, _, _ in seen]
        t1, t2 = seen[0][1], seen[0][2]
        who = labels[0] if len(labels) == 1 else f"{len(labels)} listings ({labels[0]}, …)"
        out.append(f"  {verdict:<18} {_film(was)} -> {_film(now)} ({_clock(t1)}) -> {_film(was)} (+{_span(t2 - t1)})  {who}")
        out.append(f"  {'':18} {why}")
    return "\n".join(out) + "\n"


# ---------------------------------------------------------------------------------------------------------------------
# --all: every reversal of a country, by the flip-prediction backtest's definition (a listing's take change that goes back
# to the film its previous change left: X -> Y -> X, consecutive changes of ONE listing key), each classified.

CLASSES = ("LATE-ANSWER", "SIBLING/POPULATION", "RULE", "DEPLOY/RESTART", "UNCLASSIFIED")
TSV_COLUMNS = ("class", "cc", "venue", "listing", "from", "via", "dropped_at", "restored_after_s", "dropped_by",
               "restored_by", "dropped_trigger", "restored_trigger", "build", "why")


def cluster_asks(asks, first, second):
    """The asks of the reversed listing's family in its window: those naming its title and, when it filed any, the record
    asks (the candidates the title answers found) filed from its first title ask on."""
    needle = re.compile(ask_needle(first.listing), re.I)
    lo = ((first.projection or {}).get("start") or first.at - 30) - 5
    hi = second.at + 2
    titled = [a for a in asks if lo <= a["at"] / 1000 <= hi and needle.search(a["id"])]
    if not titled:
        return []
    start = min(a["at"] for a in titled) / 1000 - 0.001
    return titled + [a for a in asks if "|record|" in a["id"] and start <= a["at"] / 1000 <= hi and a not in titled]


def analyse_all(raw, within=REVERSAL_WITHIN):
    """Every reversal of `raw` (a gather_all result) restored within `within` seconds, with its class and why: a list of
    dicts."""
    history = raw["history"]
    projections = sorted((p for p in map(parse_projection, raw.get("projections", [])) if p), key=lambda p: p["start"])
    takes = [Take(t) for t in history["takes"]]
    by_id = {p["id"]: p for p in projections}
    for take in takes:
        take.projection = by_id.get(take.projection_id) or projection_of(take.at, projections)
    population_moves(takes, [])
    rows = []
    for first, second in reversals(takes, by="key", within=within):
        own = asks_of({"asks": cluster_asks(history.get("asks", []), first, second)}, projections)
        members = set(first.members) | set(second.members)
        listings = [l for l in history.get("listings", []) if l["id"] in members]
        verdict, why = classify(first, second, own, listings)
        p1, p2 = first.projection, second.projection
        rows.append({"class": verdict, "cc": raw["cc"], "venue": first.venue, "listing": first.listing, "from": first.before,
                     "via": first.after, "dropped_at": _iso(first.at), "restored_after_s": round(second.at - first.at, 1),
                     "dropped_by": first.decided_by, "restored_by": second.decided_by,
                     "dropped_trigger": p1["trigger"] if p1 else first.trigger,
                     "restored_trigger": p2["trigger"] if p2 else second.trigger,
                     "build": second.build, "why": why, "first": first, "second": second})
    return rows


def gather_all(args, run, now):
    """Page every take change of the country, then read around the reversals only: their projections, the asks naming
    their titles, and (for those nothing else explained) the member listings re-published inside their window."""
    since = now - shared.seconds(args.since)
    args.venue = args.title = ""
    raw = {"cc": args.cc, "all": True, "since": args.since, "now": now, "notes": [], "venue": "", "title": "",
           "history": {"takes": [], "siblings": [], "asks": [], "answers": [], "listings": []}, "projections": []}
    code, text = run([os.path.join(REPO, "scripts/local-mirror/freshness"), args.cc, "--need", "identity_history", "--quiet"])
    raw["freshness"] = text.strip()
    if code == 2:
        raw["notes"].append("the mirror is not readable")
        return raw
    if code != 0:
        raw["notes"].append("the mirror is not fresh: the newest history may be missing")
    history = raw["history"]
    history["takes"] = read_takes(args, run, raw, since)
    for take in history["takes"]:
        take.pop("readIds", None)
        take.pop("oid", None)
    takes = [Take(t) for t in history["takes"]]
    pairs = reversals(takes, by="key", within=REVERSAL_WITHIN)
    times = [t.at for pair in pairs for t in pair]
    if times:
        raw["projections"] = read_projections(args.cc, times, run)
    projections = [p for p in map(parse_projection, raw["projections"]) if p]
    for take in takes:
        take.projection = projection_of(take.at, projections)
    windows = [{"lo": int((((a.projection or {}).get("start") or a.at - 30) - 5) * 1000), "hi": int((b.at + 2) * 1000),
                "needle": ask_needle(a.listing)} for a, b in pairs]
    for batch in _chunks(windows, 50):
        history["asks"].extend(_mongo(run, raw, args, "", ALL_ASKS_QUERY.replace("WINDOWS", json.dumps(batch))) or [])
    explained = analyse_all(raw)
    unexplained = [r for r in explained if r["class"] in ("RULE", "UNCLASSIFIED")]
    windows = [{"lo": int((((r["first"].projection or {}).get("start") or r["first"].at - 30) - 60) * 1000),
                "hi": int((((r["second"].projection or {}).get("start") or r["second"].at)) * 1000),
                "members": sorted((set(r["first"].members) | set(r["second"].members)) or {r["first"].key})} for r in unexplained]
    for batch in _chunks(windows, 20):
        history["listings"].extend(_mongo(run, raw, args, "", ALL_LISTINGS_QUERY.replace("WINDOWS", json.dumps(batch))) or [])
    return raw


def render_all(raw, summary=False, tsv=None):
    """The per-class count table, a line per reversal (unless `summary`), and `tsv` (a file object) a row per reversal."""
    rows = analyse_all(raw)
    counts = {c: sum(1 for r in rows if r["class"] == c) for c in CLASSES}
    out = [f"== {raw['cc'].upper()} all reversals, last {raw['since']} — {len(rows)} reversal(s) among "
           f"{len(raw['history']['takes'])} take change(s)"]
    out.extend(f"   {line}" for line in raw.get("freshness", "").splitlines()[:2])
    out.extend(f"   NOTE {note}" for note in raw.get("notes", []))
    out.append(f"{'class':<20}{'count':>6}{'share':>8}")
    for name in CLASSES:
        out.append(f"{name:<20}{counts[name]:>6}{(counts[name] / len(rows) if rows else 0):>8.0%}")
    out.append(f"{'total':<20}{len(rows):>6}")
    if not summary:
        out.append("")
        for r in sorted(rows, key=lambda r: (CLASSES.index(r["class"]), r["dropped_at"])):
            out.append(f"{r['class']:<18} {r['dropped_at'][5:19]}  {r['venue']} | {r['listing']}  {_film(r['from'])} -> "
                       f"{_film(r['via'])} -> back (+{_span(r['restored_after_s'])})")
            out.append(f"{'':19}{r['why']}")
    if tsv is not None:
        tsv.write("\t".join(TSV_COLUMNS) + "\n")
        for r in rows:
            tsv.write("\t".join(str(r[c] if r[c] is not None else "").replace("\t", " ").replace("\n", " ") for c in TSV_COLUMNS) + "\n")
    return "\n".join(out) + "\n"


def parse_args(argv):
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("cc")
    parser.add_argument("--venue", default="")
    parser.add_argument("--title", default="")
    parser.add_argument("--all", action="store_true", help="every reversal of the country, classified (no --venue / --title)")
    parser.add_argument("--summary", action="store_true", help="with --all: the per-class count table only")
    parser.add_argument("--tsv", nargs="?", const="-", metavar="FILE", help="with --all: a row per reversal, to FILE (stdout when bare)")
    parser.add_argument("--since", default="7d")
    parser.add_argument("--dump", help="keep what was read in FILE (JSON)")
    parser.add_argument("--from", dest="source", help="print from a --dump FILE, reading nothing")
    args = parser.parse_args(argv)
    args.cc = args.cc.lower()
    if args.all and (args.venue or args.title):
        parser.error("--all reads the whole country: no --venue / --title")
    if not args.all and not (args.title or args.venue):
        parser.error("--venue and/or --title is required (or --all for the whole country)")
    return args


def main(argv, run=shared.run_command, now=None):
    args = parse_args(argv)
    if args.source:
        with open(args.source, encoding="utf-8") as f:
            raw = json.load(f)
    else:
        gatherer = gather_all if args.all else gather
        raw = gatherer(args, run, now or datetime.datetime.now(datetime.timezone.utc).timestamp())
        if args.dump:
            with open(args.dump, "w", encoding="utf-8") as f:
                json.dump(raw, f, ensure_ascii=False, indent=1)
    if raw.get("all"):
        tsv = None
        if args.tsv == "-":
            tsv = io.StringIO()
        elif args.tsv:
            tsv = open(args.tsv, "w", encoding="utf-8")
        text = render_all(raw, summary=args.summary, tsv=tsv)
        sys.stdout.write(text)
        if args.tsv == "-":
            sys.stdout.write("\n" + tsv.getvalue())
        elif tsv:
            tsv.close()
    else:
        sys.stdout.write(render(raw))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
