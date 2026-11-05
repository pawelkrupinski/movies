#!/usr/bin/env python3
"""ONE judgement review page for the owner: every take awaiting the owner, an evidence card each, three answers.

Lists (1) the rows of test/resources/fixtures/identity-judgements.tsv an agent judged (evidence marked AGENT-JUDGED) and
(2) the review candidates (identity-review-candidates.tsv) no judgement row answers yet. Per take: the listing's own
facts as the judgements hold them, the other venues' and films' judgements of the same billing, the agent's suggested
verdict with its reason, and -- when `scripts/identity-judge-cards` has written target/identity-judge/cards.md -- that
card. Answers: confirm, reject, or a different film. Each is written IN PLACE through scripts/ci/identity_judgements.py
(one row per key; the lint's `problems` must stay empty or the file is put back) into the judgements file of the review
worktree -- never the root checkout. "Land my verdicts" runs scripts/review-commit (lints, commits) and
~/bin/agentctl land from that worktree.

  scripts/review-server --judgements [--port N]       # makes the worktree, then runs this against its file
  scripts/judgement_review.py --file PATH [--port N]  # directly, PATH being the worktree's identity-judgements.tsv

Local only: binds 127.0.0.1.
"""
import argparse
import datetime
import hashlib
import html
import os
import subprocess
import sys
import urllib.parse
from http.server import BaseHTTPRequestHandler, HTTPServer

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "ci"))
import identity_judgements as j  # noqa: E402

MARK = "AGENT-JUDGED"
ANSWERED = "REVIEW PAGE"  # an answer prefixes the evidence, keeping the agent's text after "was:"
CANDIDATES = "identity-review-candidates.tsv"
CANDIDATE_COLUMNS = ("country", "venue", "rawTitle", "film", "filmTitle", "flagged", "evidence")


def item_id(*key):
    return hashlib.sha1("\t".join(key).encode("utf-8")).hexdigest()[:12]


class Take:
    """A take awaiting the owner: one (country, raw title, film) over the venues it stands at."""

    def __init__(self, kind, country, title, film, venues, rows, candidate=None):
        self.kind, self.country, self.title, self.film = kind, country, title, film
        self.venues, self.rows, self.candidate = venues, rows, candidate
        self.id = item_id(kind, country, title, film)

    @property
    def verdict(self):
        return self.rows[0].verdict if self.rows else ""

    @property
    def film_title(self):
        if self.rows:
            return next((r.filmTitle for r in self.rows if r.filmTitle), "")
        return self.candidate[0]["filmTitle"]

    @property
    def reason(self):
        if self.rows:
            return self.rows[0].evidence
        return "; ".join(dict.fromkeys(c["evidence"] for c in self.candidate))


def read_candidates(path):
    if not os.path.exists(path):
        return []
    return [dict(zip(CANDIDATE_COLUMNS, cols)) for _, cols in j.lines(path) if cols[0] != "country" and len(cols) == len(CANDIDATE_COLUMNS)]


def awaiting(judgements_path, candidates_path):
    """Every take awaiting the owner: AGENT-JUDGED rows grouped by billing and film, then unanswered candidates."""
    rows = j.read(judgements_path)
    groups = {}
    for r in rows:
        if MARK in r.evidence and not r.evidence.startswith(ANSWERED):
            groups.setdefault((r.country, j.normalise(r.rawTitle), r.film), []).append(r)
    takes = [Take("agent", cc, title, film, [r.venue for r in rs], rs) for (cc, title, film), rs in groups.items()]
    judged = {(r.country, r.venue, j.normalise(r.rawTitle), r.film) for r in rows}
    judged_any = {(r.country, j.normalise(r.rawTitle), r.film) for r in rows if r.venue == "*"}
    pending = {}
    for c in read_candidates(candidates_path):
        title = j.normalise(c["rawTitle"])
        if (c["country"], c["venue"], title, c["film"]) in judged or (c["country"], title, c["film"]) in judged_any:
            continue
        pending.setdefault((c["country"], title, c["film"]), []).append(c)
    takes += [Take("candidate", cc, title, film, [c["venue"] for c in cs], [], cs) for (cc, title, film), cs in pending.items()]
    return takes


def find(takes, ident):
    return next((t for t in takes if t.id == ident), None)


# ── answers ──────────────────────────────────────────────────────────────────────────────────

class AnswerError(Exception):
    pass


def _note(today, what, row):
    old = row.evidence
    return f"{ANSWERED} {today}: owner {what}; was: {row.verdict}: {old}" if old else f"{ANSWERED} {today}: owner {what}"


def _set(row, verdict, today, what):
    row.evidence = _note(today, what, row)
    if verdict != row.verdict and row.state != "open":
        row.state, row.scope, row.reason = "open", "", ""
    elif verdict != row.verdict:
        row.scope = ""
    row.verdict, row.judged = verdict, today


def answer(judgements_path, candidates_path, ident, action, film="", film_title="", today=None):
    """Applies the owner's answer to the take `ident` and writes the file, or raises AnswerError leaving it as it was."""
    today = today or datetime.date.today().isoformat()
    take = find(awaiting(judgements_path, candidates_path), ident)
    if take is None:
        raise AnswerError("no such take awaiting an answer (answered already?)")
    rows = j.read(judgements_path)
    by_key = {r.key: r for r in rows}
    ours = [by_key[r.key] for r in take.rows]
    added = []  # (line to follow, new row)
    last_line = max((r.line for r in rows), default=0)
    if take.kind == "candidate":
        verdict = {"right": "right", "wrong": "wrong", "other": "wrong"}.get(action)
        if verdict is None:
            raise AnswerError(f"a candidate is answered right, wrong or other, not {action}")
        for c in take.candidate:
            ours.append(j.Row([c["country"], c["venue"], c["rawTitle"], c["film"], verdict, "open", "", c["filmTitle"], "", today,
                               f"{ANSWERED} {today}: owner judged {verdict}; candidate {c['flagged']}: {c['evidence']}"]))
        added = [(last_line, r) for r in ours]
        ours = []
    else:
        if action == "confirm":
            for r in ours:
                _set(r, r.verdict, today, f"confirmed {r.verdict}")
        elif action == "reject":
            for r in ours:
                _set(r, "right" if r.verdict == "wrong" else "wrong", today, "overruled")
        elif action == "other":
            for r in ours:
                _set(r, "wrong", today, "overruled: another film")
        else:
            raise AnswerError(f"unknown answer {action}")
    if action == "other":
        film = film.strip()
        if not j.FILM.match(film):
            raise AnswerError(f"{film!r} is no film id (tmdb:N, imdb:ttN, ...)")
        if film == take.film:
            raise AnswerError("the other film is the take's own")
        for venue in take.venues:
            key = (take.country, venue, take.title, film)
            existing = by_key.get(key)
            evidence = f"{ANSWERED} {today}: owner chose this film for the billing the take of {take.film} was wrong for"
            if existing is None:
                added.append((max([r.line for r in ours] or [last_line]),
                              j.Row([take.country, venue, take.title, film, "right", "open", "", film_title.strip(), "", today, evidence])))
            else:
                _set(existing, "right", today, "chose this film")
                ours.append(existing)
    with open(judgements_path, encoding="utf-8") as f:
        before = f.read()
    j.edit_in_place(judgements_path, {r.line: r for r in ours}, added)
    bad = j.problems(j.read(judgements_path))
    if bad:
        with open(judgements_path, "w", encoding="utf-8") as f:
            f.write(before)
        raise AnswerError("the lint refuses: " + "; ".join(m for _, m in bad[:3]))


# ── the page ─────────────────────────────────────────────────────────────────────────────────

def other_judgements(rows, take):
    """The judgements of the same billing at other venues or for other films, for the card."""
    return [r for r in rows if r.country == take.country and j.normalise(r.rawTitle) == take.title
            and not (r.film == take.film and r.venue in take.venues)]


def judge_card(cards_path, take):
    if not cards_path or not os.path.exists(cards_path):
        return ""
    with open(cards_path, encoding="utf-8") as f:
        blocks = f.read().split("\n## ")
    return "\n\n".join(b for b in blocks if take.title in b and take.film.split(":")[-1] in b)[:3000]


def render(judgements_path, candidates_path, cards_path=None, message="", pending=0, land_command=""):
    esc = html.escape
    takes = awaiting(judgements_path, candidates_path)
    rows = j.read(judgements_path)
    out = ["<!doctype html><meta charset=utf-8><title>Judgement review</title>",
           "<style>body{font:15px system-ui;max-width:60em;margin:1em auto;padding:0 1em}.card{border:1px solid #888;border-radius:6px;"
           "padding:.7em 1em;margin:1em 0}.right{color:#080}.wrong{color:#b00}pre{white-space:pre-wrap;background:#8881;padding:.5em}"
           "button{margin:.2em}.msg{background:#ff08;padding:.4em}small{opacity:.7}</style>",
           f"<h1>Judgement review: {len(takes)} awaiting you</h1>"]
    if message:
        out.append(f"<p class=msg>{esc(message)}</p>")
    out.append(f"<form method=post action=/land><p>{pending} unlanded change(s) in the review worktree. "
               f"<button>Land my verdicts</button> <small>runs the lint, then <code>{esc(land_command)}</code></small></p></form>")
    for t in takes:
        out.append(f"<div class=card id={t.id}><h3>[{t.country}] {esc(t.title)}</h3>")
        out.append(f"<p>Take: <b>{esc(t.film)}</b> {esc(t.film_title)}"
                   + (f" &mdash; suggested <b class={t.verdict}>{t.verdict}</b>" if t.verdict else " &mdash; <i>flagged candidate, no verdict yet</i>")
                   + f"<br>Venues: {esc(', '.join(t.venues))}</p>")
        out.append(f"<p>Why ({'agent' if t.kind == 'agent' else 'candidate'}):</p><pre>{esc(t.reason)}</pre>")
        others = other_judgements(rows, t)
        if others:
            out.append("<p>Other judgements of this billing:</p><ul>" + "".join(
                f"<li>{esc(r.venue)}: <span class={r.verdict}>{r.verdict}</span> {esc(r.film)} {esc(r.filmTitle)} "
                f"<small>{esc(r.evidence[:160])}</small></li>" for r in others) + "</ul>")
        card = judge_card(cards_path, t)
        if card:
            out.append(f"<details><summary>identity-judge-cards</summary><pre>{esc(card)}</pre></details>")
        out.append(f"<form method=post action=/answer><input type=hidden name=id value={t.id}>")
        if t.kind == "agent":
            out.append("<button name=action value=confirm>Confirm</button><button name=action value=reject>Reject</button>")
        else:
            out.append("<button name=action value=right>Right</button><button name=action value=wrong>Wrong</button>")
        out.append("or a different film: <input name=film placeholder=tmdb:123 size=14> <input name=filmTitle placeholder='title (year)' size=22> "
                   "<button name=action value=other>Take it</button></form></div>")
    return "\n".join(out)


def worktree_of(judgements_path):
    return os.path.abspath(os.path.join(os.path.dirname(judgements_path), "..", "..", ".."))


def land_commands(worktree):
    return [["scripts/review-commit"], [os.path.expanduser("~/bin/agentctl"), "land"]]


def land(worktree, commands=None):
    """Runs the lint+commit and `agentctl land` in the review worktree; returns (ok, output)."""
    log = []
    for command in commands or land_commands(worktree):
        p = subprocess.run(command, cwd=worktree, capture_output=True, text=True)
        log.append(f"$ {' '.join(command)}\n{p.stdout}{p.stderr}")
        if p.returncode != 0:
            return False, "\n".join(log)
    return True, "\n".join(log)


def pending_changes(worktree):
    p = subprocess.run(["git", "status", "--porcelain", "--", "test/resources/fixtures/identity-judgements.tsv"],
                       cwd=worktree, capture_output=True, text=True)
    return len(p.stdout.splitlines())


def make_handler(judgements_path, land_runner=land):
    worktree = worktree_of(judgements_path)
    candidates = os.path.join(os.path.dirname(judgements_path), CANDIDATES)
    cards = os.path.join(worktree, "target/identity-judge/cards.md")

    class Handler(BaseHTTPRequestHandler):
        def log_message(self, *args):
            pass

        def _send(self, body, status=200):
            data = body.encode("utf-8")
            self.send_response(status)
            self.send_header("Content-Type", "text/html; charset=utf-8")
            self.send_header("Content-Length", str(len(data)))
            self.end_headers()
            self.wfile.write(data)

        def _page(self, message=""):
            self._send(render(judgements_path, candidates, cards, message, pending_changes(worktree),
                              "scripts/review-commit && ~/bin/agentctl land"))

        def do_GET(self):
            self._page()

        def do_POST(self):
            form = urllib.parse.parse_qs(self.rfile.read(int(self.headers.get("Content-Length", 0))).decode("utf-8"))
            one = lambda k: form.get(k, [""])[0]  # noqa: E731
            if self.path == "/land":
                ok, output = land_runner(worktree)
                return self._send(f"<!doctype html><meta charset=utf-8><h1>{'Landed' if ok else 'NOT landed'}</h1>"
                                  f"<pre>{html.escape(output)}</pre><a href=/>back</a>")
            try:
                answer(judgements_path, candidates, one("id"), one("action"), one("film"), one("filmTitle"))
                self._page("saved")
            except AnswerError as e:
                self._page(f"not saved: {e}")

    return Handler


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--file", default=os.environ.get("REVIEW_LABELS_FILE"))
    ap.add_argument("--port", type=int, default=9100)
    args = ap.parse_args()
    if not args.file:
        sys.exit("judgement_review: no --file (or REVIEW_LABELS_FILE): the review worktree's identity-judgements.tsv")
    server = HTTPServer(("127.0.0.1", args.port), make_handler(os.path.abspath(args.file)))
    n = len(awaiting(args.file, os.path.join(os.path.dirname(args.file), CANDIDATES)))
    print(f"[judgement-review] {n} takes awaiting you: http://127.0.0.1:{args.port}/  (writing {args.file})", file=sys.stderr, flush=True)
    server.serve_forever()


if __name__ == "__main__":
    main()
