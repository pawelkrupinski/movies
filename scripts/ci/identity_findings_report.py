#!/usr/bin/env python3
"""THE IDENTITY FINDINGS REPORT: the newest recording's whole-corpus findings, one GitHub issue per country.

What a recording brings -- a suspect take no label judges, a take a label calls wrong, a screening marker left in a
listing's query, a slot field read off the wrong node, an allowlist entry it no longer breaks -- is DATA: no commit made
it, so no build fails on it (the identity takes check fails only what a commit's code made, base against head). It is
triaged from here instead. Each `Record scrape fixtures` run's convergence rows leave it in their diagnostics artifact
(`convergence-diagnostics-<cc>-convergence`):

  <cc>-suspect-unjudged.txt               suspect|wrong <TAB> venues <TAB> rawTitle <TAB> film <TAB> title <TAB> reasons
                                          (CountryConvergenceBehaviour's suspect audit)
  corpus-shape-recorded-<cc>-<spec>.txt   finding|stale <TAB> venue <TAB> listing <TAB> what (CorpusShapeSpec)

Per country this updates ONE issue, "Identity findings: <cc>", in place: every finding with its ready triage commands,
when it was first seen, and its age. A suspect the identity judgements have judged (or tolerate in scope `served`) since the recording is
dropped at once; the rest drop when a recording no longer carries them. An empty report closes the issue; a finding
reopens it. First-seen dates ride in the issue body, so the report needs no store of its own.

AGE LIMIT: a finding untriaged for more than --max-age-days (3) days turns the report run red. That red is NOT a
red-build gate (scripts/hooks/pre-push's GatingWorkflows): a push neither makes nor mends a recording's data, so holding
pushes behind it is the very coupling this report exists to undo. It reaches the owner through the issue instead.

usage: identity_findings_report.py --run RUN [--countries pl,de,uk,es,us] [--judgements F] [--today D]
         [--max-age-days N] [--assignee LOGIN] [--server URL] [--repo OWNER/NAME] [--work DIR] [--dry-run]
Exit 0 when every finding is within its age, 1 when one is overdue, 2 when no country's findings could be read.
"""

import argparse
import datetime
import glob
import hashlib
import json
import os
import re
import shlex
import subprocess
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from identity_judgements import judgements_of, label_of, normalise  # noqa: E402  (the one reading of a judgement)

COUNTRIES = ["pl", "de", "uk", "es", "us"]
STATE = re.compile(r"<!-- identity-findings-state: (\{.*?\}) -->", re.S)


def title_of(cc):
    return f"Identity findings: {cc}"


def read_findings(directory, cc):
    """The findings a convergence row's diagnostics hold for `cc`, each a dict with its `id`; None when the directory
    holds neither the suspect file nor a corpus-shape file (the row never reached them)."""
    suspects = os.path.join(directory, f"{cc}-suspect-unjudged.txt")
    shapes = sorted(glob.glob(os.path.join(directory, f"corpus-shape-recorded-{cc}-*.txt")))
    if not os.path.exists(suspects) and not shapes:
        return None
    out = []
    if os.path.exists(suspects):
        with open(suspects, encoding="utf-8") as f:
            for line in f:
                cols = line.rstrip("\n").split("\t")
                if len(cols) < 6:
                    continue
                kind, venues, raw, film, title, reasons = cols[:6]
                venue = venues.split(", ")[0]
                out.append({"kind": kind, "venue": venue, "venues": venues, "rawTitle": raw, "film": film, "title": title,
                            "what": reasons, "id": f"{kind}|{normalise(raw)}|{film}"})
    for path in shapes:
        spec = os.path.basename(path)[len(f"corpus-shape-recorded-{cc}-"):-len(".txt")]
        with open(path, encoding="utf-8") as f:
            for line in f:
                cols = line.rstrip("\n").split("\t")
                if len(cols) < 3:
                    continue
                kind, venue, listing = cols[:3]
                out.append({"kind": f"shape-{kind}", "spec": spec, "venue": venue, "venues": venue, "rawTitle": listing,
                            "what": cols[3] if len(cols) > 3 else "", "id": f"shape|{spec}|{kind}|{normalise(listing)}"})
    return merged(out)


def merged(findings):
    """One finding a billing (its id): a chain's listing at every venue billing it -- Cineworld's "Arrietty (4K
    Restoration) Experience It In IMAX" at twenty -- is one thing to triage, its venues listed together."""
    out = {}
    for f in findings:
        if f["id"] in out:
            kept = out[f["id"]]
            venues = kept["venues"].split(", ") + [v for v in f["venues"].split(", ") if v not in kept["venues"].split(", ")]
            kept["venues"] = ", ".join(venues)
        else:
            out[f["id"]] = dict(f)
    return list(out.values())


def still_unjudged(finding, labels, baseline):
    """Whether a suspect the recording found is still unjudged by the identity judgements as they stand now: a
    judgement made since the recording takes it off the report at once."""
    if finding["kind"] != "suspect":
        return True
    venues = [v for v in finding["venues"].split(", ") if v] or ["*"]
    if any(label_of(labels, venue, finding["rawTitle"], finding["film"]) is not None for venue in venues):
        return False
    return (normalise(finding["rawTitle"]), finding["film"]) not in baseline


def first_seen_of(body):
    """{finding's state key: first-seen date} a previous report's body carries."""
    match = STATE.search(body or "")
    if not match:
        return {}
    try:
        state = json.loads(match.group(1))
    except ValueError:
        return {}
    return {k: v for k, v in state.items() if isinstance(v, str)}


def commands(cc, finding, recording):
    q = shlex.quote
    venue, raw = finding["venue"], finding["rawTitle"]
    if finding["kind"] in ("suspect", "wrong"):
        at = [] if venue in ("", "*") else ["--venue", venue]
        return [" ".join(["scripts/identity-slice", cc] + [q(a) for a in at] + ["--title", q(raw), "--recording", recording]),
                " ".join(["scripts/identity-slice", cc, "--explain"] + [q(a) for a in at] + ["--title", q(raw)]),
                " ".join(["scripts/label-lookup", cc, "--title", q(raw)] + [q(a) for a in at])]
    trace = ["scripts/title-trace", q(raw), "--country", cc] + ([] if venue in ("", "*") else ["--venue", q(venue)])
    out = [" ".join(trace)]
    if venue not in ("", "*"):
        out.append(" ".join(["scripts/venue-replay", cc, q(venue), "--recording", recording, "--title", q(raw), "--fields"]))
    return out


SECTIONS = [
    ("suspect", "Suspect takes no label judges",
     "Judge the film into `test/resources/fixtures/identity-judgements.tsv` -- `right`, or `wrong` and fixed."),
    ("wrong", "Takes a label calls wrong",
     "The recording still takes a film labelled wrong: fix the rule that takes it."),
    ("shape-finding", "Corpus-shape findings",
     "A screening marker left in a query (SearchQueryMarkers) or a slot field read off the wrong node "
     "(CinemaSlotInvariants): fix the title rule or the parser, or allowlist the listing with why."),
    ("shape-stale", "Allowlist entries the recording no longer breaks",
     "Drop the entry from the spec's allowlist (the backlog only shrinks)."),
]


BODY_LIMIT = 60000  # GitHub refuses an issue body over 65,536 characters


def render(cc, findings, seen, today, max_age, run, run_url):
    """(issue body, overdue count): each billing in full, its triage commands too, while the body fits; the rest as one
    line each."""
    everything = 10 ** 6
    for detailed, listed in ((everything, everything), (40, everything), (15, everything), (5, everything), (0, everything),
                             (0, 200), (0, 100), (0, 30)):
        body, overdue = render_at(cc, findings, seen, today, max_age, run, run_url, detailed, listed)
        if len(body) <= BODY_LIMIT:
            break
    return body, overdue


def state_key(finding_id):
    """A finding's key in the body's state: short, so the state of hundreds of findings fits beside them."""
    return hashlib.sha1(finding_id.encode("utf-8")).hexdigest()[:16]


def render_at(cc, findings, seen, today, max_age, run, run_url, detailed, listed):
    def age(f):
        return (today - datetime.date.fromisoformat(seen[f["id"]])).days
    overdue = [f for f in findings if age(f) > max_age]
    lines = [f"# {title_of(cc)}", "",
             f"Recording [run {run}]({run_url}): **{len(findings)} finding(s)**, **{len(overdue)} untriaged for more than "
             f"{max_age} days**." if findings else
             f"Recording [run {run}]({run_url}): no finding to triage.", "",
             "These are the RECORDING's: no commit made them, so no build fails on them -- the identity takes check fails "
             "only what a commit's code makes, base against head. A finding untriaged for more than "
             f"{max_age} days turns the Identity findings report red. A suspect judged into identity-judgements.tsv leaves this issue on "
             "the next report; the rest leave when a recording no longer carries them. Updated in place by "
             "`scripts/ci/identity_findings_report.py`."]
    for kind, heading, how in SECTIONS:
        group = sorted((f for f in findings if f["kind"] == kind), key=lambda f: (-age(f), f["rawTitle"]))
        if not group:
            continue
        lines += ["", f"## {heading} ({len(group)})", "", how]
        for n, f in enumerate(group):
            flag = "⏰ " if age(f) > max_age else ""
            film = f" → {f['film']} {f.get('title', '')}".rstrip() if kind in ("suspect", "wrong") else ""
            where = f" · {f['spec']}" if f.get("spec") else ""
            facts = f"{f['venues']}{where} · first seen {seen[f['id']]} ({age(f)} d) · {f['what']}"
            if n < detailed:
                lines += ["", f"### {flag}{f['rawTitle']}{film}", facts, "", "```sh"] + commands(cc, f, run) + ["```"]
            elif n < listed:
                lines.append(f"- {flag}**{f['rawTitle']}**{film} -- {facts}")
            else:
                lines.append(f"- … and {len(group) - n} more: the run's `convergence-diagnostics-{cc}-convergence` artifact")
                break
    state = {state_key(f["id"]): seen[f["id"]] for f in findings}
    lines += ["", f"<!-- identity-findings-state: {json.dumps(state, ensure_ascii=False, sort_keys=True)} -->", ""]
    return "\n".join(lines), len(overdue)


def gh(args, stdin=None):
    """Run gh; its stdout. Raises on failure."""
    return subprocess.run(["gh"] + args, input=stdin, capture_output=True, text=True, check=True).stdout


def find_issue(cc, run_gh):
    """(number, state, body) of the country's issue, or None."""
    listed = json.loads(run_gh(["issue", "list", "--state", "all", "--search", f'in:title "{title_of(cc)}"',
                                "--json", "number,title,state,body", "--limit", "20"]) or "[]")
    exact = [i for i in listed if i["title"] == title_of(cc)]
    if not exact:
        return None
    issue = max(exact, key=lambda i: i["number"])
    return issue["number"], issue["state"], issue["body"]


def publish(cc, body, has_findings, run_gh, assignee):
    """Create, update, reopen or close the country's issue so it says `body`."""
    issue = find_issue(cc, run_gh)
    if issue is None:
        if has_findings:
            run_gh(["issue", "create", "--title", title_of(cc), "--body", body] + (["--assignee", assignee] if assignee else []))
            return "opened"
        return "none"
    number, state, _ = issue
    run_gh(["issue", "edit", str(number), "--body", body])
    if has_findings and state != "OPEN":
        run_gh(["issue", "reopen", str(number)])
        return "reopened"
    if not has_findings and state == "OPEN":
        run_gh(["issue", "close", str(number), "--comment", "No finding left to triage."])
        return "closed"
    return "updated"


def report(countries, run, directory_of, judgements_path, today, max_age, run_url, run_gh, assignee, dry_run):
    """(overdue total, countries read)."""
    overdue_total, read = 0, 0
    for cc in countries:
        found = read_findings(directory_of(cc), cc)
        if found is None:
            print(f"::warning::{title_of(cc)}: run {run} left no findings for {cc} -- its issue stays as it was")
            continue
        read += 1
        labels, _, baseline = judgements_of(judgements_path, cc)
        found = [f for f in found if still_unjudged(f, labels, baseline)]
        issue = None if dry_run else find_issue(cc, run_gh)
        seen = first_seen_of(issue[2] if issue else "")
        seen = {f["id"]: seen.get(state_key(f["id"]), today.isoformat()) for f in found}
        body, overdue = render(cc, found, seen, today, max_age, run, run_url)
        overdue_total += overdue
        outcome = "dry run" if dry_run else publish(cc, body, bool(found), run_gh, assignee)
        print(f"{title_of(cc)}: {len(found)} finding(s), {overdue} overdue -- issue {outcome}")
        if overdue:
            print(f"::error title={title_of(cc)}::{overdue} finding(s) untriaged for more than {max_age} days")
    return overdue_total, read


def download(run, cc, work, run_gh):
    """The row's diagnostics artifact of `run`, unpacked under `work`; the directory (empty when there is none)."""
    target = os.path.join(work, cc)
    os.makedirs(target, exist_ok=True)
    try:
        run_gh(["run", "download", str(run), "--name", f"convergence-diagnostics-{cc}-convergence", "--dir", target])
    except subprocess.CalledProcessError as e:
        print(f"::warning::run {run}: no convergence-diagnostics-{cc}-convergence artifact ({(e.stderr or '').strip()})")
    return target


def main(argv=None, run_gh=gh):
    p = argparse.ArgumentParser()
    p.add_argument("--run", required=True, help="the Record scrape fixtures run whose recording to report")
    p.add_argument("--countries", default=",".join(COUNTRIES))
    p.add_argument("--judgements", default="test/resources/fixtures/identity-judgements.tsv")
    p.add_argument("--today", help="YYYY-MM-DD, default today (UTC)")
    p.add_argument("--max-age-days", type=int, default=3)
    p.add_argument("--assignee", default="")
    p.add_argument("--server", default=os.environ.get("GITHUB_SERVER_URL", "https://github.com"))
    p.add_argument("--repo", default=os.environ.get("GITHUB_REPOSITORY", ""))
    p.add_argument("--work", default="identity-findings")
    p.add_argument("--diagnostics", help="read <dir>/<cc>/ instead of downloading the run's artifacts")
    p.add_argument("--dry-run", action="store_true", help="print the bodies' verdicts, touch no issue")
    a = p.parse_args(argv)
    today = datetime.date.fromisoformat(a.today) if a.today else datetime.datetime.now(datetime.timezone.utc).date()
    run_url = f"{a.server}/{a.repo}/actions/runs/{a.run}"
    def directory_of(cc):
        return os.path.join(a.diagnostics, cc) if a.diagnostics else download(a.run, cc, a.work, run_gh)
    overdue, read = report([c for c in a.countries.split(",") if c], a.run, directory_of, a.judgements, today,
                           a.max_age_days, run_url, run_gh, a.assignee, a.dry_run)
    if read == 0:
        print(f"::error::run {a.run} left no country's findings -- nothing was reported")
        return 2
    return 1 if overdue else 0


if __name__ == "__main__":
    sys.exit(main())
