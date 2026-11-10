#!/usr/bin/env python3
"""The external scheduler for the movies repo's daily and weekly GitHub Actions workflows.

WHY. GitHub's `schedule:` events are best effort, and on this repository they are worse than late:
identity-family-explorer.yml (cron '7 2 * * 0') was not run at all on Sunday 2026-11-08, with the
workflow active, and every scheduled run of 2026-10-31..11-08 started 3 to 10 hours after its slot
(nightly-stress 01:30 -> ~07:40, convergence-daily 03:23 -> ~10:30). The owner, that day: "write an
external cron trigger for the overnight jobs so we don't depend on github".

ONE SOURCE OF TRUTH: the workflows' own `on.schedule` crons. `generate` reads every
.github/workflows/*.yml and writes infra/nix/files/scheduled-workflows.json, which
modules/roles/scheduled-dispatch.nix turns into one systemd timer per workflow (OnCalendar in UTC).
`check` fails when that file, or a workflow's wiring, has drifted from the crons
(scheduled_dispatch_test.py runs it over the repository, so a cron edited without regenerating is
red in the script tests).

COVERED: a workflow whose every cron fires at most once a day (one minute, one hour). A sub-daily
one (decodo-probe every 30 min, security-headers every 6 h) is left to GitHub: its next slot is the
recovery from a dropped one, and a grace period as long as its interval would double every run.

THE DISPATCH. At each slot the timer starts `run <stem>`, which waits out a short grace period, then
asks GitHub for the workflow's runs created since the slot. A run started by the `schedule` event
(GitHub on time) or by an earlier dispatch of ours means the slot ran. Otherwise it sends the
repository_dispatch event `scheduled-<stem>`, which each covered workflow listens to and treats
exactly as its schedule (its scheduled-run conditions read `github.event_name == 'schedule' ||
github.event_name == 'repository_dispatch'`). Not a workflow_dispatch: a dispatched run gets its
inputs' DEFAULTS, a scheduled one gets them EMPTY, and every workflow here was written for the
second; a repository_dispatch run has no inputs either, so it takes the scheduled path unchanged.

THE GUARD. GitHub's scheduler stays on, as the backstop for this host being down -- so when it then
starts the slot's run hours late, that run is a duplicate: for --guard-hours after the slot (never
past the next one) the service polls every two minutes and cancels a not-yet-completed `schedule`
run of the slot, only while a run of ours exists that was not cancelled (GitHub's late run, on
arriving in the workflow's concurrency group, cancels ours if ours is still pending there). A cancelled run says nothing to the red gate and
skips report-scheduled-run (`!cancelled()`). The cost: a late run may have done a minute or two of
its plan job before it is cancelled. The guard ends as soon as it has judged the slot's schedule run
(GitHub starts one at most): each poll is a request of the App installation's 5,000 an hour.

Bounded retries (an API error is retried, re-checking the runs first, since a dispatch whose
answer was lost may still have landed). A RATE-LIMITED refusal (403/429 with x-ratelimit-remaining 0,
retry-after, or a rate-limit message) costs no attempt: it waits for the reset GitHub names, up to the
guard's end (2026-11-09: the four attempts spent in six minutes of an exhausted quota lost the nightly
recording), logged as `rate-limited until=<iso> waiting <n> s`. Every decision is one journal line, shipped to
VictoriaLogs:
    scheduled-dispatch: workflow=<file> slot=<iso> outcome=ran run=<id> event=schedule late_minutes=<n>
    scheduled-dispatch: workflow=<file> slot=<iso> outcome=dispatched-earlier run=<id>
    scheduled-dispatch: workflow=<file> slot=<iso> outcome=missed-dispatched
    scheduled-dispatch: workflow=<file> slot=<iso> outcome=late-duplicate-cancelled run=<id> late_minutes=<n>
    scheduled-dispatch: workflow=<file> slot=<iso> outcome=dispatch-failed error=<...>
`dispatch-failed` exits 1, so the unit is failed and host-health.rules' SystemdUnitFailed pages.

CREDENTIAL: the kinowo-lander GitHub App (installed on movies; Contents RW, which a
repository_dispatch needs), the identity agentctl already pushes and dispatches as on ci-runner
(bitcashier infra/nix/hosts/ci-runner/github-apps.nix). Its installation token is minted the way
agentctl's lib/agentctl/ghapp.py does: a JWT signed RS256 by `openssl dgst`, no Python crypto.
"""
import argparse
import base64
import datetime
import json
import os
import re
import subprocess
import sys
import time
import urllib.error
import urllib.request

API = "https://api.github.com"
SCHEDULED_EVENTS = ("schedule", "repository_dispatch")
EVENT_TYPE_PREFIX = "scheduled-"
JSON_PATH = "infra/nix/files/scheduled-workflows.json"
WORKFLOWS_DIR = ".github/workflows"
DAYS = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
SCHEDULE_EQ = "github.event_name == 'schedule'"
SCHEDULE_NE = "github.event_name != 'schedule'"
DISPATCH_EQ = "(github.event_name == 'schedule' || github.event_name == 'repository_dispatch')"
# The heavy measure-class jobs (the owner, 2026-11-08): each in a clock hour no other daily or weekly job starts in, on
# any day of the week.
HEAVY = ("record-scrape-fixtures", "identity-drift-explorer", "nightly-stress", "order-independence",
         "convergence-daily", "identity-family-explorer", "identity-capture", "identity-refit")
DISPATCH_NE = "(github.event_name != 'schedule' && github.event_name != 'repository_dispatch')"


class ApiError(Exception):
    """A call GitHub would not answer. `retry_at` (a UTC datetime) when it refused for a rate limit: retrying before
    then is refused again."""

    def __init__(self, message, retry_at=None):
        super().__init__(message)
        self.retry_at = retry_at


# A rate-limited call is retried this long after the reset GitHub names, and never sooner than a minute after the
# refusal (GitHub's advice for a secondary limit that names no time).
RATE_LIMIT_MARGIN_SECONDS = 5
RATE_LIMIT_MIN_WAIT_SECONDS = 60


# ---------------------------------------------------------------- cron

FIELDS = [("minute", 0, 59), ("hour", 0, 23), ("day of month", 1, 31), ("month", 1, 12), ("day of week", 0, 7)]


def _field(text, name, low, high):
    """The values one cron field matches, or None for `*` (every value)."""
    if text == "*":
        return None
    values = set()
    for part in text.split(","):
        step = 1
        if "/" in part:
            part, step_text = part.split("/", 1)
            step = int(step_text)
        if part == "*":
            start, end = low, high
        elif "-" in part:
            start, end = (int(x) for x in part.split("-", 1))
        else:
            start = int(part)
            end = high if step > 1 else start
        if not (low <= start <= end <= high) or step < 1:
            raise ValueError(f"cron {name} out of range: {text!r}")
        values.update(range(start, end + 1, step))
    if name == "day of week" and 7 in values:
        values = (values - {7}) | {0}
    return values


def parse_cron(cron):
    """[minutes, hours, days of month, months, days of week]: each a set, or None for `*`."""
    parts = cron.split()
    if len(parts) != 5:
        raise ValueError(f"cron needs five fields: {cron!r}")
    return [_field(p, *spec) for p, spec in zip(parts, FIELDS)]


def is_daily_or_rarer(cron):
    minutes, hours = parse_cron(cron)[:2]
    return minutes is not None and hours is not None and len(minutes) == 1 and len(hours) == 1


def _join(values, width=2):
    return "*" if values is None else ",".join(str(v).zfill(width) for v in sorted(values))


def on_calendar(cron):
    """systemd OnCalendar expressions (UTC) firing exactly when the cron does. Cron ORs a restricted day of month
    with a restricted day of week; OnCalendar ANDs them, so that case is two expressions."""
    minutes, hours, days, months, weekdays = parse_cron(cron)
    clock = f"{_join(hours)}:{_join(minutes)}:00 UTC"

    def expression(day_values, weekday_values):
        weekday = "" if weekday_values is None else ",".join(DAYS[d] for d in sorted(weekday_values)) + " "
        return f"{weekday}*-{_join(months)}-{_join(day_values)} {clock}"

    if days is not None and weekdays is not None:
        return [expression(days, None), expression(None, weekdays)]
    return [expression(days, weekdays)]


def matches(cron, at):
    minutes, hours, days, months, weekdays = parse_cron(cron)
    weekday = (at.weekday() + 1) % 7  # Python's Monday=0 -> cron's Sunday=0

    def hit(values, value):
        return values is None or value in values

    if days is not None and weekdays is not None:
        day_ok = at.day in days or weekday in weekdays
    else:
        day_ok = hit(days, at.day) and hit(weekdays, weekday)
    return hit(minutes, at.minute) and hit(hours, at.hour) and hit(months, at.month) and day_ok


def latest_slot(crons, now):
    """The newest minute <= now that any of the crons fires at (looked for over the last 32 days)."""
    at = now.replace(second=0, microsecond=0)
    for _ in range(32 * 24 * 60):
        if any(matches(c, at) for c in crons):
            return at
        at -= datetime.timedelta(minutes=1)
    raise ValueError(f"no slot of {crons} in the last 32 days")


def next_slot(crons, after):
    """The first minute > after that any of the crons fires at."""
    at = after.replace(second=0, microsecond=0) + datetime.timedelta(minutes=1)
    for _ in range(32 * 24 * 60):
        if any(matches(c, at) for c in crons):
            return at
        at += datetime.timedelta(minutes=1)
    raise ValueError(f"no slot of {crons} in the next 32 days")


# ---------------------------------------------------------------- the workflows (no PyYAML: macOS's python3 has none)

def _indent(line):
    return len(line) - len(line.lstrip(" "))


def _on_block(text):
    """The lines of the workflow's top-level `on:` block."""
    lines = text.splitlines()
    for i, line in enumerate(lines):
        if re.match(r"^(on|\"on\"|'on'):\s*(#.*)?$", line):
            block = []
            for nxt in lines[i + 1:]:
                if nxt.strip() and not nxt.startswith((" ", "#")):
                    break
                block.append(nxt)
            return block
    return []


def _sub_block(block, key):
    """The lines under `key:` within an `on:` block, or None when the key is absent."""
    for i, line in enumerate(block):
        if line.strip().split("#")[0].strip() == f"{key}:":
            depth = _indent(line)
            out = []
            for nxt in block[i + 1:]:
                if nxt.strip() and not nxt.strip().startswith("#") and _indent(nxt) <= depth:
                    break
                out.append(nxt)
            return out
    return None


def workflow_crons(text):
    schedule = _sub_block(_on_block(text), "schedule") or []
    return [m.group(2) for m in (re.match(r"\s*-\s*cron:\s*(['\"])(.+?)\1", line) for line in schedule) if m]


def dispatch_types(text):
    """The repository_dispatch types a workflow listens to ([] when it does not)."""
    block = _sub_block(_on_block(text), "repository_dispatch")
    if block is None:
        return []
    for line in block:
        m = re.match(r"\s*types:\s*\[(.*)\]", line)
        if m:
            return [t.strip().strip("'\"") for t in m.group(1).split(",") if t.strip()]
    return []


def read_workflows(root):
    folder = os.path.join(root, WORKFLOWS_DIR)
    out = {}
    for name in sorted(os.listdir(folder)):
        if name.endswith((".yml", ".yaml")):
            with open(os.path.join(folder, name)) as f:
                out[name] = f.read()
    return out


def stem(file_name):
    return file_name.rsplit(".", 1)[0]


def generate(workflows):
    covered, excluded = {}, {}
    for name, text in sorted(workflows.items()):
        crons = workflow_crons(text)
        if not crons:
            continue
        if all(is_daily_or_rarer(c) for c in crons):
            covered[stem(name)] = {"file": name, "crons": crons,
                                   "onCalendar": [e for c in crons for e in on_calendar(c)]}
        else:
            excluded[name] = "fires more than once a day: its next slot recovers a dropped one"
    return {"generatedBy": "infra/nix/files/scheduled_dispatch.py generate (do not edit by hand)",
            "workflows": covered, "excluded": excluded}


def render(document):
    return json.dumps(document, indent=2, sort_keys=True) + "\n"


def start_hours(cron):
    """{(day of week, hour)} the cron starts a run in. A restricted day of month can fall on any day of the week."""
    _, hours, days, _, weekdays = parse_cron(cron)
    if weekdays is None or days is not None:
        weekdays = set(range(7))
    return {(d, h) for d in weekdays for h in (range(24) if hours is None else hours)}


def staggering(covered):
    """The owner, 2026-11-08: "stagger the daily and weekly jobs hour-wise". No two covered (daily or weekly) workflows
    start in the same clock hour on any day, and a HEAVY one's hour is its own on every day."""
    found = []
    slots = {name: set().union(*(start_hours(c) for c in w["crons"])) for name, w in covered.items()}
    names = sorted(slots)
    for i, a in enumerate(names):
        for b in names[i + 1:]:
            shared = slots[a] & slots[b]
            if shared:
                day, hour = min(shared)
                found.append(f"{a} and {b} both start in hour {hour:02d} UTC ({DAYS[day]}): one daily or weekly job an hour")
            elif a in HEAVY or b in HEAVY:
                hours = {h for _, h in slots[a]} & {h for _, h in slots[b]}
                if hours:
                    found.append(f"{a} and {b} share hour {min(hours):02d} UTC on different days: "
                                 f"{a if a in HEAVY else b} is a heavy measure job, its hour its own every day")
    return found


def problems(root):
    """Everything that has drifted: the generated file, and each workflow's dispatch wiring and conditions."""
    workflows = read_workflows(root)
    document = generate(workflows)
    found = []
    try:
        with open(os.path.join(root, JSON_PATH)) as f:
            current = f.read()
    except OSError:
        current = ""
    if current != render(document):
        found.append(f"{JSON_PATH} is not what the workflows' crons generate: "
                     "run `python3 infra/nix/files/scheduled_dispatch.py generate`")
    found += staggering(document["workflows"])
    covered = {w["file"] for w in document["workflows"].values()}
    for name, text in sorted(workflows.items()):
        types = dispatch_types(text)
        scheduled_types = [t for t in types if t.startswith(EVENT_TYPE_PREFIX)]
        want = EVENT_TYPE_PREFIX + stem(name)
        if name in covered:
            if scheduled_types != [want]:
                found.append(f"{name}: `on: repository_dispatch: types:` must list exactly [{want}] "
                             f"(the external scheduler's event), not {scheduled_types}")
            bare = (text.replace(DISPATCH_EQ, "").replace(DISPATCH_NE, ""))
            for needle, fix in ((SCHEDULE_EQ, DISPATCH_EQ), (SCHEDULE_NE, DISPATCH_NE)):
                if needle in bare:
                    found.append(f"{name}: `{needle}` misses the external scheduler's runs: write `{fix}`")
            if "github.event.schedule" in text:
                found.append(f"{name}: reads github.event.schedule, which a repository_dispatch run does not carry")
        elif scheduled_types:
            found.append(f"{name}: listens to {scheduled_types} but is not covered by the external scheduler")
    return found


# ---------------------------------------------------------------- GitHub

def _b64url(data):
    return base64.urlsafe_b64encode(data).rstrip(b"=").decode()


def rate_limit_reset(status, headers, body, now):
    """When a refused call may be retried (a UTC datetime), or None when the refusal is not a rate limit. A primary
    limit is a 403/429 with `x-ratelimit-remaining: 0` and the reset epoch in `x-ratelimit-reset`; a secondary one
    carries `retry-after` seconds, or only its message."""
    if status not in (403, 429):
        return None
    header = {k.lower(): v for k, v in (headers or {}).items()}
    exhausted = header.get("x-ratelimit-remaining") == "0"
    if not (exhausted or "retry-after" in header or "rate limit" in (body or "").lower()):
        return None
    try:
        if "retry-after" in header:
            epoch = now + int(header["retry-after"])
        elif exhausted and "x-ratelimit-reset" in header:
            epoch = int(header["x-ratelimit-reset"])
        else:
            epoch = now + RATE_LIMIT_MIN_WAIT_SECONDS
    except ValueError:
        epoch = now + RATE_LIMIT_MIN_WAIT_SECONDS
    return datetime.datetime.fromtimestamp(epoch, datetime.timezone.utc)


def app_jwt(app_id, key_file, now):
    head = _b64url(json.dumps({"alg": "RS256", "typ": "JWT"}, separators=(",", ":")).encode())
    body = _b64url(json.dumps({"iat": int(now) - 60, "exp": int(now) + 540, "iss": str(app_id)},
                              separators=(",", ":")).encode())
    signing = f"{head}.{body}".encode()
    signed = subprocess.run(["openssl", "dgst", "-sha256", "-sign", key_file], input=signing,
                            capture_output=True, timeout=30)
    if signed.returncode != 0 or not signed.stdout:
        raise ApiError(f"openssl could not sign with the App key: {signed.stderr.decode(errors='replace')[:200]}")
    return f"{head}.{body}.{_b64url(signed.stdout)}"


class GitHub:
    """The calls the dispatcher makes, as an installation of a GitHub App."""

    def __init__(self, repository, app_id, key_file, api=API):
        self.repository, self.app_id, self.key_file, self.api = repository, app_id, key_file, api
        self._token, self._minted = None, 0.0

    def _http(self, method, path, bearer, body=None):
        request = urllib.request.Request(
            f"{self.api}/{path}", method=method,
            data=json.dumps(body).encode() if body is not None else None,
            headers={"Authorization": f"Bearer {bearer}", "Accept": "application/vnd.github+json",
                     "X-GitHub-Api-Version": "2022-11-28", "User-Agent": "kinowo-scheduled-dispatch",
                     "Content-Type": "application/json"})
        try:
            with urllib.request.urlopen(request, timeout=30) as response:
                return json.loads(response.read() or b"null")
        except urllib.error.HTTPError as e:
            body = e.read()[:500].decode(errors="replace")
            raise ApiError(f"{method} {path}: HTTP {e.code} {body[:200]}",
                           retry_at=rate_limit_reset(e.code, dict(e.headers or {}), body, time.time())) from None
        except (urllib.error.URLError, OSError, ValueError) as e:
            raise ApiError(f"{method} {path}: {e}") from None

    def token(self):
        """An installation token, re-minted after 50 of its 60 minutes (the guard outlives one)."""
        if self._token is None or time.time() - self._minted > 3000:
            self._minted = time.time()
            jwt = app_jwt(self.app_id, self.key_file, time.time())
            installation = self._http("GET", f"repos/{self.repository}/installation", jwt)["id"]
            granted = self._http("POST", f"app/installations/{installation}/access_tokens", jwt,
                                 {"repositories": [self.repository.split("/", 1)[1]]})
            self._token = granted["token"]
        return self._token

    def runs_since(self, workflow_file, since):
        created = since.strftime("%Y-%m-%dT%H:%M:%SZ")
        answer = self._http("GET", f"repos/{self.repository}/actions/workflows/{workflow_file}/runs"
                                   f"?created=%3E%3D{created}&per_page=100", self.token())
        return answer.get("workflow_runs") or []

    def cancel(self, run_id):
        self._http("POST", f"repos/{self.repository}/actions/runs/{run_id}/cancel", self.token())

    def dispatch(self, event_type, payload):
        self._http("POST", f"repos/{self.repository}/dispatches", self.token(),
                   {"event_type": event_type, "client_payload": payload})


# ---------------------------------------------------------------- the decision

def iso(at):
    return at.strftime("%Y-%m-%dT%H:%M:%SZ")


def _created(run):
    return datetime.datetime.strptime(run["created_at"], "%Y-%m-%dT%H:%M:%SZ").replace(tzinfo=datetime.timezone.utc)


class Slot:
    """One slot of one workflow, settled against GitHub: run it if GitHub did not, then cancel GitHub's late copy."""

    def __init__(self, github, workflow_file, slot, log, clock, sleep, attempts=4, backoff=60, poll=120):
        self.github, self.file, self.slot, self.log = github, workflow_file, slot, log
        self.clock, self.sleep, self.attempts, self.backoff, self.poll = clock, sleep, attempts, backoff, poll
        self.head = f"scheduled-dispatch: workflow={workflow_file} slot={iso(slot)}"

    def _runs(self):
        return [r for r in self.github.runs_since(self.file, self.slot) if r.get("event") in SCHEDULED_EVENTS]

    def ensure(self, deadline):
        """'ran' (GitHub's schedule started it), 'dispatched-earlier' (an earlier dispatch of ours did),
        'missed-dispatched' (neither: dispatched now) or 'dispatch-failed' (the API would not say or take it).
        A rate-limited refusal costs no attempt: it waits for the reset GitHub names, while that is before
        `deadline` (the guard's end: a dispatch later than that would collide with GitHub's late run or the next
        slot). 2026-11-09: four fixed attempts spent in 6 minutes of an exhausted quota lost the nightly recording."""
        error = ""
        attempt = 0
        while attempt < self.attempts:
            try:
                runs = sorted(self._runs(), key=lambda r: r.get("created_at") or "")
                ours = [r for r in runs if r["event"] == "repository_dispatch"]
                if ours:
                    self.log(f"{self.head} outcome=dispatched-earlier run={ours[0].get('id')}")
                    return "dispatched-earlier"
                if runs:
                    self.log(f"{self.head} outcome=ran run={runs[0].get('id')} event=schedule "
                             f"late_minutes={int((_created(runs[0]) - self.slot).total_seconds() // 60)}")
                    return "ran"
                self.github.dispatch(EVENT_TYPE_PREFIX + stem(self.file),
                                     {"slot": iso(self.slot), "source": "scheduled-dispatch"})
                self.log(f"{self.head} outcome=missed-dispatched")
                return "missed-dispatched"
            except ApiError as e:
                error = str(e).replace("\n", " ")
                if e.retry_at is not None:
                    wait = max((e.retry_at - self.clock()).total_seconds() + RATE_LIMIT_MARGIN_SECONDS,
                               RATE_LIMIT_MIN_WAIT_SECONDS)
                    if self.clock() + datetime.timedelta(seconds=wait) >= deadline:
                        self.log(f"{self.head} rate-limited until={iso(e.retry_at)} past the guard's end "
                                 f"{iso(deadline)} api-error={error}")
                        break
                    self.log(f"{self.head} rate-limited until={iso(e.retry_at)} waiting {int(wait)} s "
                             f"api-error={error}")
                    self.sleep(wait)
                    continue
                attempt += 1
                self.log(f"{self.head} attempt={attempt}/{self.attempts} api-error={error}")
                if attempt < self.attempts:
                    self.sleep(self.backoff * attempt)
        self.log(f"{self.head} outcome=dispatch-failed error={error}")
        return "dispatch-failed"

    def guard(self, until):
        """Until `until`, cancel every schedule run of the slot that GitHub starts after ours (it starts them hours
        late: 3-10 h in 2026-11), so the slot runs once. Only while a run of ours exists that was not cancelled: never
        the slot's only run. Ours waiting in its concurrency group is cancelled by GitHub's late run of the same group
        arriving (a pending run is superseded; prune-actions-caches cancels in progress too), and GitHub's is then the
        slot's run. GitHub starts a slot's schedule run at most once, so the guard ends once it has judged it against
        ours (cancelled, or kept because ours was superseded): every poll is a request of the App installation's hourly quota. A
        rate-limited poll waits for the reset rather than polling into it."""
        cancelled = set()
        while self.clock() < until:
            wait = self.poll
            try:
                runs = self._runs()
                if any(r["event"] == "repository_dispatch" and r.get("conclusion") != "cancelled" for r in runs):
                    for r in runs:
                        if r["event"] == "schedule" and r.get("status") != "completed" and r["id"] not in cancelled:
                            self.github.cancel(r["id"])
                            cancelled.add(r["id"])
                            self.log(f"{self.head} outcome=late-duplicate-cancelled run={r['id']} "
                                     f"late_minutes={int((_created(r) - self.slot).total_seconds() // 60)}")
                events = {r["event"] for r in runs}
                if {"schedule", "repository_dispatch"} <= events:  # ours listed too: the schedule run was judged
                    break
            except ApiError as e:
                self.log(f"{self.head} guard api-error={str(e).replace(chr(10), ' ')}")
                if e.retry_at is not None:
                    wait = max(wait, (e.retry_at - self.clock()).total_seconds() + RATE_LIMIT_MARGIN_SECONDS)
            self.sleep(wait)
        return sorted(cancelled)


def settle(github, workflow_file, crons, now, grace_minutes, guard_hours, log, clock, sleep):
    """Wait out the grace after the latest slot, make sure it ran, and guard it. The exit status."""
    slot = latest_slot(crons, now)
    wait = (slot + datetime.timedelta(minutes=grace_minutes) - now).total_seconds()
    if wait > 0:
        log(f"scheduled-dispatch: workflow={workflow_file} slot={iso(slot)} waiting {int(wait)} s of grace")
        sleep(wait)
    guard_end = min(slot + datetime.timedelta(hours=guard_hours), next_slot(crons, slot))
    one = Slot(github, workflow_file, slot, log, clock, sleep)
    outcome = one.ensure(guard_end)
    if outcome in ("missed-dispatched", "dispatched-earlier"):
        one.guard(guard_end)
    return 1 if outcome == "dispatch-failed" else 0


def run(args, clock=None, sleep=time.sleep, log=None):
    with open(args.schedule) as f:
        entry = json.load(f)["workflows"][args.workflow]
    clock = clock or (lambda: datetime.datetime.now(datetime.timezone.utc))
    log = log or (lambda line: print(line, flush=True))
    key_file = args.key_file or os.path.join(os.environ.get("CREDENTIALS_DIRECTORY", ""), "github-app-key")
    github = GitHub(args.repository, args.app_id, key_file)
    return settle(github, entry["file"], entry["crons"], clock(), args.grace_minutes, args.guard_hours, log, clock,
                  sleep)


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    sub = parser.add_subparsers(dest="command", required=True)
    gen = sub.add_parser("generate", help=f"write {JSON_PATH} from the workflows' crons")
    gen.add_argument("--root", default=".")
    chk = sub.add_parser("check", help="fail when the generated file or a workflow's wiring has drifted")
    chk.add_argument("--root", default=".")
    rn = sub.add_parser("run", help="settle a workflow's latest slot: wait the grace, dispatch if GitHub did not run it")
    rn.add_argument("workflow", help="the workflow's file stem, a key of the schedule file")
    rn.add_argument("--schedule", required=True, help="scheduled-workflows.json")
    rn.add_argument("--repository", required=True)
    rn.add_argument("--app-id", required=True)
    rn.add_argument("--key-file", help="the App's private key (default: the systemd credential github-app-key)")
    rn.add_argument("--grace-minutes", type=int, default=15)
    rn.add_argument("--guard-hours", type=int, default=14,
                    help="how long after the slot GitHub's late schedule run is cancelled (never past the next slot)")
    args = parser.parse_args(argv)
    if args.command == "generate":
        with open(os.path.join(args.root, JSON_PATH), "w") as f:
            f.write(render(generate(read_workflows(args.root))))
        return 0
    if args.command == "check":
        found = problems(args.root)
        for p in found:
            print(f"scheduled-dispatch check: {p}", file=sys.stderr)
        return 1 if found else 0
    return run(args)


if __name__ == "__main__":
    sys.exit(main())
