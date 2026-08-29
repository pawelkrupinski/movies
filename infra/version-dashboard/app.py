#!/usr/bin/env python3
"""What every NixOS host in the kinowo fleet is actually running, and how far behind main it is.

MODELLED ON ~/bitcashier/version-dashboard/app.py, and the important thing it copies is WHERE THE
DATA COMES FROM. This page does not ssh to each host to ask what it is running. It joins two
sources:

  the ROSTER, from the flake     -- who is supposed to exist, via one `nix eval`
  the RUNNING STATE, from Prometheus -- what each host says it is running, via the `nixos_*` metrics
                                       that modules/fleet/observability.nix publishes through
                                       node_exporter's textfile collector

That split is the whole point, and it is worth stating because the obvious design is worse. A page
that ssh'd everywhere would be slow, would need credentials, and -- the real problem -- would report
a host as FINE right up until it stopped answering, then report nothing at all. Reading the metrics
means a host that has stopped publishing is VISIBLE AS SILENT, which is a different and more useful
statement than "unreachable": it says the machine stopped telling us, not that we failed to ask.

THE ONE THING IT DOES BESIDES READ, and the narrow shape of it. Ported from bitcashier's "Bring to
latest…" button, but deliberately smaller: this page NEVER BUILDS AND NEVER EVALUATES ANYTHING FOR
A HOST. CI has already staged a signed closure at /var/lib/nixdeploy/staged-system and
nixos-auto-apply already refuses to activate it whenever doing so would disturb a unit -- on this
fleet the reason is almost always `units_would_change`. So the button's whole job is the step
auto-apply declines to take by itself: activate that exact closure, accepting the unit restarts.
Nothing else. It cannot reach a revision CI has not staged, so it is not a third path to
production; it is the manual half of the path that already exists.

It runs the same two commands, in the same order, as nix/files/nixos-auto-apply.py --
`nix-env -p /nix/var/nix/profiles/system --set <staged>` and then
`<staged>/bin/switch-to-configuration switch` -- and that ORDER is load-bearing:
switch-to-configuration re-reads the system profile, so activating without setting it first leaves
the running system pointing at the closure it just replaced.

RUNNING IT

    python3 infra/version-dashboard/app.py          # serve on http://127.0.0.1:8788/nixos
    python3 infra/version-dashboard/app.py --once   # print the page's data as text and exit

`--once` reads and prints, and that is all: it never serves, so nothing can post to it, and it
starts no action of its own. It is safe to run from a script or a cron job.

Stdlib only, on the system python3 -- no venv, no requirements.txt. It needs `nix` on PATH for the
roster and `ssh` for the Prometheus read.
"""

import concurrent.futures
import html
import json
import os
import re
import shlex
import subprocess
import sys
import threading
import time
import urllib.parse
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

HERE = os.path.dirname(os.path.abspath(__file__))
INFRA_DIR = os.path.dirname(HERE)

# 8788, NOT 8787. bitcashier's version-dashboard already holds 8787 on this workstation, and two
# processes racing for one port means whichever loses dies at boot -- silently, since launchd just
# retries. Different estate, different port.
PORT = int(os.environ.get("KINOWO_DASHBOARD_PORT", "8788"))

# HOW PROMETHEUS IS REACHED, and why it is not a plain URL.
#
# Prometheus binds its PRIVATE address only (roles/prometheus.nix: never 0.0.0.0, because its web UI
# has no authentication). That subnet is not routable from a laptop, and this fleet has no VPN and
# no jump host -- so the only path from here is ssh to monitoring-1's public address and curl from
# there. That is one extra hop and it is the reason the cache below exists.
MONITORING_SSH = os.environ.get("KINOWO_MONITORING_SSH", "root@2.28.52.210")
PROM_URL = os.environ.get("KINOWO_PROM_URL", "http://10.20.0.11:9090")

# The series the page reads. One query, not one per metric: Prometheus returns them all in a single
# instant vector and joining locally is far cheaper than a dozen round trips down an ssh pipe.
PROM_SELECTOR = '{__name__=~"nixos_.*|node_os_info"}'

SSH_OPTS = ["-o", "BatchMode=yes", "-o", "ConnectTimeout=8", "-o", "StrictHostKeyChecking=accept-new"]

CACHE_TTL = 30.0        # how long a built page is served without rebuilding
STALE_GRACE = 300.0     # past this, the page says out loud that it is stale rather than looking fresh
NIX_TIMEOUT = 240
SSH_TIMEOUT = 30
GIT_TIMEOUT = 30

# THE ACTION HALF -- everything below this line only matters once somebody presses a button.

# Where CI pins the closure it staged. It is the same default as
# nix/modules/fleet/auto-apply.nix's `stagedSystem`, restated rather than read out of the flake
# because this process must be able to name it while a `nix eval` is failing -- the moment the
# roster cannot be read is exactly the moment somebody wants the button.
STAGED_PIN = "/var/lib/nixdeploy/staged-system"

# ROOT, not an admin over sudo. This is where this fleet differs from bitcashier's, whose hosts
# grant root no keys at all and whose scripts are therefore full of `sudo -n`. Here root ssh is the
# only access there is -- it is already how this page reads Prometheus, above -- so the scripts
# below run their commands bare. If that ever changes, the change is `sudo -n` in two scripts and
# a user here, not a redesign.
FLEET_SSH_USER = os.environ.get("KINOWO_FLEET_SSH_USER", "root")

# A store path this tool will consent to activate, ported verbatim in intent from the reference:
# the switch phase is handed a path, and this pattern plus the "a check must have offered it" rule
# in handle_fleet_apply are the two things that keep that endpoint from being an activate-any-path
# endpoint. Neither alone is enough -- the pattern would still admit any well-formed store path
# that happens to exist on the host.
CLOSURE_PATH_RE = re.compile(r"^/nix/store/[a-z0-9]{32}-nixos-system-[A-Za-z0-9._+-]+$")

LOG_LINE_CAP = 4000     # a runaway switch must not grow this process's memory without bound
CHECK_TIMEOUT = 300     # ssh + a dry-activate; ported from the reference, same work
SWITCH_TIMEOUT = 1800   # a real switch, including whatever the activation scripts do
JOB_RETENTION = 40      # finished jobs kept for their logs; see _remember_job

# ROLES THAT DEMAND A SECOND, TYPED CONFIRMATION rather than one click.
#
# Keyed on `fleet.role`, NOT on the hostname, and that is the point: a second database host added
# tomorrow inherits the guard by declaring `role = "mongo"`, whereas a hostname list would let it
# through silently. mongo-1 is the production database. Activating a closure there can restart
# mongod, and the Fly-hosted web tier holds CHANGE STREAMS against it (see the replSetName comment
# in nix/hosts/mongo-1/default.nix -- a change stream that drops does not come back by itself), so
# a restart here is not "some units bounce", it is the read path going quiet until the app
# reconnects. That is a risk, not an incident that happened; it is stated as a risk.
CONFIRM_ROLES = {"mongo"}

_cache_lock = threading.Lock()
_cache = {"built_at": 0.0, "data": None, "building": False}


def run(argv, cwd=None, timeout=30):
    """Run a command, returning (ok, stdout, stderr). Never raises -- every caller here would only
    turn an exception back into an error string for the page, and a dashboard that 500s during an
    incident is one more thing to debug at the wrong moment."""
    try:
        p = subprocess.run(argv, cwd=cwd, timeout=timeout,
                           stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        return p.returncode == 0, p.stdout, p.stderr.strip()
    except subprocess.TimeoutExpired:
        return False, "", f"timed out after {timeout}s"
    except Exception as exc:  # noqa: BLE001
        return False, "", f"{type(exc).__name__}: {exc}"


# --------------------------------------------------------------------------------------------
# THE ROSTER -- who is supposed to exist
# --------------------------------------------------------------------------------------------

def flake_machines():
    """Every host the flake declares, with the addresses and role it declares for them.

    THE FLAKE IS THE ROSTER, NOT PROMETHEUS, and the direction matters. Deriving the list from what
    is currently reporting would make a host that has died disappear from the page entirely -- the
    single most important thing it could tell you, rendered as an absence nobody notices. Taking the
    roster from the flake means a declared host that stops publishing shows up as a row that says
    so.
    """
    ok, out, err = run([
        "nix", "--extra-experimental-features", "nix-command flakes",
        "eval", "--json", ".#nixosConfigurations", "--apply",
        "cfgs: builtins.mapAttrs (n: c: {"
        " hostName = c.config.networking.hostName;"
        " privateAddress = c.config.fleet.privateAddress;"
        " publicAddress = c.config.fleet.publicAddress;"
        " role = c.config.fleet.role;"
        "}) cfgs",
    ], cwd=INFRA_DIR, timeout=NIX_TIMEOUT)
    if not ok:
        return {}, f"nix eval failed: {err.splitlines()[-1] if err else 'unknown'}"
    try:
        return json.loads(out), None
    except json.JSONDecodeError as exc:
        return {}, f"nix eval returned unparseable JSON: {exc}"


def git_head():
    ok, out, _ = run(["git", "rev-parse", "HEAD"], cwd=INFRA_DIR, timeout=GIT_TIMEOUT)
    head = out.strip() if ok else ""
    ok, out, _ = run(["git", "status", "--porcelain", "-uno"], cwd=INFRA_DIR, timeout=GIT_TIMEOUT)
    dirty = bool(out.strip()) if ok else False
    ok, out, _ = run(["git", "rev-parse", "origin/main"], cwd=INFRA_DIR, timeout=GIT_TIMEOUT)
    origin = out.strip() if ok else ""
    return head, dirty, origin


def commits_between(a, b):
    """How many commits b is ahead of a. Used only to phrase 'behind by N', so a failure is
    cosmetic and returns None rather than an error."""
    if not a or not b or a == b:
        return 0 if a == b else None
    ok, out, _ = run(["git", "rev-list", "--count", f"{a}..{b}"], cwd=INFRA_DIR, timeout=GIT_TIMEOUT)
    if not ok:
        return None
    try:
        return int(out.strip())
    except ValueError:
        return None


# --------------------------------------------------------------------------------------------
# THE RUNNING STATE -- what each host says about itself
# --------------------------------------------------------------------------------------------

def prom_series():
    """One instant query, fetched by ssh'ing to monitoring-1 and curling its own Prometheus.

    The `--` before the remote command is not decoration: without it an address beginning with a
    dash would be read by ssh as a flag.
    """
    url = f"{PROM_URL}/api/v1/query?query=" + _urlquote(PROM_SELECTOR)
    ok, out, err = run(
        ["ssh", *SSH_OPTS, MONITORING_SSH, "--", "curl", "-sS", "--max-time", "15", url],
        timeout=SSH_TIMEOUT)
    if not ok:
        return [], f"could not reach Prometheus through {MONITORING_SSH}: {err or 'no output'}"
    try:
        doc = json.loads(out)
    except json.JSONDecodeError:
        return [], "Prometheus returned unparseable JSON (is it bound to the address we queried?)"
    if doc.get("status") != "success":
        return [], f"Prometheus answered status={doc.get('status')}"
    return doc.get("data", {}).get("result", []), None


def _urlquote(s):
    return "".join(c if c.isalnum() or c in "-_.~" else "%%%02X" % b
                   for c in s for b in [ord(c)])


def index_by_host(series):
    """Group every returned series by the host it belongs to.

    KEYED ON THE `instance` ADDRESS, not on a hostname label. The textfile publisher sets `host`,
    but Prometheus ALSO relabels a `host` coming from a scraped target into `exported_host` when the
    scrape config already assigns one -- which this fleet's does. So `host` may be the scrape's
    value and `exported_host` the host's own. The instance address is set by the scrape config from
    nodeTargets and is unambiguous, so that is what we join on.
    """
    out = {}
    for s in series:
        m = s.get("metric", {})
        instance = m.get("instance", "")
        addr = instance.split(":")[0]
        if not addr:
            continue
        out.setdefault(addr, []).append(s)
    return out


def pick(series_list, name):
    for s in series_list:
        if s.get("metric", {}).get("__name__") == name:
            return s
    return None


def value_of(s, default=None):
    if not s:
        return default
    try:
        return float(s["value"][1])
    except (KeyError, IndexError, ValueError):
        return default


def label_of(s, key, default=""):
    if not s:
        return default
    return s.get("metric", {}).get(key, default)


CLOSURE_RE = re.compile(r"^(?P<hash>[a-z0-9]{32})-nixos-system-(?P<host>.+?)-(?P<version>[\d.]+\..+)$")


def short_closure(closure):
    """The store hash is the identity; the rest is the same on every host and only adds width."""
    m = CLOSURE_RE.match(closure or "")
    return m.group("hash")[:12] if m else (closure or "")[:12]


def closure_version(closure):
    m = CLOSURE_RE.match(closure or "")
    return m.group("version") if m else ""


def read_machine(name, decl, series_list, head, origin):
    """Join one host's declaration with what it is publishing, and decide how alarming it is."""
    row = {
        "name": name,
        "role": decl.get("role", ""),
        "private": decl.get("privateAddress", ""),
        "public": decl.get("publicAddress", ""),
        "reporting": bool(series_list),
    }

    if not series_list:
        # A DECLARED HOST THAT SAYS NOTHING. Not the same as "unreachable" -- we never tried to
        # reach it. Either its node_exporter is down, its textfile collector is empty, or Prometheus
        # is not scraping it. All three are worth waking up for, and none of them are "fine".
        row.update(state="not reporting", severity="alarm", detail="publishes no nixos_* metrics",
                   closure="", booted="", nixpkgs="", revision="", staged_revision="",
                   auto_apply="", blocked_reason="", last_verdict=0,
                   # NO BUTTON ON A SILENT HOST. Not because acting would be unsafe -- the check
                   # phase would read the host directly and find out the truth -- but because
                   # nothing here knows whether it has anything staged, and an offer to "activate
                   # the staged closure" on a machine we cannot say has one is an offer to find
                   # out by trying. The row already says the useful thing: it stopped reporting.
                   actionable=False)
        return row

    current = pick(series_list, "nixos_closure_info")
    booted = pick(series_list, "nixos_booted_closure_info")
    revision = pick(series_list, "nixos_configuration_revision_info")
    staged_rev = pick(series_list, "nixos_staged_revision_info")
    osinfo = pick(series_list, "node_os_info")
    info = pick(series_list, "nixos_auto_apply_info")

    row["closure"] = label_of(current, "closure")
    row["booted"] = label_of(booted, "closure")
    row["nixpkgs"] = label_of(osinfo, "build_id") or closure_version(row["closure"])
    row["revision"] = label_of(revision, "revision")
    row["staged_revision"] = label_of(staged_rev, "revision")
    row["auto_apply"] = label_of(info, "state")
    row["blocked_reason"] = label_of(info, "reason")
    row["detail"] = label_of(info, "detail")
    row["last_verdict"] = value_of(pick(series_list, "nixos_auto_apply_last_verdict_timestamp_seconds"), 0)

    reboot_required = value_of(pick(series_list, "nixos_reboot_required"), 0) == 1
    reboot_owed = value_of(pick(series_list, "nixos_auto_apply_reboot_owed"), 0) == 1
    staged_pending = value_of(pick(series_list, "nixos_staged_pending"), 0) == 1
    blocked = value_of(pick(series_list, "nixos_auto_apply_blocked"), 0) == 1

    # A REVISION ENDING `-dirty` MEANS THE CLOSURE WAS BUILT FROM AN UNCOMMITTED TREE, which makes
    # it unreproducible: nothing in git describes what that machine is running. It is called out
    # separately from "behind" because the fix is different -- committing and redeploying, not
    # waiting for auto-apply.
    row["dirty"] = row["revision"].endswith("-dirty")
    row["revision_short"] = row["revision"].replace("-dirty", "")[:9]
    row["staged_short"] = row["staged_revision"][:9]

    row["behind"] = commits_between(row["revision"].replace("-dirty", ""), origin or head)

    row["reboot_required"] = reboot_required or reboot_owed
    row["staged_pending"] = staged_pending
    row["blocked"] = blocked

    # WHETHER THIS ROW GETS A BUTTON AT ALL. The rule the brief states -- nothing staged, or staged
    # equals running, means no offer -- lives HERE, on the read side, so the table and the endpoint
    # can never disagree about it: handle_fleet_apply resolves the machine out of this same cached
    # row rather than re-deciding.
    #
    # `nixos_staged_pending` is the host's own answer and is preferred, since it is computed on the
    # machine from the pin and /run/current-system. The revision comparison is the fallback for a
    # host that publishes a staged revision but not the gauge (an older auto-apply, a half-written
    # textfile): being one release behind on the metric should not hide a button the operator
    # otherwise has no way to reach.
    #
    # This is a HINT, never an authorisation. Everything here comes from a scrape that can be up to
    # a minute stale, so the check phase reads the pin off the host again and only then offers the
    # switch. A row that says "staged" and a host that has since activated it produces a check that
    # says "already running it", not a needless switch.
    row["actionable"] = bool(
        staged_pending
        or (row["staged_revision"] and row["staged_revision"] != row["revision"].replace("-dirty", ""))
    ) and bool(row["public"])

    if reboot_required or reboot_owed:
        row["state"] = "reboot owed"
        row["severity"] = "warn"
    elif blocked:
        row["state"] = f"blocked: {row['blocked_reason'] or 'unknown'}"
        row["severity"] = "warn"
    elif staged_pending:
        row["state"] = "staged, not activated"
        row["severity"] = "warn"
    elif row["dirty"]:
        row["state"] = "built dirty"
        row["severity"] = "warn"
    else:
        row["state"] = "current"
        row["severity"] = "ok"

    return row


def build():
    """Gather everything the page needs. Roster and metrics are fetched CONCURRENTLY because one is
    a local nix eval and the other an ssh round trip, and there is no reason to pay for both."""
    started = time.time()
    head, dirty_checkout, origin = git_head()

    with concurrent.futures.ThreadPoolExecutor(max_workers=2) as pool:
        f_roster = pool.submit(flake_machines)
        f_series = pool.submit(prom_series)
        roster, roster_err = f_roster.result()
        series, prom_err = f_series.result()

    by_addr = index_by_host(series)
    rows = []
    for name in sorted(roster):
        decl = roster[name]
        rows.append(read_machine(name, decl, by_addr.get(decl.get("privateAddress", ""), []),
                                 head, origin))

    # Anything publishing metrics that the flake does not declare. On a three-host fleet this should
    # always be empty; if it is not, something is scraping a machine nobody owns.
    declared = {d.get("privateAddress") for d in roster.values()}
    undeclared = sorted(a for a in by_addr if a not in declared)

    return {
        "built_at": started,
        "took": time.time() - started,
        "rows": rows,
        "undeclared": undeclared,
        "head": head,
        "origin": origin,
        "dirty_checkout": dirty_checkout,
        "errors": [e for e in (roster_err, prom_err) if e],
    }


def cached(force=False):
    now = time.time()
    with _cache_lock:
        fresh = _cache["data"] is not None and (now - _cache["built_at"]) < CACHE_TTL
        if fresh and not force:
            return _cache["data"]
        building = _cache["building"]
        have_stale = _cache["data"] is not None
        if not building:
            _cache["building"] = True

    # SERVE STALE WHILE REBUILDING rather than making every visitor wait on an ssh round trip. Only
    # one rebuild runs at a time; the rest read whatever the last one produced.
    if building and have_stale:
        return _cache["data"]

    try:
        data = build()
        with _cache_lock:
            _cache["data"] = data
            _cache["built_at"] = time.time()
        return data
    finally:
        with _cache_lock:
            _cache["building"] = False


# --------------------------------------------------------------------------------------------
# THE ACTION -- activating the closure CI already staged
# --------------------------------------------------------------------------------------------
#
# TWO PHASES, `check` THEN `switch`, ported from the reference for a reason that is not ceremony.
# The check does a `switch-to-configuration dry-activate` and changes nothing; its output is the
# list of units activating WOULD disturb, which on this fleet is the whole question -- the only
# reason auto-apply has not already done this is `units_would_change`, so "which units" is the
# decision the operator is being asked to make. Pressing one button and finding out afterwards
# would be handing them the outcome instead of the choice.
#
# The second thing the split buys is that THE BROWSER NEVER NAMES A STORE PATH IT INVENTED. The
# check reads the pin off the host and emits it as a `@@ CANSWITCH <path>` marker; the switch phase
# accepts only a path some check in this process's lifetime actually read that way. Without that,
# /fleet-apply would activate any store path anybody could POST to it.

CHECK_SCRIPT = r"""
set -uo pipefail
pin=%(pin)s
echo "· connected to $(hostname) as $(id -un)"
running=$(readlink -f /run/current-system 2>/dev/null || true)
echo "· running   ${running:-unknown}"
booted=$(readlink -f /run/booted-system 2>/dev/null || true)
if [ -n "$booted" ] && [ "$booted" != "$running" ]; then
  echo "· booted    $booted  (differs from current: ordinary residue of an earlier switch)"
fi
staged=$(readlink -f "$pin" 2>/dev/null || true)
if [ -z "$staged" ] || [ ! -e "$staged" ]; then
  echo "· staged    nothing is pinned at $pin — CI has never staged a closure here, or the"
  echo "·           gcroot went with it. There is nothing for this button to activate."
  exit 0
fi
echo "· staged    $staged"
if [ -x "$staged/sw/bin/nixos-version" ]; then
  echo "· staged revision $("$staged/sw/bin/nixos-version" --json 2>/dev/null || echo unknown)"
fi
if [ "$staged" = "$running" ]; then
  echo "· this host is already running the staged closure — there is nothing here to activate"
  exit 0
fi
echo ""
echo "--- switch-to-configuration dry-activate: what activating it WOULD disturb ---"
# Captured as well as streamed, so the failure branch below can READ the output rather than guess
# at it from an exit code. `tee` keeps the operator's console live either way.
dry=$(mktemp)
"$staged/bin/switch-to-configuration" dry-activate 2>&1 | tee "$dry"
rc=${PIPESTATUS[0]}
echo "--- dry run exited $rc — NOTHING on this host has been changed ---"
if [ "$rc" -ne 0 ]; then
  # THE TWO FAILURES WANT OPPOSITE RESPONSES and neither is legible from "exited 11", which is why
  # they are told apart here rather than left as one error. A held lock is somebody else's
  # activation in progress -- most likely nixos-auto-apply's own timer, which runs on these hosts
  # -- and it clears by itself. Anything else did not complete, so what activating would disturb is
  # UNKNOWN, and offering a switch on the back of a dry run that never finished would be offering
  # to activate something nothing has checked.
  if grep -qi 'could not acquire lock' "$dry"; then
    echo "!! Something else is activating on this host: switch-to-configuration could not take"
    echo "!! its lock. The lock is held only while a process runs, so this clears by itself —"
    echo "!! wait for nixos-auto-apply's run to finish and check again."
    # The lock file existing proves nothing (flock is released when the holder exits), but a
    # HOLDER is findable, and naming it beats sending someone to wait for something that is not
    # actually running.
    holder=""
    for fd in /proc/[0-9]*/fd/*; do
      case "$(readlink "$fd" 2>/dev/null)" in
        */switch-to-configuration.lock)
          pid=${fd#/proc/}; pid=${pid%%%%/*}
          holder="$holder $pid($(tr '\0' ' ' < /proc/$pid/cmdline 2>/dev/null | cut -c1-60))" ;;
      esac
    done
    if [ -n "$holder" ]; then
      echo "!! Held by:$holder"
    else
      echo "!! No holder is visible now, so it has already finished — check again."
    fi
  else
    echo "!! The dry run did not complete, so what activating this closure would disturb is"
    echo "!! UNKNOWN — which is why no switch is offered. Read the output above."
  fi
  rm -f "$dry"
  exit "$rc"
fi
rm -f "$dry"
echo "@@ CANSWITCH $staged"
""" % {"pin": shlex.quote(STAGED_PIN)}

SWITCH_SCRIPT = r"""
set -uo pipefail
staged="$1"
[ -e "$staged" ] || { echo "!! $staged is not on this host any more"; exit 5; }
running=$(readlink -f /run/current-system 2>/dev/null || true)
if [ "$staged" = "$running" ]; then
  echo "· already running it — nothing to do"
  echo "@@ DONE"
  exit 0
fi
# THE PROFILE IS SET FIRST, AND THAT ORDER IS LOAD-BEARING -- the same order, for the same reason,
# as nix/files/nixos-auto-apply.py: switch-to-configuration re-reads /nix/var/nix/profiles/system,
# so activating without setting it leaves the running system pointing at the closure it just
# replaced. Someone "tidying" these two lines into the other order gets a host that activated the
# new configuration and will roll back to the old one at next boot.
echo "· nix-env --profile /nix/var/nix/profiles/system --set $staged"
if ! nix-env --profile /nix/var/nix/profiles/system --set "$staged"; then
  echo "!! could not set the system profile; nothing was activated"
  exit 6
fi
echo "· switch-to-configuration switch"
"$staged/bin/switch-to-configuration" switch
rc=$?
echo "· switch-to-configuration exited $rc"
now=$(readlink -f /run/current-system 2>/dev/null || true)
echo "· now running $now"
# THE VERDICT IS WHAT /run/current-system POINTS AT, not the exit code. switch-to-configuration
# exits non-zero when any one unit fails to come back, which is worth reporting but is not the
# same statement as "the closure was not activated" -- and conflating them would tell an operator
# to retry a switch that has already happened.
if [ "$now" = "$staged" ]; then echo "@@ DONE"; else echo "@@ FAILED"; fi
exit "$rc"
"""

_apply_jobs = {}
_apply_lock = threading.Lock()
_apply_seq = [0]
# THE FLEET-WIDE SLOT: the id of the one job allowed to be running, or None.
#
# ONE AT A TIME ACROSS THE WHOLE FLEET, which is stricter than the reference's per-machine rule,
# and deliberately so. Two switches on ONE host would race `nix-env --set` on the same profile and
# leave it pointing at whichever finished last, which is the unrecoverable-looking version of this
# and the reason a lock has to exist at all. But this fleet is three hosts that depend on each
# other -- monitoring-1 runs the k3s server that k3s-worker-1 joins, and mongo-1 is the database
# both the worker pod and the Fly web tier talk to -- so two simultaneous switches on DIFFERENT
# hosts still means an operator reading one console while something they cannot see restarts the
# thing it depends on. A three-host fleet loses nothing by being serial.
_apply_active = [None]


def needs_confirmation(row):
    """Whether this machine's switch demands a typed confirmation rather than one click."""
    return (row.get("role") or "") in CONFIRM_ROLES


def fleet_ssh_argv(address, args=()):
    """The ssh command, built the same way for every machine and every phase.

    THE PUBLIC ADDRESS, taken from `fleet.publicAddress` in the flake -- never hardcoded here and
    never the private one. This fleet has no VPN and no jump host, so 10.20.0.x is unreachable from
    a laptop; that is the same fact MONITORING_SSH above is a consequence of. Reading the address
    from the roster means a host that gets a new public IP needs no edit in this file.

    `bash -s --` feeds the script over stdin instead of interpolating it into a remote command
    line: the script is multi-line and quoted, and building it into an argv is how a shell-quoting
    bug turns into an arbitrary remote command."""
    return ["ssh", *SSH_OPTS, "-l", FLEET_SSH_USER, address, "bash", "-s", "--", *args]


def _apply_worker(job_id, argv, script, timeout):
    """Run one ssh invocation, streaming its output into the job as it arrives.

    STDERR IS MERGED INTO STDOUT ON PURPOSE. Everything interesting here -- switch-to-configuration's
    own narration, systemd's complaints, ssh's failure to connect -- is on stderr, and a console
    that dropped it would show a switch that said nothing and looked like it had done nothing."""
    job = _apply_jobs[job_id]

    def emit(line):
        with _apply_lock:
            if len(job["lines"]) < LOG_LINE_CAP:
                job["lines"].append(line)
            elif len(job["lines"]) == LOG_LINE_CAP:
                job["lines"].append(f"!! output capped at {LOG_LINE_CAP} lines")

    # The exact command, first line of every log. When this goes wrong the first question is always
    # "what did it actually run", and an operator who can paste that line into a terminal can find
    # out for themselves without reading this file.
    emit("$ " + " ".join(shlex.quote(a) for a in argv))
    try:
        proc = subprocess.Popen(argv, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT, text=True, bufsize=1)
    except Exception as exc:  # noqa: BLE001
        emit(f"!! could not start ssh: {type(exc).__name__}: {exc}")
        with _apply_lock:
            job["done"], job["exit"] = True, -1
            _apply_active[0] = None
        return

    # A TIMER RATHER THAN subprocess.run(timeout=...), because the output is being read line by
    # line as it arrives -- that is the whole point of the console -- and `run` would only hand it
    # over at the end. The killer fires on the process, so the reader loop ends naturally with
    # whatever had been printed before the kill still in the log.
    killer = threading.Timer(timeout, proc.kill)
    killer.start()
    try:
        proc.stdin.write(script)
        proc.stdin.close()
        for raw in proc.stdout:
            line = raw.rstrip("\n")
            # `@@` markers are the script talking to THIS PROCESS, not to the reader. They carry
            # the one thing the browser is not permitted to make up -- the store path a switch may
            # activate -- and the switch's own verdict, so they are consumed here rather than
            # printed.
            if line.startswith("@@ "):
                marker = line[3:].strip()
                if marker.startswith("CANSWITCH "):
                    candidate = marker.split(" ", 1)[1].strip()
                    if CLOSURE_PATH_RE.match(candidate):
                        with _apply_lock:
                            job["can_switch"] = candidate
                    else:
                        emit(f"!! refusing to offer {candidate}: not a system closure path")
                else:
                    with _apply_lock:
                        job["result"] = marker
                continue
            emit(line)
        proc.wait()
    except Exception as exc:  # noqa: BLE001
        emit(f"!! {type(exc).__name__}: {exc}")
    finally:
        killer.cancel()

    with _apply_lock:
        job["exit"] = proc.returncode
        job["done"] = True
        result = job.get("result")
        # RELEASED HERE AND NOWHERE ELSE, inside the same `finally`-shaped path that marks the job
        # done. A slot released on a happy path only is a slot that stays taken for ever the first
        # time ssh dies oddly, and then no button on the page works again until the process is
        # restarted -- with nothing on screen explaining why.
        _apply_active[0] = None

    if result == "DONE":
        threading.Thread(target=_rebuild_when_scraped,
                         args=(job.get("private"), job.get("closure")), daemon=True).start()


def _rebuild_when_scraped(address, closure, budget=240.0, step=15.0):
    """Rebuild the page once Prometheus actually reports `closure` on `address`.

    REBUILDING THE INSTANT THE SWITCH FINISHES REBUILDS IT WRONG. This page is rendered from
    Prometheus, which scrapes on its own cadence, and the metric itself is written by the host's
    activation script -- so the moment a switch completes is precisely the moment the metrics still
    describe the closure that has just been replaced. An immediate rebuild would race the scrape,
    lose, and then sit there for a full cache period being confidently out of date about the one
    row the operator is watching.

    So wait for something exact -- this address reporting this closure -- rather than guessing at
    how long a scrape takes. It polls Prometheus directly rather than calling build(), because
    build() also runs the `nix eval` roster query and nothing a switch does can change the roster.

    Bounded, and it rebuilds either way when the budget runs out: a switch whose metric never
    arrives is itself the most interesting thing on the page -- a host that activated something and
    then stopped reporting -- and leaving the screen showing what it said beforehand hides exactly
    that."""
    want = (closure or "").rsplit("/", 1)[-1]
    deadline = time.time() + budget
    while address and want and time.time() < deadline:
        time.sleep(step)
        try:
            series, err = prom_series()
            if err:
                continue      # a blip in the poll must not skip the rebuild below
            entry = pick(index_by_host(series).get(address, []), "nixos_closure_info")
            if label_of(entry, "closure") == want:
                break
        except Exception:  # noqa: BLE001
            continue
    try:
        cached(force=True)
    except Exception:  # noqa: BLE001 -- a failed rebuild must not kill this thread silently
        pass


def machine_row(name):
    """The machine as THIS PAGE last showed it.

    Deliberately not a fresh roster eval: the button may only act on something the page has already
    displayed, so a name arriving at this endpoint from anywhere else resolves to nothing. It is
    also what makes `actionable` a single decision rather than one the table and the endpoint each
    make for themselves."""
    data = _cache["data"] or cached()
    for row in (data or {}).get("rows", []):
        if row.get("name") == name:
            return row
    return None


def handle_fleet_apply(body):
    """POST /fleet-apply -- start one check or one switch. Returns (payload, status)."""
    name = (body.get("machine") or "").strip()
    phase = body.get("phase")
    row = machine_row(name)
    if not row:
        return {"error": f"no machine called {name!r} on this page"}, 400
    address = row.get("public")
    if not address:
        return {"error": f"{name} declares no publicAddress, so nothing here can reach it"}, 400
    if not row.get("actionable"):
        # The same rule the table renders, enforced again at the endpoint. The table not drawing a
        # button is a UI fact; this is the one that holds when the POST arrives from a stale tab.
        return {"error": f"{name} has nothing staged that differs from what it is running"}, 409

    if phase == "check":
        argv, script, timeout, closure = fleet_ssh_argv(address), CHECK_SCRIPT, CHECK_TIMEOUT, ""
    elif phase == "switch":
        closure = (body.get("closure") or "").strip()
        if not CLOSURE_PATH_RE.match(closure):
            return {"error": "that is not a system closure path"}, 400
        # THE BROWSER MAY NOT NAME A CLOSURE THIS PROCESS HAS NOT SEEN STAGED. The only paths a
        # switch may activate are ones a check phase read off that host's own pin during this
        # process's lifetime. Ported from the reference unchanged, and it is the load-bearing half
        # of the pair: CLOSURE_PATH_RE only says the string is well formed.
        with _apply_lock:
            offered = any(j["machine"] == name and j.get("can_switch") == closure
                          for j in _apply_jobs.values())
        if not offered:
            return {"error": "run the check first: this closure was never read off that host"}, 400
        # THE PRODUCTION-DATABASE GATE, checked on the SERVER and not only in the browser. A
        # confirm() in JS is a courtesy to the person clicking; it stops nothing that posts
        # directly, and the whole reason this host is singled out is that we do not want the
        # difficult case to be one keystroke away from the easy one. The confirmation is the
        # machine's own name, typed, because a yes/no prompt is answered reflexively and a name is
        # not.
        if needs_confirmation(row) and (body.get("confirm") or "").strip() != name:
            return {"error": f"{name} is the production database: this switch needs the machine's "
                             f"name typed back as confirmation"}, 400
        argv, script, timeout = fleet_ssh_argv(address, [closure]), SWITCH_SCRIPT, SWITCH_TIMEOUT
    else:
        return {"error": "phase must be 'check' or 'switch'"}, 400

    with _apply_lock:
        # THE ONE-AT-A-TIME GUARD, and it is taken here -- in the same critical section that
        # registers the job -- rather than checked first and taken after. Two POSTs arriving
        # together (an impatient double-click is the ordinary case; ThreadingHTTPServer really does
        # run them concurrently) would both pass a separate check and both start.
        busy = _apply_active[0]
        if busy is not None:
            other = _apply_jobs.get(busy, {})
            return {"error": f"a {other.get('phase', 'job')} is already running against "
                             f"{other.get('machine', 'another machine')}; only one action runs at "
                             f"a time across the whole fleet"}, 409
        _apply_seq[0] += 1
        job_id = f"j{_apply_seq[0]}"
        _apply_jobs[job_id] = {
            "machine": name, "phase": phase, "lines": [], "done": False, "exit": None,
            "can_switch": None, "result": None, "started": time.time(),
            # Both addresses are carried: the public one is what was ssh'd to, the private one is
            # the key Prometheus reports under, and _rebuild_when_scraped needs the latter.
            "public": address, "private": row.get("private", ""), "closure": closure,
        }
        _apply_active[0] = job_id
        _remember_job()

    threading.Thread(target=_apply_worker, args=(job_id, argv, script, timeout),
                     daemon=True).start()
    return {"job": job_id, "phase": phase}, 200


def _remember_job():
    """Drop the oldest finished jobs once there are too many. Caller holds _apply_lock.

    Finished jobs cannot be dropped the moment they end: `can_switch` is read back out of them by
    the switch phase, and the browser is still polling the log of the one that just completed. So
    they are kept, and a day of clicking is prevented from accumulating output for ever instead."""
    if len(_apply_jobs) <= JOB_RETENTION:
        return
    for stale in sorted(_apply_jobs, key=lambda k: _apply_jobs[k]["started"])[:10]:
        if _apply_jobs[stale]["done"]:
            _apply_jobs.pop(stale, None)


def fleet_apply_log(job_id, start):
    """GET /fleet-apply/log -- everything after line `start`, plus how the job ended.

    THE BROWSER SENDS AN OFFSET RATHER THAN THIS SENDING THE WHOLE LOG EACH TIME. A switch that
    prints a few thousand lines would otherwise be re-serialised on every poll, and the console
    would visibly stutter at exactly the moment somebody is reading it."""
    with _apply_lock:
        job = _apply_jobs.get(job_id)
        if not job:
            return {"error": "no such job"}
        return {"lines": job["lines"][start:], "done": job["done"], "exit": job["exit"],
                "can_switch": job["can_switch"], "result": job["result"],
                "machine": job["machine"], "phase": job["phase"]}


def fleet_apply_status():
    """GET /fleet-apply/status -- what, if anything, is running right now.

    Polled by every open tab, so that a second browser window does not present a live-looking
    button for an action the server would refuse. Cheap on purpose: no ssh, no build, just the
    slot."""
    with _apply_lock:
        busy = _apply_active[0]
        job = _apply_jobs.get(busy) if busy else None
    if not job:
        return {"busy": False}
    return {"busy": True, "job": busy, "machine": job["machine"], "phase": job["phase"]}


STYLE = """
:root { color-scheme: light dark; }
* { box-sizing: border-box; }
body { margin: 0; padding: 24px; font: 13px/1.5 ui-monospace, SFMono-Regular, Menlo, monospace;
       background: #0f1115; color: #d7dae0; }
h1 { font-size: 15px; font-weight: 600; margin: 0 0 2px; letter-spacing: .3px; }
.sub { color: #7d8590; font-size: 12px; margin-bottom: 18px; }
table { border-collapse: collapse; width: 100%; margin-bottom: 20px; }
th { text-align: left; font-weight: 600; color: #7d8590; padding: 6px 10px; font-size: 11px;
     text-transform: uppercase; letter-spacing: .5px; border-bottom: 1px solid #242832; }
td { padding: 8px 10px; border-bottom: 1px solid #1a1d25; vertical-align: top; }
tr.ok    td:first-child { border-left: 3px solid #30a46c; }
tr.warn  td:first-child { border-left: 3px solid #f5a623; }
tr.alarm td:first-child { border-left: 3px solid #e5484d; }
.name { font-weight: 600; color: #e6e9ef; }
.mut { color: #6e7681; }
.badge { display: inline-block; padding: 1px 7px; border-radius: 10px; font-size: 11px;
         background: #1c2029; color: #9aa4b2; }
.badge.ok { background: #10291d; color: #4cc38a; }
.badge.warn { background: #2e2411; color: #f5a623; }
.badge.alarm { background: #2d1416; color: #ff6369; }
.err { background: #2d1416; border: 1px solid #5b1d22; color: #ff9ea3; padding: 10px 12px;
       border-radius: 6px; margin-bottom: 14px; }
.note { background: #1a1d25; border: 1px solid #242832; color: #9aa4b2; padding: 10px 12px;
        border-radius: 6px; margin-bottom: 14px; }
.detail { color: #6e7681; font-size: 11px; max-width: 62ch; }
code { color: #a5b4fc; }

/* The action half. The console is a <details>, so a finished job's output collapses out of the
   way without being destroyed -- it is the only record of what a switch did. */
button { font: inherit; padding: 4px 10px; border-radius: 5px; cursor: pointer;
         border: 1px solid #2f3646; background: #1c2029; color: #d7dae0; }
button:hover:not(:disabled) { border-color: #4a5568; }
button:disabled { opacity: .45; cursor: default; }
button.go { border-color: #7a3a12; background: #2e2411; color: #f5a623; }
button.danger { border-color: #5b1d22; background: #2d1416; color: #ff9ea3; }
.hint { color: #6e7681; font-size: 11px; margin-left: 8px; }
.actionrow { display: flex; align-items: center; flex-wrap: wrap; gap: 4px; }
tr.actions td { border-bottom: 1px solid #242832; padding-top: 0; }
details.cons { margin-top: 8px; }
details.cons.hidden { display: none; }
details.cons summary { cursor: pointer; color: #7d8590; font-size: 11px; }
.out { margin: 6px 0 0; padding: 8px 10px; max-height: 340px; overflow: auto;
       background: #0a0c10; border: 1px solid #1a1d25; border-radius: 5px;
       white-space: pre-wrap; word-break: break-word; font-size: 11.5px; }
.out .bad { color: #ff6369; }
.out .step { color: #9aa4b2; }
.out .rule { color: #f5a623; }
.out .cmd { color: #6e7681; }
.consfoot { color: #9aa4b2; font-size: 11px; margin-top: 6px; }
.toolbar { display: flex; align-items: center; gap: 8px; margin-bottom: 14px; }
"""


def fmt_age(ts):
    if not ts:
        return "never"
    d = int(time.time() - ts)
    if d < 90:
        return f"{d}s ago"
    if d < 5400:
        return f"{d // 60}m ago"
    return f"{d // 3600}h ago"


def render(data):
    rows = data["rows"]
    n_ok = sum(1 for r in rows if r["severity"] == "ok")
    n_warn = sum(1 for r in rows if r["severity"] == "warn")
    n_alarm = sum(1 for r in rows if r["severity"] == "alarm")

    parts = [
        "<!doctype html><html><head><meta charset='utf-8'>",
        "<title>kinowo — NixOS fleet</title>",
        # NO `<meta http-equiv=refresh>` ANY MORE, and its removal is part of the action half
        # rather than a tidy-up. A meta refresh reloads the document unconditionally, which would
        # wipe an open console mid-switch -- and that console is the only record of what the switch
        # did. The auto-reload moved into JS below, where it can decline while a job is running.
        f"<style>{STYLE}</style></head><body>",
        "<h1>kinowo — NixOS fleet</h1>",
        f"<div class='sub'>{len(rows)} declared host(s): "
        f"{n_ok} current, {n_warn} needing attention, {n_alarm} not reporting"
        f" &middot; built in {data['took']:.1f}s, {fmt_age(data['built_at'])}</div>",
    ]

    # THE WAY TO GET A CURRENT PAGE WITHOUT WAITING FOR THE TIMER. Ported from the reference: a
    # browser reload only re-serves whatever the cache holds, which is the whole complaint this
    # answers -- it forces a rebuild FIRST and reloads once there is something new to show.
    parts.append(
        "<div class=toolbar>"
        "<button id=refreshbtn onclick='refreshNow(this)'>Refresh</button>"
        "<span id=refreshnote class=hint></span>"
        "<span id=busynote class=hint></span></div>")

    age = time.time() - data["built_at"]
    if age > STALE_GRACE:
        parts.append(f"<div class='err'>This page is {int(age)}s old — the last rebuild did not "
                     f"finish. Everything below may be out of date.</div>")

    for err in data["errors"]:
        parts.append(f"<div class='err'>{html.escape(err)}</div>")

    if data["dirty_checkout"]:
        parts.append("<div class='note'>The infra checkout has uncommitted changes, so "
                     "&ldquo;behind main&rdquo; below is measured against a tree that is not what "
                     "CI would build.</div>")

    if data["undeclared"]:
        parts.append("<div class='err'>Publishing metrics but not declared in the flake: "
                     + ", ".join(html.escape(a) for a in data["undeclared"]) + "</div>")

    parts.append("<table><tr>"
                 "<th>machine</th><th>role</th><th>address</th><th>closure</th><th>nixpkgs</th>"
                 "<th>revision</th><th>auto-apply</th><th>state</th></tr>")

    for r in rows:
        sev = r["severity"]
        if r["reporting"]:
            rev = f"<code>{html.escape(r['revision_short'])}</code>"
            if r["dirty"]:
                rev += " <span class='badge warn'>dirty</span>"
            behind = r.get("behind")
            if behind:
                rev += f" <span class='mut'>{behind} behind</span>"
            elif behind == 0 and not r["dirty"]:
                rev += " <span class='mut'>on main</span>"
            if r["staged_short"] and r["staged_short"] != r["revision_short"]:
                rev += f"<br><span class='mut'>staged {html.escape(r['staged_short'])}</span>"
            closure = f"<code>{html.escape(short_closure(r['closure']))}</code>"
            if r["booted"] and r["booted"] != r["closure"]:
                closure += "<br><span class='mut'>booted " + \
                           html.escape(short_closure(r["booted"])) + "</span>"
            auto = f"{html.escape(r['auto_apply'] or '—')}<br>" \
                   f"<span class='mut'>{fmt_age(r['last_verdict'])}</span>"
        else:
            rev = closure = auto = "<span class='mut'>—</span>"

        parts.append(
            f"<tr class='{sev}'>"
            f"<td class='name'>{html.escape(r['name'])}</td>"
            f"<td class='mut'>{html.escape(r['role'])}</td>"
            f"<td class='mut'>{html.escape(r['private'])}</td>"
            f"<td>{closure}</td>"
            f"<td class='mut'>{html.escape(r['nixpkgs'])}</td>"
            f"<td>{rev}</td>"
            f"<td>{auto}</td>"
            f"<td><span class='badge {sev}'>{html.escape(r['state'])}</span>"
            + (f"<div class='detail'>{html.escape(r.get('detail') or '')}</div>"
               if r.get("detail") and sev != "ok" else "")
            + "</td></tr>")

        if r.get("actionable"):
            parts.append(action_row(r))

    parts.append("</table>")
    parts.append("<div class='sub'>The button activates the closure <b>CI already staged</b> on "
                 "that host — nothing is built or evaluated here, so it can never reach a revision "
                 "CI has not staged. Everything else is still nixos-auto-apply's job, or "
                 "<code>nixos-rebuild switch --flake infra#&lt;host&gt;</code>.</div>")
    parts.append(f"<script>{SCRIPT}</script>")
    parts.append("</body></html>")
    return "".join(parts)


def action_row(r):
    """The row under a machine that carries its button and the console the button writes into.

    A SECOND `<tr>` RATHER THAN A NINTH COLUMN, which is how the reference does it and for the same
    reason: the console needs the full width of the table, and a column sized for a log would
    squeeze the eight columns that are the actual point of the page down to nothing on every row,
    including the ones with no button at all.

    `data-*` attributes carry what the JS needs, so nothing is templated into a JS string literal
    -- a machine name reaching a script body through string interpolation is how an innocuous
    quote becomes a syntax error, or worse."""
    danger = needs_confirmation(r)
    # The two buttons are the same element with different words. A production database that offered
    # the same one-click affordance as the k3s worker would be telling the operator they are the
    # same act, and they are not.
    if danger:
        control = (
            "<button class='danger' onclick='fleetCheck(this)'>"
            "Check what activating would restart…</button>"
            "<span class='hint'>&#9888; production database. Activating can restart mongod, and "
            "the Fly web tier's change streams stop with it — a dropped change stream does not "
            "reconnect by itself, so the site would serve stale showtimes until the app is "
            "restarted. Nothing changes until you confirm, and the confirmation is this "
            "machine's name typed out.</span>")
    else:
        control = (
            "<button onclick='fleetCheck(this)'>Activate the staged closure…</button>"
            "<span class='hint'>reads the pin off the host and shows what activating it would "
            "restart — it changes nothing until you confirm</span>")

    return (
        f"<tr class='actions {r['severity']}'><td colspan='8' class='actioncell' "
        f"data-machine='{html.escape(r['name'])}' "
        f"data-danger='{'1' if danger else ''}'>"
        f"<div class='actionrow'>{control}</div>"
        # `hidden` and closed to begin with: on a page where every host has something staged this
        # would otherwise be three empty consoles pushing the table off the screen.
        "<details class='cons hidden'><summary>console</summary>"
        "<pre class='out'></pre><div class='consfoot'></div></details>"
        "</td></tr>")


# --------------------------------------------------------------------------------------------
# The browser half. Vanilla, inline, no framework -- the same stdlib-only rule the server keeps.
# --------------------------------------------------------------------------------------------
SCRIPT = r"""
// How many check/switch jobs this tab has in flight. The auto-reload and the Refresh button both
// consult it: a reload destroys an open console, and the console is the only record of what a
// switch did, so a refresh that comes due mid-job hands the decision back rather than taking it.
let jobsRunning = 0;

function writeLines(pre, lines){
  for(const line of lines){
    const row = document.createElement('div');
    row.textContent = line;
    // Classified off the line's own prefix, which is why the shell scripts are so consistent about
    // '· ', '!! ' and '--- ': the server sends no markup and the browser invents no meaning.
    if(line.startsWith('!!')) row.className = 'bad';
    else if(line.startsWith('· ')) row.className = 'step';
    else if(line.startsWith('---')) row.className = 'rule';
    else if(line.startsWith('$ ')) row.className = 'cmd';
    pre.appendChild(row);
  }
  pre.scrollTop = pre.scrollHeight;
}

function consoleOf(cell){
  const box = cell.querySelector('.cons');
  box.classList.remove('hidden');
  box.open = true;                    // opened for you the first time; still collapsible after
  return box;
}

async function fleetPost(body){
  const r = await fetch('/fleet-apply', {method:'POST',
    headers:{'Content-Type':'application/json'}, body: JSON.stringify(body)});
  return await r.json();
}

// Polls one job's log until it ends. `from` is an offset so the server re-sends only what is new;
// a switch printing thousands of lines would otherwise be re-serialised on every tick.
async function followJob(job, pre, foot){
  let from = 0, misses = 0;
  for(;;){
    let d = null;
    try{
      const r = await fetch('/fleet-apply/log?job='+encodeURIComponent(job)+'&from='+from);
      d = await r.json();
    }catch(err){ d = null; }
    // A FEW MISSES ARE NOT A FAILURE -- a moment of this dashboard being unreachable says nothing
    // about whether the switch on the host succeeded, and treating it as failure would tell an
    // operator to retry an activation that has already happened. Say it only once it persists,
    // and say exactly what is and is not known.
    if(d === null || d.error === 'no such job'){
      if(++misses < 12){
        if(misses === 3) foot.textContent = 'lost contact with the dashboard — retrying…';
        await new Promise(done => setTimeout(done, 1000));
        continue;
      }
      writeLines(pre, ['!! lost contact with the dashboard, so this job can no longer be read.',
                       '!! It may still be running on the host — check there before retrying.']);
      foot.textContent = '';
      return null;
    }
    misses = 0;
    if(d.error){ writeLines(pre, ['!! '+d.error]); foot.textContent = ''; return null; }
    if(d.lines && d.lines.length){ writeLines(pre, d.lines); from += d.lines.length; }
    if(d.done){ foot.textContent = 'finished — exit '+d.exit; return d; }
    await new Promise(done => setTimeout(done, 600));
  }
}

async function fleetCheck(btn){
  const cell = btn.closest('.actioncell');
  const box = consoleOf(cell), pre = box.querySelector('.out'), foot = box.querySelector('.consfoot');
  pre.textContent = ''; foot.textContent = 'connecting…'; btn.disabled = true;
  // Disabled BEFORE the await, so a double-click cannot post twice. The server refuses the second
  // one anyway (one job fleet-wide); this just keeps the page from showing an error for something
  // the operator did not really mean to do.
  jobsRunning++;
  try{
    const started = await fleetPost({machine: cell.dataset.machine, phase: 'check'});
    if(started.error){
      writeLines(pre, ['!! '+started.error]); foot.textContent = ''; return;
    }
    const result = await followJob(started.job, pre, foot);
    if(!result) return;
    if(!result.can_switch){
      foot.textContent = 'nothing to activate on this host — see the output above';
      return;
    }
    // THE SWITCH BUTTON ONLY EXISTS ONCE A DRY RUN HAS SAID WHAT IT WOULD DISTURB. It is created
    // here rather than rendered with the page precisely so that it cannot be pressed before that
    // output is on screen: `units_would_change` is the reason auto-apply declined, so which units
    // is the decision being delegated.
    foot.textContent = '';
    const go = document.createElement('button');
    go.className = cell.dataset.danger ? 'danger' : 'go';
    go.textContent = 'Activate this closure now';
    go.onclick = () => fleetSwitch(go, cell, result.can_switch);
    const note = document.createElement('span');
    note.className = 'hint';
    note.textContent = 'nothing above has changed this host; this is the switch itself';
    foot.appendChild(go); foot.appendChild(note);
  }finally{
    jobsRunning--;
    btn.disabled = false;
  }
}

async function fleetSwitch(btn, cell, closure){
  const name = cell.dataset.machine;
  if(cell.dataset.danger){
    // A TYPED NAME, NOT A YES/NO. A confirm() is answered reflexively; typing the machine's name
    // is a deliberate act, and it is the same string the server independently requires -- this
    // prompt is the courtesy, handle_fleet_apply is the guard.
    const typed = prompt('This is the PRODUCTION DATABASE.\n\n'
      + 'Activating ' + closure + ' on ' + name + ' can restart mongod. The Fly-hosted web tier '
      + 'holds change streams against it, and a dropped change stream does not reconnect by '
      + 'itself, so the site can go on serving stale showtimes until the app is restarted.\n\n'
      + 'Type the machine name to confirm:');
    if((typed || '').trim() !== name) return;
  }else if(!confirm('Activate the staged closure on ' + name + '?\n\n' + closure
      + '\n\nThis runs switch-to-configuration switch over ssh as root. Every unit the dry run '
      + 'listed above will be stopped, started, restarted or reloaded.')){
    return;
  }
  btn.disabled = true;
  const box = consoleOf(cell), pre = box.querySelector('.out'), foot = box.querySelector('.consfoot');
  writeLines(pre, ['', '--- activating ---']);
  foot.textContent = 'switching…';
  jobsRunning++;
  try{
    const started = await fleetPost({machine: name, phase: 'switch', closure: closure,
                                     confirm: name});
    if(started.error){ writeLines(pre, ['!! '+started.error]); foot.textContent = ''; return; }
    const result = await followJob(started.job, pre, foot);
    // THE VERDICT IS THE MARKER, NOT THE EXIT CODE. switch-to-configuration exits non-zero when
    // any single unit fails to come back, which is worth reading but is not the same statement as
    // "the closure was not activated" -- the script decides by re-reading /run/current-system.
    if(result && result.result === 'DONE'){
      foot.textContent = 'activated. The table above still shows the previous closure until '
        + 'Prometheus scrapes the host again; this page will reload itself once it has.';
      cell.querySelector('.actionrow').innerHTML =
        '<span class=hint>switched — reload the page to act on this machine again</span>';
    }else if(result){
      foot.textContent = 'the switch did not complete — read the output above before retrying';
      btn.disabled = false;
    }
  }finally{
    jobsRunning--;
  }
}

async function refreshNow(btn){
  const note = document.getElementById('refreshnote');
  btn.disabled = true; note.textContent = 'rebuilding…';
  let was = 0;
  try{
    was = ((await (await fetch('/nixos/built')).json()).built_at) || 0;
    await fetch('/nixos/refresh', {method:'POST'});
  }catch(err){
    btn.disabled = false; note.textContent = 'could not reach the dashboard'; return;
  }
  // Watch the CACHE CLOCK rather than waiting on the POST. The rebuild does an ssh round trip and
  // possibly a `nix eval`; a request somebody is staring at is the wrong place for that wait, and
  // the reference makes the same split for the same reason.
  const deadline = Date.now() + 180000;
  for(;;){
    await new Promise(done => setTimeout(done, 700));
    let d = null;
    try{ d = await (await fetch('/nixos/built')).json(); }catch(err){}
    if(d && d.built_at > was && !d.building){
      if(jobsRunning > 0){
        btn.disabled = false;
        note.textContent = 'fresh data is ready — reload once the running job has finished';
        return;
      }
      note.textContent = 'reloading…'; location.reload(); return;
    }
    if(Date.now() > deadline){
      btn.disabled = false;
      note.textContent = 'the rebuild is taking unusually long — it is still running';
      return;
    }
  }
}

// THE AUTO-RELOAD THAT REPLACED `<meta http-equiv=refresh>`. Same 30s cadence as before, but it
// declines while this tab has a job in flight, which the meta tag could not do.
setInterval(() => { if(jobsRunning === 0) location.reload(); }, 30000);

// A SECOND TAB MUST NOT OFFER A BUTTON THE SERVER WOULD REFUSE. The one-at-a-time slot is
// fleet-wide and lives on the server, so every tab polls it and greys itself out; without this the
// only way to discover somebody else is mid-switch is to press a button and read a 409.
setInterval(async () => {
  let d = null;
  try{ d = await (await fetch('/fleet-apply/status')).json(); }catch(err){ return; }
  const busy = d && d.busy;
  const note = document.getElementById('busynote');
  note.textContent = busy && jobsRunning === 0
    ? 'a ' + d.phase + ' is running against ' + d.machine + ' — one action runs at a time'
    : '';
  // Only ever disables buttons this tab did not start: `jobsRunning` covers its own, and their
  // own handlers already own their disabled state.
  if(jobsRunning > 0) return;
  for(const b of document.querySelectorAll('.actionrow button')) b.disabled = !!busy;
}, 3000);
"""


class Handler(BaseHTTPRequestHandler):
    def log_message(self, *args):  # quiet: the terminal is for errors, not an access log
        pass

    def _send(self, code, body, ctype="text/html; charset=utf-8"):
        body = body.encode() if isinstance(body, str) else body
        self.send_response(code)
        self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(len(body)))
        # NO-STORE ON EVERYTHING. The log endpoint is polled with an increasing offset and the
        # status endpoint is a lock's state; a cached answer to either is worse than no answer,
        # because it looks current.
        self.send_header("Cache-Control", "no-store")
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        path, _, query = self.path.partition("?")
        path = path.rstrip("/") or "/"
        args = urllib.parse.parse_qs(query)
        if path in ("/", "/nixos"):
            self._send(200, render(cached()))
        elif path == "/nixos/built":
            # Just the cache's clock. Deliberately tiny and it NEVER triggers a build: it is polled
            # while the Refresh spinner turns, and a poll that could start work would make the
            # button race itself.
            with _cache_lock:
                body = json.dumps({"built_at": _cache["built_at"], "building": _cache["building"]})
            self._send(200, body, "application/json")
        elif path == "/fleet-apply/log":
            try:
                start = max(0, int((args.get("from") or ["0"])[0]))
            except ValueError:
                start = 0
            self._send(200, json.dumps(fleet_apply_log((args.get("job") or [""])[0], start)),
                       "application/json")
        elif path == "/fleet-apply/status":
            self._send(200, json.dumps(fleet_apply_status()), "application/json")
        elif path == "/healthz":
            self._send(200, "ok", "text/plain; charset=utf-8")
        else:
            self._send(404, "not found")

    def do_POST(self):
        """EVERY ACTION IS A POST, AND THERE IS NO GET THAT DOES ANYTHING.

        Not a REST nicety. A GET that switches a host is one a browser prefetch, a link preview, a
        history restore or a reflexive reload can fire on its own -- and the page auto-reloads
        every 30 seconds, so a GET action would be an activation on a timer. Keeping the verbs
        split means the reload path physically cannot reach any of this."""
        path = self.path.split("?")[0].rstrip("/") or "/"
        if path == "/nixos/refresh":
            # Force a build regardless of the TTL, and answer at once with the clock the caller
            # should watch. It deliberately does NOT wait for the build: the point of the button is
            # that the waiting is visible in the page rather than in a request somebody is
            # staring at.
            with _cache_lock:
                was, busy = _cache["built_at"], _cache["building"]
            if not busy:
                threading.Thread(target=lambda: cached(force=True), daemon=True).start()
            self._send(200, json.dumps({"was": was, "already": busy}), "application/json")
            return
        if path != "/fleet-apply":
            self._send(404, "not found")
            return
        try:
            length = int(self.headers.get("Content-Length", 0))
            payload, code = handle_fleet_apply(json.loads(self.rfile.read(length) or "{}"))
        except Exception as exc:  # noqa: BLE001
            # A malformed POST is the caller's problem, not a 500: answering with the reason keeps
            # it in the console the operator is already reading rather than in this terminal.
            payload, code = {"error": f"{type(exc).__name__}: {exc}"}, 400
        self._send(code, json.dumps(payload), "application/json")


def refresh_forever():
    while True:
        time.sleep(CACHE_TTL)
        try:
            cached(force=True)
        except Exception:  # noqa: BLE001 -- a refresh failure must not kill the loop
            pass


def main():
    if "--once" in sys.argv:
        data = cached(force=True)
        for e in data["errors"]:
            print(f"error: {e}")
        for r in data["rows"]:
            print(f"{r['name']:<14} {r['severity']:<6} {r['state']:<28} "
                  f"closure={short_closure(r['closure']):<14} rev={r.get('revision_short','')} "
                  f"nixpkgs={r['nixpkgs']}")
        return 0

    cached(force=True)  # warm before announcing, so the first visitor is not the one who waits
    threading.Thread(target=refresh_forever, daemon=True).start()
    srv = ThreadingHTTPServer(("127.0.0.1", PORT), Handler)
    print(f"kinowo NixOS fleet dashboard on http://127.0.0.1:{PORT}/nixos")
    try:
        srv.serve_forever()
    except KeyboardInterrupt:
        pass
    return 0


if __name__ == "__main__":
    sys.exit(main())
