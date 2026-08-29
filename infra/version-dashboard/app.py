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

WHAT IT DELIBERATELY DOES NOT DO: deploy anything. bitcashier's dashboard carries a "Bring to
latest" button that shells out over ssh; this one is read-only by design. Activation on this fleet
belongs to nixos-auto-apply (which does it when it would disturb nothing) or to a human running
`nixos-rebuild switch`. A button that bypasses both would be a third path to production nobody
asked for.

RUNNING IT

    python3 infra/version-dashboard/app.py          # serve on http://127.0.0.1:8788/nixos
    python3 infra/version-dashboard/app.py --once   # print the page's data as text and exit

Stdlib only, on the system python3 -- no venv, no requirements.txt. It needs `nix` on PATH for the
roster and `ssh` for the Prometheus read.
"""

import concurrent.futures
import html
import json
import os
import re
import subprocess
import sys
import threading
import time
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
                   auto_apply="", blocked_reason="", last_verdict=0)
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
        "<meta http-equiv='refresh' content='30'>",
        f"<style>{STYLE}</style></head><body>",
        "<h1>kinowo — NixOS fleet</h1>",
        f"<div class='sub'>{len(rows)} declared host(s): "
        f"{n_ok} current, {n_warn} needing attention, {n_alarm} not reporting"
        f" &middot; built in {data['took']:.1f}s, {fmt_age(data['built_at'])}</div>",
    ]

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

    parts.append("</table>")
    parts.append("<div class='sub'>Read-only. Activation is nixos-auto-apply's job, or "
                 "<code>nixos-rebuild switch --flake infra#&lt;host&gt;</code>.</div>")
    parts.append("</body></html>")
    return "".join(parts)


class Handler(BaseHTTPRequestHandler):
    def log_message(self, *args):  # quiet: the terminal is for errors, not an access log
        pass

    def do_GET(self):
        path = self.path.split("?")[0].rstrip("/") or "/"
        if path in ("/", "/nixos"):
            body = render(cached()).encode()
            self.send_response(200)
            self.send_header("Content-Type", "text/html; charset=utf-8")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)
        elif path == "/healthz":
            self.send_response(200)
            self.end_headers()
            self.wfile.write(b"ok")
        else:
            self.send_response(404)
            self.end_headers()
            self.wfile.write(b"not found")


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
