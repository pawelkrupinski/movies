#!/usr/bin/env python3
"""
WHAT ALERTMANAGER ACTUALLY SILENCES, asked of a running one.

The cases below are the fleet's inhibition contract stated as behaviour rather than as YAML. Each
is a (source, target, should_the_target_be_suppressed) triple: the source alert is posted, the
target alert is posted, and Alertmanager is asked which of them it has suppressed.

THE NEGATIVE CASES ARE THE POINT. Two of them reproduce bugs that shipped: an inhibit rule keyed on
a label its SOURCE was not required to carry matches every alert that also lacks it, so one dead
worker silenced fleet-wide warnings; and the fix for that, scoped only to `country`, let the web
tier silence the worker tier -- deleting the one alert that can see a worker discarding its tasks.
Both read as perfectly sensible YAML. Neither survives being asked.

Run indirectly, by test_alertmanager.sh, which starts the Alertmanager this talks to.

"NOTHING UNPROCESSED" DOES NOT MEAN "INHIBITION APPLIED". Every GET /api/v2/alerts asks the
inhibitor whether each alert is muted and records the answer, so the first read after the post
already reports every alert processed. But the inhibitor learns its SOURCE alerts from a
subscription it drains on its own goroutine: read before it has caught up and every target is
'active', because no source is cached yet. That is how one staging run (nix-stage-closures 2229,
2026-10-24) failed all six positive cases at once, 40 ms after the post, on a busy runner. The
wait below is on a SENTINEL posted after everything else: the subscription is FIFO, so once the
sentinel's own target reads suppressed, every source posted before it is cached.
"""
import json
import sys
import time
import urllib.error
import urllib.request

PORT = BASE = None  # set by main() from argv
SETTLE_SECONDS = 30

# (name, source labels, target labels, target must be suppressed)
CASES = [
    ("a critical disk silences the warning on the SAME disk",
     {"alertname": "FilesystemSpaceCritical", "severity": "critical", "host": "mongo-1", "mountpoint": "/"},
     {"alertname": "FilesystemSpaceLow", "severity": "warning", "host": "mongo-1", "mountpoint": "/"},
     True),

    ("...and NOT the warning on another disk of the same host",
     {"alertname": "FilesystemSpaceCritical", "severity": "critical", "host": "mongo-1", "mountpoint": "/"},
     {"alertname": "FilesystemInodesLow", "severity": "warning", "host": "mongo-1", "mountpoint": "/var/lib/mongodb"},
     False),

    ("...and NOT a warning on a different host",
     {"alertname": "FilesystemSpaceCritical", "severity": "critical", "host": "mongo-1", "mountpoint": "/"},
     {"alertname": "FilesystemSpaceLow", "severity": "warning", "host": "k3s-worker-1", "mountpoint": "/"},
     False),

    ("the three-day disk projection silences the seven-day one on the SAME disk",
     {"alertname": "FilesystemWillFillWithin3Days", "severity": "warning", "host": "k3s-worker-1", "mountpoint": "/"},
     {"alertname": "FilesystemWillFillWithin7Days", "severity": "warning", "host": "k3s-worker-1", "mountpoint": "/"},
     True),

    ("...and NOT the seven-day one on another disk",
     {"alertname": "FilesystemWillFillWithin3Days", "severity": "warning", "host": "mongo-1", "mountpoint": "/"},
     {"alertname": "FilesystemWillFillWithin7Days", "severity": "warning", "host": "mongo-1", "mountpoint": "/var/lib/mongodb"},
     False),

    ("a stalled queue silences the growth warning for ITS country",
     {"alertname": "WorkerQueueStalled", "severity": "critical", "country": "pl"},
     {"alertname": "WorkerQueueGrowingUnbounded", "severity": "warning", "country": "pl"},
     True),

    ("...and NOT another country's",
     {"alertname": "WorkerQueueStalled", "severity": "critical", "country": "pl"},
     {"alertname": "WorkerQueueGrowingUnbounded", "severity": "warning", "country": "de"},
     False),

    # THE FLEET-WIDE SILENCE. `WorkerDown` carries no `host`; so does every `absent()` companion.
    # An inhibit rule joining on `host` without requiring its source to have one matched them
    # against each other on the empty value and suppressed the lot.
    ("a host-less critical does NOT silence a host-less warning of another family",
     {"alertname": "WorkerDown", "severity": "critical", "country": "us"},
     {"alertname": "FilesystemMetricsAbsent", "severity": "warning"},
     False),

    ("...nor the fleet-wide worker-metrics companion",
     {"alertname": "WorkerDown", "severity": "critical", "country": "us"},
     {"alertname": "WorkerQueueMetricsAbsent", "severity": "warning"},
     False),

    # THE CROSS-TIER SILENCE, which is what the fix for the case above introduced. `country` is set
    # by the scrape target, so the web tier carries it too.
    #
    # ON `es`, NOT `pl`, AND THAT MATTERS. Every case is posted into one batch, so a NEGATIVE case
    # only means anything when no OTHER source in the batch legitimately suppresses its target.
    # Written on `pl` these two failed against a correct config, because the batch also carries
    # `WorkerQueueStalled{country=pl}` — which is supposed to suppress that country's pipeline
    # warnings. A new negative case needs a country no critical above claims.
    ("the web tier does NOT silence the worker tier of the same country",
     {"alertname": "WebServerErrorsServed", "severity": "critical", "country": "es"},
     {"alertname": "WorkerTaskTypeUnhandled", "severity": "warning", "country": "es", "task_type": "RefreshRatings"},
     False),

    ("...nor a scrape warning for that country",
     {"alertname": "WebServerErrorsServed", "severity": "critical", "country": "es"},
     {"alertname": "CinemaScrapeOldestAgeHigh", "severity": "warning", "country": "es"},
     False),

    ("a down worker DOES silence its own country's queue warning",
     {"alertname": "WorkerDown", "severity": "critical", "country": "uk"},
     {"alertname": "WorkerTasksFailingRepeatedly", "severity": "warning", "country": "uk"},
     True),

    # A dead worker stops that country's scraping and projecting too, so these are restatements
    # rather than news. Scoping the rule to `Worker.*` on the target dropped them for a revision.
    ("...and its country's scrape-staleness warning, which a dead worker causes",
     {"alertname": "WorkerDown", "severity": "critical", "country": "uk"},
     {"alertname": "CinemaScrapeOldestAgeHigh", "severity": "warning", "country": "uk"},
     True),

    ("...and its country's read-model warning",
     {"alertname": "WorkerDown", "severity": "critical", "country": "uk"},
     {"alertname": "ReadModelProjectionTriggerUnaccounted", "severity": "warning", "country": "uk"},
     True),

    # A DEAD WORKER DOES NOT EXPLAIN A WIRING BUG. `WorkerTaskTypeUnhandled` fires on tasks claimed
    # with no handler — a code defect the worker had to be RUNNING to commit. Silencing it here is
    # the same loss the web-tier case above guards against, arriving by a different door, and a
    # `Worker.*` target prefix let it in for one revision.
    ("a down worker does NOT silence its country's unhandled-task-type bug",
     {"alertname": "WorkerDown", "severity": "critical", "country": "uk"},
     {"alertname": "WorkerTaskTypeUnhandled", "severity": "warning", "country": "uk", "task_type": "RefreshRatings"},
     False),

    # `CinemaScrapeNeverScraped` has `for: 24h`; no five-minute outage can cause it, and a stall
    # held for hours would hide a venue whose parse has been broken all along.
    ("...nor a venue that has never been scraped at all",
     {"alertname": "WorkerQueueStalled", "severity": "critical", "country": "uk"},
     {"alertname": "CinemaScrapeNeverScraped", "severity": "warning", "country": "uk"},
     False),

    # FLUX HAS NO RULE, and the case is here so re-adding one has to argue with a test. A suspended
    # reconciliation is somebody's deliberate act; a controller being down does not explain it.
    ("the residential proxy failing silences its country's chain fallbacks",
     {"alertname": "ResidentialProxyFailing", "severity": "warning", "country": "uk"},
     {"alertname": "ChainFallbackSaturated", "severity": "warning", "country": "uk", "client": "CineworldClient"},
     True),

    ("...and its country's paid egress failing, the same outage read twice",
     {"alertname": "ResidentialProxyFailing", "severity": "warning", "country": "pl"},
     {"alertname": "PaidEgressFailing", "severity": "warning", "country": "pl", "provider": "decodo"},
     True),

    ("paid egress failing silences its country's queue backing up behind the retries",
     {"alertname": "PaidEgressFailing", "severity": "warning", "country": "us", "provider": "zyte"},
     {"alertname": "WorkerQueueHeadOld", "severity": "warning", "country": "us", "task_type": "ScrapeCinema"},
     True),

    ("...and NOT another country's",
     {"alertname": "PaidEgressFailing", "severity": "warning", "country": "us", "provider": "zyte"},
     {"alertname": "WorkerQueueHeadOld", "severity": "warning", "country": "de", "task_type": "ScrapeCinema"},
     False),

    ("a slower CI run silences its own jobs' regressions",
     {"alertname": "CiRunWallClockRegressed", "severity": "warning", "workflow": "Main"},
     {"alertname": "CiJobDurationRegressed", "severity": "warning", "workflow": "Main", "ci_job": "test"},
     True),

    ("...and NOT another workflow's",
     {"alertname": "CiRunWallClockRegressed", "severity": "warning", "workflow": "Main"},
     {"alertname": "CiJobDurationRegressed", "severity": "warning", "workflow": "Identity model convergence", "ci_job": "takes"},
     False),

    ("a slower CI job silences its own steps' regressions",
     {"alertname": "CiJobDurationRegressed", "severity": "warning", "workflow": "Main", "ci_job": "test"},
     {"alertname": "CiStepDurationRegressed", "severity": "warning", "workflow": "Main", "ci_job": "test", "step": "sbt"},
     True),

    ("...and NOT another job's",
     {"alertname": "CiJobDurationRegressed", "severity": "warning", "workflow": "Main", "ci_job": "test"},
     {"alertname": "CiStepDurationRegressed", "severity": "warning", "workflow": "Main", "ci_job": "lint", "step": "sbt"},
     False),

    ("a sustained probe failure silences its own 10-minute warning",
     {"alertname": "ProbeFailing", "severity": "critical", "instance": "https://kinowo.net/", "country": "pl"},
     {"alertname": "ProbeFailing", "severity": "warning", "instance": "https://kinowo.net/", "country": "pl"},
     True),

    ("...and NOT another URL's",
     {"alertname": "ProbeFailing", "severity": "critical", "instance": "https://kinowo.net/", "country": "pl"},
     {"alertname": "ProbeFailing", "severity": "warning", "instance": "https://showtimes.cc/uk", "country": "uk"},
     False),

    ("a down Flux controller does NOT silence a deliberate suspension",
     {"alertname": "FluxControllerDown", "severity": "critical", "controller": "source-controller"},
     {"alertname": "FluxObjectSuspended", "severity": "warning", "customresource_kind": "Kustomization",
      "name": "web-pl-config"},
     False),
]


# THE SENTINEL: a source and a target that only each other can match (a host no case uses), muted by
# the critical-silences-warning-on-the-same-disk rule. Posted in a SECOND request, after every case.
SENTINEL_SOURCE = {"alertname": "InhibitionSentinelCritical", "severity": "critical",
                   "host": "inhibition-sentinel", "mountpoint": "/sentinel"}
SENTINEL_TARGET = {"alertname": "InhibitionSentinelWarning", "severity": "warning",
                   "host": "inhibition-sentinel", "mountpoint": "/sentinel"}


def post(alerts):
    body = json.dumps(alerts).encode()
    request = urllib.request.Request(f"{BASE}/alerts", data=body,
                                     headers={"Content-Type": "application/json"}, method="POST")
    urllib.request.urlopen(request, timeout=10).read()


def fetch():
    with urllib.request.urlopen(f"{BASE}/alerts?silenced=false&inhibited=true&active=true", timeout=10) as response:
        return json.load(response)


def await_ready(deadline):
    """Alertmanager answers /-/ready only once its config is loaded and the API is up."""
    while time.time() < deadline:
        try:
            urllib.request.urlopen(f"http://127.0.0.1:{PORT}/-/ready", timeout=2).read()
            return True
        except (urllib.error.URLError, OSError):
            time.sleep(0.25)
    return False


def key(labels):
    return tuple(sorted(labels.items()))


def main(port):
    global PORT, BASE
    PORT, BASE = port, f"http://127.0.0.1:{port}/api/v2"
    if not await_ready(time.time() + 60):
        print("  FAILED alertmanager did not become ready")
        return 1

    # One batch: every source and every target at once. Posting them together is deliberate — it is
    # the state a real incident produces, and it makes each case's expectation hold in the presence
    # of the others rather than only in isolation.
    everything = []
    seen = set()
    for _, source, target, _ in CASES:
        for labels in (source, target):
            if key(labels) not in seen:
                seen.add(key(labels))
                everything.append({"labels": labels})
    post(everything)
    post([{"labels": SENTINEL_SOURCE}, {"labels": SENTINEL_TARGET}])

    # The inhibitor caches sources asynchronously (see the docstring): wait for the sentinel.
    states = {}
    caught_up = False
    deadline = time.time() + SETTLE_SECONDS
    while time.time() < deadline:
        states = {key(a["labels"]): a["status"]["state"] for a in fetch()}
        caught_up = states.get(key(SENTINEL_TARGET)) == "suppressed"
        if caught_up and all(state != "unprocessed" for state in states.values()) and len(states) >= len(seen):
            break
        time.sleep(0.25)
    if not caught_up:
        # Never read the cases from a cache that may be empty: every negative case would pass.
        print("  FAILED the inhibition sentinel was never suppressed, so no case below can be trusted:")
        print(f"         either the inhibitor never caught up within {SETTLE_SECONDS} s, or alertmanager.yaml no longer")
        print("         silences a warning under a critical on the same host and mountpoint")
        return 1

    bad = 0
    for name, source, target, expected in CASES:
        state = states.get(key(target))
        if state is None:
            print(f"  FAILED {name}: target alert never appeared in the API")
            bad = 1
            continue
        if state == "unprocessed":
            # Alertmanager had not finished with it inside the wait loop's deadline. Reading that as
            # "not suppressed" would pass every NEGATIVE case for the wrong reason, which is the one
            # way this suite could go quietly worthless.
            print(f"  FAILED {name}: alertmanager never finished processing the target alert")
            bad = 1
            continue
        suppressed = state == "suppressed"
        if suppressed != expected:
            want = "suppressed by" if expected else "NOT suppressed by"
            print(f"  FAILED {name}")
            print(f"         {target.get('alertname')} should be {want} {source.get('alertname')}, "
                  f"but its state is '{state}'")
            bad = 1
        else:
            print(f"  ok  {name}")
    return bad


if __name__ == "__main__":
    sys.exit(main(sys.argv[1]))
