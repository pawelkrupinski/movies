#!/usr/bin/env python3
"""The off-worker identity drift check's counts, pushed to Prometheus: what `scripts.IdentityLiveDrift` found in each
country of one "Record scrape fixtures" recording (its live-drift-<cc>.json), for the IdentityIncrementalDrift alert
(infra/nix/files/monitoring/rules/identity-regressions.rules) and worker-diagnostics' "Identity drift" panel.

WHY ON CI. The check ran inside each worker until its whole resolve OOM-killed worker-us two nights running (442cff245);
a recording holds the same comparison's two sides -- the corpus, and the films prod screened at that moment -- so
.github/workflows/identity-live-drift.yml runs it on a runner's heap after each recording.

    # TYPE kinowo_identity_drift_listings gauge
    # TYPE kinowo_identity_drift_unsettled gauge
    # TYPE kinowo_identity_drift_compared gauge
    # TYPE kinowo_identity_drift_recorded_timestamp_seconds gauge

- `kinowo_identity_drift_listings{country,kind}`: listings whose live TMDB film the whole resolve takes otherwise, by what
  adopting it would do (lost, gained, changed);
- `kinowo_identity_drift_unsettled{country}`: listings the whole resolve left on a model gap, not compared;
- `kinowo_identity_drift_compared{country}`: listings compared (joined to a screening film, settled);
- `kinowo_identity_drift_recorded_timestamp_seconds{country}`: when the compared moment was recorded -- a stale one
  checks nothing (IdentityLiveDriftStale).

One sample per series per check, stamped when pushed: a reader takes `last_over_time(...[1d])`. Pushed through
grafana.kinowo.net's token-gated OTLP path with the deploy marker's Grafana token, as scripts/ci/ci_timings.py pushes the
build timings (service.name kinowo-ci). A push that fails fails the step: unlike a timing chart, this is an alert's input.

Run:  python3 scripts/ci/identity_live_drift.py DIR [--push]   (GRAFANA_URL, GRAFANA_ANNOTATION_TOKEN)
Test: python3 scripts/ci/identity_live_drift_test.py
"""
import argparse
import datetime
import glob
import json
import os
import sys
import urllib.request

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from ci_timings import NoRedirect  # noqa: E402

SERVICE_NAME = "kinowo-ci"
KINDS = ("lost", "gained", "changed")


def reports(directory):
    """Every country's report under `directory`, by country."""
    found = {}
    for path in sorted(glob.glob(os.path.join(directory, "**", "live-drift-*.json"), recursive=True)):
        with open(path, encoding="utf-8") as handle:
            report = json.load(handle)
        found[report["country"]] = report
    return found


def recorded_seconds(report):
    return datetime.datetime.fromisoformat(report["recordedAt"].replace("Z", "+00:00")).timestamp()


def otlp_payload(found, now_nanos):
    """The reports as OTLP/JSON gauges, every sample at `now_nanos`."""
    if not found:
        return None
    stamp = str(now_nanos)

    def point(value, **labels):
        return {"asDouble": float(value), "timeUnixNano": stamp,
                "attributes": [{"key": key, "value": {"stringValue": str(text)}} for key, text in sorted(labels.items())]}

    def gauge(name, points):
        return {"name": name, "gauge": {"dataPoints": points}}

    countries = sorted(found)
    metrics = [
        gauge("kinowo_identity_drift_listings",
              [point(found[cc]["drifted"].get(kind, 0), country=cc, kind=kind) for cc in countries for kind in KINDS]),
        gauge("kinowo_identity_drift_unsettled", [point(found[cc]["unsettled"], country=cc) for cc in countries]),
        gauge("kinowo_identity_drift_compared", [point(found[cc]["compared"], country=cc) for cc in countries]),
        gauge("kinowo_identity_drift_recorded_timestamp_seconds",
              [point(recorded_seconds(found[cc]), country=cc) for cc in countries]),
    ]
    return {"resourceMetrics": [{
        "resource": {"attributes": [{"key": "service.name", "value": {"stringValue": SERVICE_NAME}}]},
        "scopeMetrics": [{"metrics": metrics}],
    }]}


def push(payload, url, token, opener=urllib.request.build_opener(NoRedirect).open):
    """POST the payload; None when pushed, else why not."""
    if not url or not token:
        return "GRAFANA_URL / GRAFANA_ANNOTATION_TOKEN unset"
    request = urllib.request.Request(
        url.rstrip("/") + "/ci/otlp/v1/metrics", data=json.dumps(payload).encode(), method="POST",
        headers={"Content-Type": "application/json", "Authorization": "Bearer " + token})
    try:
        with opener(request, timeout=20) as response:
            status = response.status
    except Exception as error:  # noqa: BLE001 -- reported, and the step fails on it
        return str(error)
    return None if status == 200 else "HTTP %s" % status


def summary(found):
    return "\n".join(
        "%s: %d of %d compared listings drifted (%s), %d unsettled, recorded %s" % (
            cc, sum(found[cc]["drifted"].values()), found[cc]["compared"],
            ", ".join("%s %d" % (kind, found[cc]["drifted"].get(kind, 0)) for kind in KINDS),
            found[cc]["unsettled"], found[cc]["recordedAt"])
        for cc in sorted(found))


def main(argv=None, now=None, opener=None):
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("directory")
    parser.add_argument("--push", action="store_true", help="push to Prometheus (GRAFANA_URL, GRAFANA_ANNOTATION_TOKEN)")
    args = parser.parse_args(argv)
    found = reports(args.directory)
    if not found:
        print("::error::no live-drift-<cc>.json under %s: nothing to push" % args.directory)
        return 1
    print(summary(found))
    if not args.push:
        return 0
    stamp = int((now if now is not None else datetime.datetime.now(datetime.timezone.utc).timestamp()) * 1e9)
    failed = push(otlp_payload(found, stamp), os.environ.get("GRAFANA_URL"), os.environ.get("GRAFANA_ANNOTATION_TOKEN"),
                  **({"opener": opener} if opener else {}))
    if failed:
        print("::error::drift counts not pushed: %s" % failed)
        return 1
    print("Pushed drift counts for %s." % ", ".join(sorted(found)))
    return 0


if __name__ == "__main__":
    sys.exit(main())
