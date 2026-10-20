#!/usr/bin/env python3
"""What a CI job asked the outside world: its outbound calls per external service and outcome, as a
table in the job summary and as `kinowo_ci_http_calls` in Prometheus, beside production's
`kinowo_worker_http_total`.

The counts are the worker's own counter: every wiring a recording leg (or the pre-suite fill) builds
over the real wire counts its attempts into `kinowo_worker_http_total`, and tools.CiHttpCalls writes
the process's sum as a TSV under target/ci-http-calls/ (a header of label names, then `count`). This
sums every file there. A job that asked nothing writes no file and reports nothing.

Pushed through grafana.kinowo.net's token-gated OTLP path (roles/public-proxy.nix
`tokenGatedRequests`) with the deploy marker's Grafana token, exactly as ci_timings.py pushes the
build timings: no port, password or firewall rule of its own. OTLP's `service.name` is Prometheus's
`job`, `ci-recording`. Each push is ONE sample per (country, service, outcome, phase, row) at the time
of the push, labelled with its `run_id`; a run's series end with that sample, so the live series are
one run's few dozen at a time and the store gains that many per run. Never fails the job.

    # TYPE kinowo_ci_http_calls gauge

Run:  python3 scripts/ci/ci_http_calls.py --country pl --row convergence --run-id ID [--push]
Test: python3 scripts/ci/ci_http_calls_test.py
"""
import argparse
import csv
import glob
import json
import os
import sys
import time
import urllib.request

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from ci_timings import NoRedirect  # noqa: E402

SERVICE_NAME = "ci-recording"
METRIC = "kinowo_ci_http_calls"
DIRECTORY = os.path.join("target", "ci-http-calls")


def read(directory):
    """{(service, phase, outcome): count} over every process's TSV. `service` is the counter's own label;
    before it had one, the call phase stands in for it."""
    totals = {}
    for path in sorted(glob.glob(os.path.join(directory, "*.tsv"))):
        with open(path, encoding="utf-8") as handle:
            for row in csv.DictReader(handle, delimiter="\t"):
                phase = row.get("phase", "")
                key = (row.get("service") or phase or "unknown", phase, row.get("outcome") or "unknown")
                totals[key] = totals.get(key, 0) + int(float(row["count"]))
    return totals


def summary_markdown(totals, title):
    """One row per service, a column per outcome seen, its total last."""
    if not totals:
        return ""
    outcomes = sorted({o for _, _, o in totals}, key=lambda o: (o != "success", o))
    services = {}
    for (service, _, outcome), count in totals.items():
        services.setdefault(service, {}).setdefault(outcome, 0)
        services[service][outcome] += count
    lines = ["### External calls — %s" % title, "",
             "| service | " + " | ".join(outcomes) + " | total |",
             "|---|" + "---:|" * (len(outcomes) + 1)]
    for service, by in sorted(services.items(), key=lambda kv: -sum(kv[1].values())):
        lines.append("| %s | %s | %d |" % (service, " | ".join(str(by.get(o, 0)) for o in outcomes), sum(by.values())))
    lines.append("| **all** | %s | %d |" % (
        " | ".join(str(sum(c for (_, _, o), c in totals.items() if o == outcome)) for outcome in outcomes),
        sum(totals.values())))
    return "\n".join(lines) + "\n\n"


def otlp_payload(totals, country, row, run_id, workflow, now=None):
    stamp = str(int((now if now is not None else time.time()) * 1000) * 1000000)
    points = [{"asDouble": float(count), "timeUnixNano": stamp,
               "attributes": [{"key": k, "value": {"stringValue": str(v)}} for k, v in sorted({
                   "country": country, "service": service, "phase": phase, "outcome": outcome,
                   "row": row, "run_id": run_id, "workflow": workflow}.items())]}
              for (service, phase, outcome), count in sorted(totals.items())]
    return {"resourceMetrics": [{
        "resource": {"attributes": [{"key": "service.name", "value": {"stringValue": SERVICE_NAME}}]},
        "scopeMetrics": [{"metrics": [{"name": METRIC, "gauge": {"dataPoints": points}}]}],
    }]}


def push(payload, url, token, opener=urllib.request.build_opener(NoRedirect).open):
    """POST the payload; a message on any failure, never an exception."""
    if not url or not token:
        return "Grafana secrets unset -- external-call counts not pushed."
    request = urllib.request.Request(
        url.rstrip("/") + "/ci/otlp/v1/metrics", data=json.dumps(payload).encode(), method="POST",
        headers={"Content-Type": "application/json", "Authorization": "Bearer " + token})
    try:
        with opener(request, timeout=20) as response:
            status = response.status
    except Exception as error:  # noqa: BLE001 -- see the header: never fail the job
        return "::warning::external-call counts not pushed: %s" % error
    if status != 200:
        return "::warning::external-call counts not pushed: HTTP %s" % status
    return "Pushed external-call counts."


def main(argv=None, out=sys.stdout):
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("--dir", default=DIRECTORY)
    parser.add_argument("--country", required=True)
    parser.add_argument("--row", default="")
    parser.add_argument("--run-id", default=os.environ.get("GITHUB_RUN_ID", ""))
    parser.add_argument("--workflow", default=os.environ.get("GITHUB_WORKFLOW", ""))
    parser.add_argument("--push", action="store_true", help="push to Prometheus (GRAFANA_URL, GRAFANA_ANNOTATION_TOKEN)")
    args = parser.parse_args(argv)
    totals = read(args.dir)
    if not totals:
        print("No external calls recorded (%s holds no counts)." % args.dir, file=out)
        return 0
    markdown = summary_markdown(totals, " ".join(p for p in (args.country, args.row) if p))
    print(markdown, file=out)
    summary = os.environ.get("GITHUB_STEP_SUMMARY")
    if summary:
        with open(summary, "a", encoding="utf-8") as handle:
            handle.write(markdown)
    if args.push:
        payload = otlp_payload(totals, args.country, args.row, args.run_id, args.workflow)
        print(push(payload, os.environ.get("GRAFANA_URL"), os.environ.get("GRAFANA_ANNOTATION_TOKEN")), file=out)
    return 0


if __name__ == "__main__":
    sys.exit(main())
