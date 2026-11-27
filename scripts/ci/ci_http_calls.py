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
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import ci_machine  # noqa: E402
import grafana_otlp  # noqa: E402

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
    stamp = str(int((now if now is not None else ci_machine.wall()) * 1000) * 1000000)
    points = [grafana_otlp.point(stamp, count, {
                  "country": country, "service": service, "phase": phase, "outcome": outcome,
                  "row": row, "run_id": run_id, "workflow": workflow})
              for (service, phase, outcome), count in sorted(totals.items())]
    return grafana_otlp.document(SERVICE_NAME, [grafana_otlp.gauge(METRIC, points)])


def push(payload, url, token, opener=grafana_otlp.OPEN):
    """POST the payload; a message on any failure, never an exception."""
    if not url or not token:
        return "Grafana secrets unset -- external-call counts not pushed."
    failed = grafana_otlp.post(payload, url, token, opener)
    return "::warning::external-call counts not pushed: %s" % failed if failed else "Pushed external-call counts."


DOCSTRING = __doc__ or ""  # None under python -OO


def main(argv=None, out=sys.stdout):
    parser = argparse.ArgumentParser(description=DOCSTRING.split("\n")[0])
    parser.add_argument("--dir", default=DIRECTORY)
    parser.add_argument("--country", required=True)
    parser.add_argument("--row", default="")
    parser.add_argument("--run-id", default=ci_machine.environment().get("GITHUB_RUN_ID", ""))
    parser.add_argument("--workflow", default=ci_machine.environment().get("GITHUB_WORKFLOW", ""))
    parser.add_argument("--push", action="store_true", help="push to Prometheus (GRAFANA_URL, GRAFANA_ANNOTATION_TOKEN)")
    args = parser.parse_args(argv)
    totals = read(args.dir)
    if not totals:
        print("No external calls recorded (%s holds no counts)." % args.dir, file=out)
        return 0
    markdown = summary_markdown(totals, " ".join(p for p in (args.country, args.row) if p))
    print(markdown, file=out)
    summary = ci_machine.environment().get("GITHUB_STEP_SUMMARY")
    if summary:
        with open(summary, "a", encoding="utf-8") as handle:
            handle.write(markdown)
    if args.push:
        payload = otlp_payload(totals, args.country, args.row, args.run_id, args.workflow)
        print(push(payload, ci_machine.environment().get("GRAFANA_URL"), ci_machine.environment().get("GRAFANA_ANNOTATION_TOKEN")), file=out)
    return 0


if __name__ == "__main__":
    sys.exit(main())
