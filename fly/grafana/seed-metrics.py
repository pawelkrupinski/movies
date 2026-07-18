#!/usr/bin/env python3
"""Emits a realistic slice of the worker's AND web app's Prometheus exposition
as line-protocol on stdout, ready to POST to VictoriaMetrics'
/api/v1/import/prometheus.

Why this exists: `fly/grafana/smoke-test.sh` boots Grafana against an EMPTY
metrics backend, so every `label_values(...)` template query returns nothing
and variable interpolation is never actually exercised. `local-harness.sh`
feeds this data in so `$country` resolves to real options and the dashboards
can be read for real.

The label SHAPE here is the load-bearing part, copied from what the workers
actually export:

  * `kinowo_worker_throttled` and the readmodel-projection histogram carry a
    `country` label — these are what `$country` is resolved from.
  * `jvm_*` / `process_*` carry only `app` (heap, CPU and JIT belong to the
    JVM, not to a country). That asymmetry is what makes the memory panels'
    `and on(app) <country-labelled metric>` join load-bearing for country
    scoping, so the seed must preserve it or the harness would prove nothing.
  * The WEB apps export the same country-less `jvm_*` families (one process per
    country, so the `app` label already implies the country) alongside
    `kinowo_web_movies_served`, which DOES carry `country` and is what the web
    memory panel joins against. Seeding both sides is what lets the harness prove
    the worker and web memory panels scope to DIFFERENT apps for one country
    instead of quietly overlapping.

Heap caps differ per app on purpose so the series stay distinguishable on a
panel (a shared value would overplot and hide a mis-scoped query). Web runs G1
(`G1 Old Gen` pool) where the worker runs SerialGC (`Tenured Gen`), matching
production and the pool names the two panels select on.

Usage: seed-metrics.py [--hours 2] [--step-seconds 60]
"""
import argparse
import math
import random
import time

# (app, country, heap-max-bytes). de/uk share a heap cap in production; pl's
# machine is the larger one.
WORKERS = [
    ("kinowo-worker", "pl", 486539264),
    ("kinowo-worker-uk", "uk", 454164480),
    ("kinowo-worker-de", "de", 454164480),
]

# (app, country, heap-max-bytes, cities). One web app per country — the naming
# is `kinowo` for pl and `showtimes-<cc>` elsewhere — all at -Xmx384m today, so
# the caps are nudged apart only to keep the three lines readable side by side.
WEBS = [
    ("kinowo", "pl", 402653184, ["poznan", "wroclaw", "warszawa"]),
    ("showtimes-uk", "uk", 402128896, ["london"]),
    ("showtimes-de", "de", 401604608, ["frankfurt"]),
]


def labels(pairs):
    return ",".join('%s="%s"' % (k, v) for k, v in pairs)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--hours", type=float, default=2.0)
    parser.add_argument("--step-seconds", type=int, default=60)
    args = parser.parse_args()

    random.seed(20260718)  # deterministic corpus: reruns produce the same lines
    now = int(time.time())
    start = now - int(args.hours * 3600)
    steps = list(range(start, now + 1, args.step_seconds))

    out = []
    for app, country, heap_max in WORKERS:
        instance = "%s.internal:9000" % app
        base = [("app", app), ("instance", instance), ("job", app)]
        with_country = base + [("country", country)]

        # Counters need a per-app starting point and a monotonic climb.
        cpu_seconds = random.uniform(400.0, 900.0)
        jit_seconds = random.uniform(60.0, 140.0)
        project_sum = random.uniform(200.0, 600.0)
        project_count = random.randint(400, 900)

        for i, ts in enumerate(steps):
            ms = ts * 1000

            out.append("kinowo_worker_throttled{%s} 0 %d" % (labels(with_country), ms))

            # NO country label on the JVM/process families — see module docstring.
            out.append(
                'jvm_memory_max_bytes{%s,area="heap"} %d %d'
                % (labels(base), heap_max, ms)
            )
            # Sawtooth heap: climbs then drops at GC, staying under the cap.
            phase = (i % 40) / 40.0
            heap_used = int(heap_max * (0.32 + 0.45 * phase))
            out.append(
                'jvm_memory_used_bytes{%s,area="heap"} %d %d'
                % (labels(base), heap_used, ms)
            )
            nonheap_used = int(96_000_000 + 12_000_000 * math.sin(i / 9.0))
            out.append(
                'jvm_memory_used_bytes{%s,area="nonheap"} %d %d'
                % (labels(base), nonheap_used, ms)
            )
            # SerialGC's old generation — the pool name the worker memory panel
            # selects on. Rides high (~65% of the cap) as it does in production.
            out.append(
                'jvm_memory_pool_used_bytes{%s,pool="Tenured Gen"} %d %d'
                % (labels(base), int(heap_max * 0.66), ms)
            )

            cpu_seconds += random.uniform(0.8, 2.4)
            out.append(
                "process_cpu_seconds_total{%s} %.3f %d" % (labels(base), cpu_seconds, ms)
            )
            jit_seconds += random.uniform(0.01, 0.30)
            out.append(
                "jvm_compilation_time_seconds_total{%s} %.3f %d"
                % (labels(base), jit_seconds, ms)
            )

            # The projection histogram DOES carry country.
            batch = random.randint(0, 3)
            project_count += batch
            project_sum += batch * random.uniform(0.4, 2.5)
            out.append(
                "kinowo_worker_readmodel_project_duration_seconds_sum{%s} %.4f %d"
                % (labels(with_country), project_sum, ms)
            )
            out.append(
                "kinowo_worker_readmodel_project_duration_seconds_count{%s} %d %d"
                % (labels(with_country), project_count, ms)
            )

    for app, country, heap_max, cities in WEBS:
        instance = "%s.internal:9000" % app
        base = [("app", app), ("instance", instance), ("job", "%s-web" % app)]

        cpu_seconds = random.uniform(150.0, 400.0)

        for i, ts in enumerate(steps):
            ms = ts * 1000

            # Country-labelled business gauge — the web memory panel's join key.
            for city in cities:
                for scope, span in (("all", 90), ("tomorrow", 14)):
                    served = span + random.randint(-3, 3)
                    out.append(
                        'kinowo_web_movies_served{%s,city="%s",scope="%s"} %d %d'
                        % (labels(base + [("country", country)]), city, scope, served, ms)
                    )

            # NO country label on the JVM/process families — same asymmetry as
            # the worker, so the web memory panel needs the on(app) join to scope.
            out.append(
                'jvm_memory_max_bytes{%s,area="heap"} %d %d'
                % (labels(base), heap_max, ms)
            )
            # G1 sawtooths on a shorter period than SerialGC and reclaims to a
            # lower floor — the shape the web panel is read for.
            phase = (i % 24) / 24.0
            heap_used = int(heap_max * (0.22 + 0.38 * phase))
            out.append(
                'jvm_memory_used_bytes{%s,area="heap"} %d %d'
                % (labels(base), heap_used, ms)
            )
            out.append(
                'jvm_memory_pool_used_bytes{%s,pool="G1 Old Gen"} %d %d'
                % (labels(base), int(heap_used * 0.55), ms)
            )
            nonheap_used = int(120_000_000 + 10_000_000 * math.sin(i / 7.0))
            out.append(
                'jvm_memory_used_bytes{%s,area="nonheap"} %d %d'
                % (labels(base), nonheap_used, ms)
            )
            cpu_seconds += random.uniform(0.2, 0.9)
            out.append(
                "process_cpu_seconds_total{%s} %.3f %d" % (labels(base), cpu_seconds, ms)
            )

    print("\n".join(out))


if __name__ == "__main__":
    main()
