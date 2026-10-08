#!/usr/bin/env python3
"""A worker's boot cost, GC and live set over a time window, read from the fleet's Prometheus and
VictoriaLogs:

  python3 scripts/ops/fleet_metrics.py boot <cc> <startISO> <endISO>   # per boot: CPU at 180/300/600 s, cores 10-30 min,
                                                                        # peak 30 s rate, JIT and GC s at 300 s, full GCs by 360 s,
                                                                        # and the families the identity take-up re-resolved
  python3 scripts/ops/fleet_metrics.py gc   <cc> <startISO> <endISO>   # -Xlog:gc lines: live set after full GCs, heap after
                                                                        # young ones, split at 10 min of uptime (needs the
                                                                        # workers run with -Xlog:gc:stdout; none have since 10-04)
  python3 scripts/ops/fleet_metrics.py live <cc> <startISO> <endISO>   # old gen after the last full GC, 1/min, past each
                                                                        # pod's first 10 min

Where they are, from the environment (nothing about the fleet is written here):
  FLEET_PROMETHEUS_URL     e.g. http://<monitoring host>:9090
  FLEET_VICTORIALOGS_URL   e.g. http://<monitoring host>:9428
  FLEET_SOCKS              host:port of a SOCKS5 proxy into the fleet's network (e.g. an `ssh -D` tunnel), if needed"""
import collections
import datetime
import json
import os
import re
import subprocess
import sys


# ── reading ────────────────────────────────────────────────────────────────────────────────────────

def get(url, params, timeout=120):
    args = ["curl", "-s", "--fail", "--max-time", str(timeout), "-G", url]
    if os.environ.get("FLEET_SOCKS"):
        args[1:1] = ["--socks5-hostname", os.environ["FLEET_SOCKS"]]
    for k, v in params.items():
        args += ["--data-urlencode", f"{k}={v}"]
    return subprocess.run(args, capture_output=True, text=True, check=True).stdout


def base(name):
    url = os.environ.get(name)
    if not url:
        sys.exit(f"{name} is not set (see --help)")
    return url.rstrip("/")


def iso(t):
    return datetime.datetime.fromtimestamp(t, datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def parse_time(s):
    """An ISO instant (Z or offset, up to nanoseconds) as epoch seconds."""
    return datetime.datetime.fromisoformat(re.sub(r"(\.\d{6})\d+", r"\1", s).replace("Z", "+00:00")).timestamp()


def series(expr, start, end, step=15):
    """{epoch: value} of a PromQL expression, read in 6-hour slices (Prometheus caps a range's points)."""
    a, e, out = parse_time(start), parse_time(end), {}
    while a < e:
        b = min(a + 6 * 3600, e)
        j = json.loads(get(base("FLEET_PROMETHEUS_URL") + "/api/v1/query_range",
                           {"query": expr, "start": iso(a), "end": iso(b), "step": str(step)}))
        for s in j["data"]["result"]:
            for t, v in s["values"]:
                out[float(t)] = float(v)
        a = b
    return out


def logs(query, limit):
    return [json.loads(line) for line in
            get(base("FLEET_VICTORIALOGS_URL") + "/select/logsql/query", {"query": query, "limit": str(limit)}).splitlines()
            if line.strip()]


def quantiles(xs):
    """(min, median, p90, max) of a non-empty list."""
    s = sorted(xs)
    return s[0], s[len(s) // 2], s[int(len(s) * 0.9)], s[-1]


# ── boot ───────────────────────────────────────────────────────────────────────────────────────────

def boot_rows(cpu, started, jit, gc, full_gcs, takeups, since):
    """One row per boot (a process_start_time value) at or after `since` - 600 s with 4+ CPU samples:
    (boot epoch, re-resolved families, cpu@180, cpu@300, cpu@600, centicores 10-30 min, peak 30 s cores,
    jit@300, gc@300, full GCs@360). Values a boot never reached are NaN."""
    times = sorted(cpu)
    rows = []
    for b in sorted(set(started.values())):
        if b < since - 600:
            continue
        points = [(t, cpu[t]) for t in times if started.get(t) == b]
        if len(points) < 4:
            continue

        def at(uptime, src=cpu):
            for t, _ in points:
                if t - b >= uptime and t in src:
                    return src[t]
            return float("nan")

        early = [(t, v) for t, v in points if t - b <= 330]
        peak = 0.0
        for i in range(len(early)):
            for k in range(i + 1, len(early)):
                if early[k][0] - early[i][0] >= 30:
                    peak = max(peak, (early[k][1] - early[i][1]) / (early[k][0] - early[i][0]))
                    break
        rr = next((n for t, n in sorted(takeups) if b <= t <= b + 900), None)
        rows.append((b, rr, at(180), at(300), at(600), (at(1800) - at(600)) / 12, peak, at(300, jit), at(300, gc), at(360, full_gcs)))
    return rows


def take_up_line(msg):
    m = re.search(r"(\d+) re-resolved", msg)
    return int(m[1]) if m else None


def boot(cc, start, end):
    sel = f'{{job="kinowo-worker",country="{cc}"}}'
    cpu = series(f"process_cpu_seconds_total{sel}", start, end)
    started = series(f"process_start_time_seconds{sel}", start, end)
    jit = series(f"jvm_compilation_time_seconds_total{sel}", start, end)
    gc = series(f"sum(jvm_gc_collection_seconds_sum{sel})", start, end)
    full = series(f'sum(jvm_gc_collection_seconds_count{{job="kinowo-worker",country="{cc}",gc="MarkSweepCompact"}})', start, end)
    takeups = [(parse_time(j["_time"]), n) for j in
               logs(f'_time:[{start}, {end}] country:{cc} tier:worker "identity model: taken up" | fields _time, _msg', 1000)
               if (n := take_up_line(j["_msg"])) is not None]
    print(f"{'boot':12} {'rr':>5} {'@180':>5} {'@300':>5} {'@600':>5} {'cc10-30':>7} {'peak30':>6} {'jit300':>6} {'gc300':>5} {'fullGC360':>9}")
    for b, rr, c180, c300, c600, cc1030, peak, j300, g300, f360 in boot_rows(cpu, started, jit, gc, full, takeups, parse_time(start)):
        print(f"{iso(b)[5:16]:12} {str(rr):>5} {c180:5.0f} {c300:5.0f} {c600:5.0f} {cc1030:7.1f} {peak:6.2f} {j300:6.0f} {g300:5.0f} {f360:9.0f}")


# ── gc ─────────────────────────────────────────────────────────────────────────────────────────────

GC_LINE = re.compile(r"\[([\d.]+)s\].*Pause (Young|Full) \(([^)]+)\) (\d+)M->(\d+)M\((\d+)M\) ([\d.]+)ms")


def gc_event(msg):
    """("Full"|"Young", uptime s, cause, MB before, MB after, pause ms) of a -Xlog:gc pause line, else None."""
    m = GC_LINE.search(msg)
    if not m:
        return None
    up, kind, cause, before, after, _cap, ms = m.groups()
    return kind, float(up), cause, int(before), int(after), float(ms)


def gc_summary(events):
    """Lines summarising (kind, uptime, cause, before, after, ms) events, boot (< 10 min) apart from steady."""
    out = []
    for label, keep in [("boot (<10 min)", lambda e: e[1] < 600), ("steady (>=10 min)", lambda e: e[1] >= 600)]:
        fulls = [e for e in events if e[0] == "Full" and keep(e)]
        if fulls:
            lo, med, p90, hi = quantiles([e[4] for e in fulls])
            out.append(f"{label}: {len(fulls)} full GCs, live after: min {lo} median {med} p90 {p90} max {hi} MB; "
                       f"pause total {sum(e[5] for e in fulls) / 1000:.0f}s; causes {dict(collections.Counter(e[2] for e in fulls))}")
        youngs = [e for e in events if e[0] == "Young" and keep(e)]
        if youngs:
            _, med, p90, hi = quantiles([e[4] for e in youngs])
            out.append(f"{label}: {len(youngs)} young GCs, heap after: median {med} p90 {p90} max {hi} MB; "
                       f"pause total {sum(e[5] for e in youngs) / 1000:.0f}s")
    return out


def gc(cc, start, end):
    rows = logs(f'_time:[{start}, {end}] country:{cc} tier:worker "[gc]" | sort by (_time) | fields _time,_msg,pod', 200000)
    events = [e for j in rows if (e := gc_event(j["_msg"]))]
    print(f"pods {len({j.get('pod') for j in rows})}, full GCs {sum(e[0] == 'Full' for e in events)}, "
          f"young GCs {sum(e[0] == 'Young' for e in events)}")
    print("\n".join(gc_summary(events)))


# ── live ───────────────────────────────────────────────────────────────────────────────────────────

def live(cc, start, end):
    expr = (f'max(jvm_memory_pool_collection_used_bytes{{job="kinowo-worker",country="{cc}",pool="Tenured Gen"}}) / 2^20 '
            f'and on() (time() - max(process_start_time_seconds{{job="kinowo-worker",country="{cc}"}}) > 600)')
    values = list(series(expr, start, end, step=60).values())
    if not values:
        sys.exit("no samples")
    lo, med, p90, hi = quantiles(values)
    print(f"{len(values)} samples: min {lo:.0f} median {med:.0f} p90 {p90:.0f} max {hi:.0f} MiB")


def main(argv):
    commands = {"boot": boot, "gc": gc, "live": live}
    if len(argv) != 4 or argv[0] not in commands:
        print(__doc__)
        sys.exit(0 if argv[:1] in (["-h"], ["--help"]) else 2)
    commands[argv[0]](*argv[1:])


if __name__ == "__main__":
    main(sys.argv[1:])
