#!/usr/bin/env python3
"""A job's own measurements, written as JSON lines into its metrics file and uploaded as the job's `ci-metrics-*`
artifact by .github/actions/job-telemetry. The collector (scripts/ci/ci_trends.py `export`, run on ci-runner) joins
them with the API's step timestamps and sends the events to VictoriaLogs; nothing here talks to anything but the
local machine and, for the rate limit, api.github.com.

    job_metrics.py start    # machine-id.sh, the resource sampler, GC logging for every JVM after this step, the
                            # GitHub API rate limit
    job_metrics.py finish   # the sampler's series, each JVM's GC, JUnit suites and tests, the rate limit again;
                            # prints one summary line and the artifact name (GITHUB_OUTPUT `name`)

Lines (one JSON object each, `kind` first): machine (machine-id.sh), job_start, series, procs, jvm, cache (from
sbt-target-cache / setup-jdk), tests, suite, test, rate_limit, job_end. Every field is in the twenty-runners doc's
schema table.

NEVER FAILS: both commands catch everything, say so on one line, and exit 0. GC logging costs the JVMs nothing
measurable (`-Xlog:gc`, info level: one line per pause).

Test: python3 scripts/ci/job_metrics_test.py
"""
import glob
import json
import os
import re
import subprocess
import sys
import time
import urllib.request
import xml.etree.ElementTree as ElementTree

HERE = os.path.dirname(os.path.abspath(__file__))
SAMPLE_SECONDS = 5
TOP = 15
JUNIT_GLOBS = ["target/test-reports/**/*.xml", "*/target/test-reports/**/*.xml",
               "page-tests-playwright/**/junit*.xml", "page-tests-playwright/reports/**/*.xml",
               "ios-localserver*.xml", "android/app/build/test-results/**/*.xml"]
GC_PAUSE = re.compile(r"^\[([0-9.]+)s\].*\bGC\(\d+\) (Pause .*?)\s*(?:(\d+)M->(\d+)M\((\d+)M\) )?([0-9.]+)ms\s*$")
UPTIME = re.compile(r"^\[([0-9.]+)s\]")


def temp_dir():
    return os.environ.get("RUNNER_TEMP") or "/tmp"


def metrics_file():
    return os.environ.get("CI_METRICS_FILE") or os.path.join(temp_dir(), "ci-metrics", "metrics.jsonl")


def sampler_out():
    return os.environ.get("CGROUP_SAMPLER_OUT") or os.path.join(temp_dir(), "cgroup-sampler.max")


def gc_dir():
    return os.path.join(temp_dir(), "ci-gc")


def append(*objects):
    path = metrics_file()
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "a", encoding="utf-8") as handle:
        for obj in objects:
            handle.write(json.dumps(obj, separators=(",", ":"), sort_keys=False) + "\n")


def github_env(name, value):
    path = os.environ.get("GITHUB_ENV")
    if path:
        with open(path, "a", encoding="utf-8") as handle:
            handle.write("%s=%s\n" % (name, value))


def rate_limit(token, opener=urllib.request.urlopen):
    if not token:
        return None
    request = urllib.request.Request("https://api.github.com/rate_limit",
                                     headers={"Authorization": "Bearer " + token, "Accept": "application/vnd.github+json"})
    try:
        with opener(request, timeout=5) as response:
            core = json.load(response)["resources"]["core"]
        return {"remaining": core.get("remaining"), "limit": core.get("limit"), "used": core.get("used")}
    except Exception:  # noqa: BLE001 -- never fail the job
        return None


def identity():
    return {"github_job": os.environ.get("GITHUB_JOB"), "runner_name": os.environ.get("RUNNER_NAME"),
            "run_id": os.environ.get("GITHUB_RUN_ID"), "run_attempt": os.environ.get("GITHUB_RUN_ATTEMPT"),
            "label": os.environ.get("CI_METRICS_LABEL") or None}


def start(env=os.environ, run=subprocess.run):
    os.makedirs(gc_dir(), exist_ok=True)
    github_env("CI_METRICS_FILE", metrics_file())
    run([os.path.join(HERE, "machine-id.sh")], check=False, timeout=30)
    line = dict(kind="job_start", t=round(time.time(), 1), **identity())
    limits = rate_limit(env.get("GH_TOKEN"))
    if limits:
        line["rate_remaining"] = limits["remaining"]
    append(line)
    run(["bash", "-c", '"$0" start %d' % SAMPLE_SECONDS, os.path.join(HERE, "cgroup-sampler.sh")], check=False,
        timeout=30, env=dict(env, CGROUP_SAMPLER_OUT=sampler_out()))
    flag = "-Xlog:gc:file=%s/gc-%%p.log:uptime,level,tags" % gc_dir()
    existing = env.get("JDK_JAVA_OPTIONS", "")
    if flag not in existing:
        github_env("JDK_JAVA_OPTIONS", (existing + " " + flag).strip())
    print("metrics: started (machine-id, sampler every %d s, GC logs in %s)" % (SAMPLE_SECONDS, gc_dir()))


def table(path):
    """A tab-separated file with a header as (columns, rows of numbers or strings)."""
    if not os.path.exists(path):
        return None, []
    with open(path, encoding="utf-8", errors="replace") as handle:
        lines = [line.rstrip("\n") for line in handle if line.strip()]
    if not lines:
        return None, []
    columns = lines[0].split("\t")
    rows = []
    for line in lines[1:]:
        cells = line.split("\t")
        if len(cells) != len(columns):
            continue
        rows.append([number(cell) for cell in cells])
    return columns, rows


def number(cell):
    try:
        value = float(cell)
        return int(value) if value.is_integer() else value
    except ValueError:
        return cell if cell else None


def jvm_commands(path):
    commands = {}
    if os.path.exists(path):
        with open(path, encoding="utf-8", errors="replace") as handle:
            for line in handle:
                pid, _, command = line.rstrip("\n").partition("\t")
                commands[pid] = command
    return commands


def main_class(command):
    words = command.split()
    skip = False
    for index, word in enumerate(words[1:], 1):
        if skip:
            skip = False
            continue
        if word in ("-cp", "-classpath", "--class-path", "--module-path", "-p"):
            skip = True
            continue
        if word == "-jar":
            return os.path.basename(words[index + 1]) if index + 1 < len(words) else "-jar"
        if not word.startswith("-"):
            return word.rsplit("/", 1)[-1][:120]
    return None


def jvm_flags(command):
    return " ".join(word for word in command.split() if word.startswith(("-X", "-XX:", "-Dsbt", "-Dfile"))
                    and not word.startswith("-Xlog"))[:600]


def gc_of(path):
    """One GC log (`-Xlog:gc`, the uptime decorator first): pauses, their total and max, the largest after-GC heap."""
    count = full = 0
    pause_ms = max_ms = 0.0
    heap_after = committed = 0
    uptime = 0.0
    with open(path, encoding="utf-8", errors="replace") as handle:
        for line in handle:
            seen = UPTIME.match(line)
            if seen:
                uptime = max(uptime, float(seen.group(1)))
            match = GC_PAUSE.match(line.strip())
            if not match:
                continue
            count += 1
            took = float(match.group(6))
            pause_ms += took
            max_ms = max(max_ms, took)
            if "Full" in match.group(2):
                full += 1
            if match.group(4):
                heap_after = max(heap_after, int(match.group(4)))
                committed = max(committed, int(match.group(5)))
    return {"gc_count": count, "gc_full": full, "gc_pause_ms": round(pause_ms, 1), "gc_max_pause_ms": round(max_ms, 1),
            "heap_after_max_mb": heap_after, "heap_committed_max_mb": committed, "uptime_s": round(uptime, 1),
            "gc_overhead": round(pause_ms / 1000.0 / uptime, 4) if uptime > 0 else None}


def jvms(commands):
    found = []
    # The job's own GC logs (job-telemetry's JDK_JAVA_OPTIONS); a job's older /tmp/gc-*.log only when it has none,
    # since a JVM that writes both would count twice.
    paths = sorted(glob.glob(os.path.join(gc_dir(), "gc-*.log"))) or sorted(glob.glob("/tmp/gc-*.log"))
    for path in paths:
        pid = re.sub(r"\D", "", os.path.basename(path).split(".")[0]) or None
        try:
            figures = gc_of(path)
        except OSError:
            continue
        command = commands.get(pid or "", "")
        found.append(dict(kind="jvm", pid=pid, log=os.path.basename(path), main=main_class(command),
                          flags=jvm_flags(command), **figures))
    return found


def junit(root="."):
    """Every JUnit report under the job's known report paths: totals, the slowest suites and tests."""
    paths = set()
    for pattern in JUNIT_GLOBS:
        paths.update(glob.glob(os.path.join(root, pattern), recursive=True))
    suites, tests = [], []
    totals = {"files": 0, "tests": 0, "failed": 0, "skipped": 0, "time_s": 0.0}
    for path in sorted(paths):
        try:
            tree = ElementTree.parse(path)
        except (ElementTree.ParseError, OSError):
            continue
        totals["files"] += 1
        root_ = tree.getroot()
        elements = [root_] if root_.tag == "testsuite" else list(root_.iter("testsuite"))
        for suite in elements:
            cases = list(suite.findall("testcase"))
            name = suite.get("name") or os.path.basename(path)
            failed = sum(1 for case in cases if case.find("failure") is not None or case.find("error") is not None)
            skipped = sum(1 for case in cases if case.find("skipped") is not None)
            seconds = float(suite.get("time") or sum(float(case.get("time") or 0) for case in cases) or 0)
            totals["tests"] += len(cases)
            totals["failed"] += failed
            totals["skipped"] += skipped
            totals["time_s"] += seconds
            suites.append({"kind": "suite", "suite": name[:200], "tests": len(cases), "failed": failed,
                           "skipped": skipped, "time_s": round(seconds, 2)})
            for case in cases:
                tests.append({"kind": "test", "suite": name[:200], "test": (case.get("name") or "")[:200],
                              "time_s": round(float(case.get("time") or 0), 3),
                              "status": "failed" if (case.find("failure") is not None or case.find("error") is not None)
                              else ("skipped" if case.find("skipped") is not None else "passed")})
    if not totals["files"]:
        return []
    totals["time_s"] = round(totals["time_s"], 1)
    slow_suites = sorted(suites, key=lambda s: -s["time_s"])[:TOP]
    slow_tests = sorted(tests, key=lambda t: -t["time_s"])[:TOP]
    failed_tests = [t for t in tests if t["status"] == "failed"][:TOP]
    return [dict(kind="tests", suites=len(suites), **totals)] + slow_suites + slow_tests + \
        [t for t in failed_tests if t not in slow_tests]


def summary(lines):
    series = next((l for l in lines if l["kind"] == "series"), None)
    parts = []
    if series and len(series["rows"]) >= 2:
        columns = series["columns"]
        first, last = series["rows"][0], series["rows"][-1]
        get = lambda row, name: row[columns.index(name)] if name in columns and isinstance(row[columns.index(name)], (int, float)) else None
        ticks = (get(last, "cpu_total") or 0) - (get(first, "cpu_total") or 0)
        seconds = (get(last, "t") or 0) - (get(first, "t") or 0)
        if ticks > 0:
            parts.append("CPU %.0f%%" % (100.0 * ((get(last, "cpu_busy") or 0) - (get(first, "cpu_busy") or 0)) / ticks))
        if seconds > 0 and get(last, "psi_mem_some_us") is not None:
            parts.append("PSI mem some %.1f%%" % ((get(last, "psi_mem_some_us") - get(first, "psi_mem_some_us")) / seconds / 1e4))
        parts.append("%d samples" % len(series["rows"]))
    jvm_lines = [l for l in lines if l["kind"] == "jvm"]
    if jvm_lines:
        parts.append("GC %.1f s over %d JVMs (max pause %.0f ms)" % (
            sum(l["gc_pause_ms"] for l in jvm_lines) / 1000.0, len(jvm_lines), max(l["gc_max_pause_ms"] for l in jvm_lines)))
    tests = next((l for l in lines if l["kind"] == "tests"), None)
    if tests:
        parts.append("%d tests (%d failed, %d skipped) in %.0f s" % (tests["tests"], tests["failed"], tests["skipped"],
                                                                      tests["time_s"]))
    return "metrics: " + (", ".join(parts) if parts else "nothing sampled")


def artifact_name():
    raw = "ci-metrics-%s-%s" % (os.environ.get("GITHUB_JOB") or "job", os.environ.get("RUNNER_NAME") or str(os.getpid()))
    return re.sub(r"[^A-Za-z0-9_.-]+", "-", raw)[:200]


def finish(env=os.environ, root="."):
    out = sampler_out()
    lines = []
    columns, rows = table(out + ".series")
    if columns:
        lines.append({"kind": "series", "every_s": SAMPLE_SECONDS, "columns": columns, "rows": rows})
    columns, rows = table(out + ".procs")
    if columns:
        lines.append({"kind": "procs", "columns": columns, "rows": rows})
    lines += jvms(jvm_commands(out + ".jvms"))
    lines += junit(root)
    limits = rate_limit(env.get("GH_TOKEN"))
    if limits:
        lines.append(dict(kind="rate_limit", **limits))
    lines.append(dict(kind="job_end", t=round(time.time(), 1), status=env.get("CI_JOB_STATUS") or None, **identity()))
    append(*lines)
    print(summary(lines))
    output = env.get("GITHUB_OUTPUT")
    if output:
        with open(output, "a", encoding="utf-8") as handle:
            handle.write("name=%s\n" % artifact_name())
            handle.write("dir=%s\n" % os.path.dirname(metrics_file()))
    if env.get("CI_KEEP_GC_LOGS") == "true":
        target = os.path.join(os.path.dirname(metrics_file()), "gc")
        os.makedirs(target, exist_ok=True)
        for path in glob.glob(os.path.join(gc_dir(), "gc-*.log")):
            with open(path, "rb") as source, open(os.path.join(target, os.path.basename(path)), "wb") as sink:
                sink.write(source.read())


def main(argv=None):
    argv = sys.argv[1:] if argv is None else argv
    command = argv[0] if argv else ""
    try:
        if command == "start":
            start()
        elif command == "finish":
            finish()
        else:
            print("usage: job_metrics.py start|finish", file=sys.stderr)
            return 2
    except Exception as error:  # noqa: BLE001 -- see the header: never fail the job
        print("::warning::job metrics (%s) incomplete: %s" % (command, error))
    return 0


if __name__ == "__main__":
    sys.exit(main())
