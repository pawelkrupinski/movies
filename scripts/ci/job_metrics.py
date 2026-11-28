#!/usr/bin/env python3
"""A job's own measurements, written as JSON lines into its metrics file and uploaded as the job's `ci-metrics-*`
artifact by .github/actions/job-telemetry. The collector (scripts/ci/ci_trends.py `export`, run on ci-runner) joins
them with the API's step timestamps and sends the events to VictoriaLogs; nothing here talks to anything but the
local machine and, for the rate limit, api.github.com.

    job_metrics.py start    # machine-id.sh, the resource sampler, GC logging for every JVM after this step, the
                            # GitHub API rate limit
    job_metrics.py finish   # the sampler's series, each JVM's GC, JUnit suites and tests, the rate limit again;
                            # prints one summary line and the artifact name (GITHUB_OUTPUT `name`)

The resource series is the WHOLE MACHINE from the job's start to its end, on every runner: on Linux cgroup-sampler.sh
(/proc/stat's CPU ticks, PSI, memory, disk, network), on macOS darwin_sampler.py (Mach's cumulative CPU ticks over all
cores, load, memory, swap, the kernel's memory-pressure level; macOS has no PSI). Both count CPU in 1/100 s ticks
(TICKS_PER_SECOND): busy ticks over total ticks is the machine's utilisation, busy ticks / 100 its CPU-seconds.
ci_timings.py pushes both per job (kinowo_ci_job_cpu_*).

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
import urllib.request
import xml.etree.ElementTree as ElementTree
from typing import Any, Callable, Dict, List, Mapping, Optional, Set, Tuple, TypedDict
import ci_machine  # noqa: E402

Line = Dict[str, Any]   # one JSON line of the metrics file
HERE = os.path.dirname(os.path.abspath(__file__))
SAMPLE_SECONDS = 5
# Linux's /proc/stat counts USER_HZ ticks and Mach's host_statistics CPU ticks, both 100 a second.
TICKS_PER_SECOND = 100
TOP = 15
JUNIT_GLOBS = ["target/test-reports/**/*.xml", "*/target/test-reports/**/*.xml",
               "page-tests-playwright/**/junit*.xml", "page-tests-playwright/reports/**/*.xml",
               "ios-localserver*.xml", "android/app/build/test-results/**/*.xml"]
GC_PAUSE = re.compile(r"^\[([0-9.]+)s\].*\bGC\(\d+\) (Pause .*?)\s*(?:(\d+)M->(\d+)M\((\d+)M\) )?([0-9.]+)ms\s*$")
UPTIME = re.compile(r"^\[([0-9.]+)s\]")


def temp_dir():
    return ci_machine.environment().get("RUNNER_TEMP") or "/tmp"


def metrics_file():
    return ci_machine.environment().get("CI_METRICS_FILE") or os.path.join(temp_dir(), "ci-metrics", "metrics.jsonl")


def sampler_out():
    return ci_machine.environment().get("CGROUP_SAMPLER_OUT") or os.path.join(temp_dir(), "cgroup-sampler.max")


def gc_dir():
    return os.path.join(temp_dir(), "ci-gc")


def append(*objects: Line) -> None:
    path = metrics_file()
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "a", encoding="utf-8") as handle:
        for obj in objects:
            handle.write(json.dumps(obj, separators=(",", ":"), sort_keys=False) + "\n")


def github_env(name: str, value: str) -> None:
    path = ci_machine.environment().get("GITHUB_ENV")
    if path:
        with open(path, "a", encoding="utf-8") as handle:
            handle.write("%s=%s\n" % (name, value))


def rate_limit(token: Optional[str], opener: Callable[..., Any] = urllib.request.urlopen) -> Optional[Dict[str, Any]]:
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


def identity() -> Dict[str, Optional[str]]:
    return {"github_job": ci_machine.environment().get("GITHUB_JOB"), "runner_name": ci_machine.environment().get("RUNNER_NAME"),
            "run_id": ci_machine.environment().get("GITHUB_RUN_ID"), "run_attempt": ci_machine.environment().get("GITHUB_RUN_ATTEMPT"),
            "label": ci_machine.environment().get("CI_METRICS_LABEL") or None}


def start(env: Mapping[str, str] = ci_machine.environment(), run: Callable[..., Any] = subprocess.run, platform: str = sys.platform,
          spawn: Callable[..., Any] = subprocess.Popen) -> None:
    os.makedirs(gc_dir(), exist_ok=True)
    github_env("CI_METRICS_FILE", metrics_file())
    run([os.path.join(HERE, "machine-id.sh")], check=False, timeout=30)
    line = dict(kind="job_start", t=round(ci_machine.wall(), 1), **identity())
    limits = rate_limit(env.get("GH_TOKEN"))
    if limits:
        line["rate_remaining"] = limits["remaining"]
    append(line)
    if platform == "darwin":
        # No /proc: this script's own sampler, detached, until the runner ends the job's processes.
        spawn([sys.executable, os.path.join(HERE, "darwin_sampler.py"), sampler_out() + ".series", str(SAMPLE_SECONDS)],
              start_new_session=True, stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    else:
        run(["bash", "-c", '"$0" start %d' % SAMPLE_SECONDS, os.path.join(HERE, "cgroup-sampler.sh")], check=False,
            timeout=30, env=dict(env, CGROUP_SAMPLER_OUT=sampler_out()))
    flag = "-Xlog:gc:file=%s/gc-%%p.log:uptime,level,tags" % gc_dir()
    existing = env.get("JDK_JAVA_OPTIONS", "")
    if flag not in existing:
        github_env("JDK_JAVA_OPTIONS", (existing + " " + flag).strip())
    print("metrics: started (machine-id, sampler every %d s, GC logs in %s)" % (SAMPLE_SECONDS, gc_dir()))


def table(path: str) -> Tuple[Optional[List[str]], List[List[Any]]]:
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


def number(cell: str) -> Any:
    try:
        value = float(cell)
        return int(value) if value.is_integer() else value
    except ValueError:
        return cell if cell else None


def jvm_commands(path: str) -> Dict[str, str]:
    commands: Dict[str, str] = {}
    if os.path.exists(path):
        with open(path, encoding="utf-8", errors="replace") as handle:
            for line in handle:
                pid, _, command = line.rstrip("\n").partition("\t")
                commands[pid] = command
    return commands


VALUE_OPTIONS = ("-cp", "-classpath", "--class-path", "--module-path", "-p")


def _launcher_label(word: str) -> Optional[str]:
    """What a command word that starts the program says of the JVM: an sbt argument file is sbt's forked test JVM, a
    plain word is the main class; a flag says nothing."""
    if word.startswith("@") or re.match(r"^sbt-args\d+\.tmp$", word.rsplit("/", 1)[-1]):
        return "sbt forked test JVM"
    return None if word.startswith("-") else word.rsplit("/", 1)[-1][:120]


def _jar_name(path: Optional[str]) -> str:
    return os.path.basename(path) if path else "-jar"


def main_class(command: str) -> Optional[str]:
    words = iter(command.split()[1:])
    for word in words:
        if word in VALUE_OPTIONS:
            next(words, None)
        elif word == "-jar":
            return _jar_name(next(words, None))
        else:
            label = _launcher_label(word)
            if label:
                return label
    return None


def jvm_flags(command: str) -> str:
    return " ".join(word for word in command.split() if word.startswith(("-X", "-XX:", "-Dsbt", "-Dfile"))
                    and not word.startswith("-Xlog"))[:600]


class GcFigures(TypedDict):
    gc_count: int
    gc_full: int
    gc_pause_ms: float
    gc_max_pause_ms: float
    heap_after_max_mb: int
    heap_committed_max_mb: int
    uptime_s: float
    gc_overhead: Optional[float]


def gc_of(path: str) -> GcFigures:
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


def jvms(commands: Mapping[str, str]) -> List[Line]:
    found: List[Line] = []
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
        # A JVM that lived under a second and never paused (`java -version`, a launcher probe) says nothing.
        if figures["gc_count"] == 0 and figures["uptime_s"] < 1:
            continue
        found.append(dict(kind="jvm", pid=pid, log=os.path.basename(path), main=main_class(command),
                          flags=jvm_flags(command), **figures))
    return found


def _failed(case: Any) -> bool:
    return case.find("failure") is not None or case.find("error") is not None


def _status(case: Any) -> str:
    if _failed(case):
        return "failed"
    return "skipped" if case.find("skipped") is not None else "passed"


def _parsed_reports(paths: List[str]) -> List[Tuple[str, Any]]:
    """(path, root element) of each JUnit report that parses."""
    parsed: List[Tuple[str, Any]] = []
    for path in paths:
        try:
            parsed.append((path, ElementTree.parse(path).getroot()))
        except (ElementTree.ParseError, OSError):
            continue
    return parsed


def _suites_in(root: Any) -> List[Any]:
    return [root] if root.tag == "testsuite" else list(root.iter("testsuite"))


def _suite_line(name: str, cases: List[Any], suite: Any) -> Line:
    """The `suite` line of one suite."""
    seconds = _suite_seconds(suite, cases)
    return {"kind": "suite", "suite": name[:200], "tests": len(cases), "failed": sum(map(_failed, cases)),
            "skipped": sum(1 for case in cases if case.find("skipped") is not None), "time_s": round(seconds, 2)}


def _suite_seconds(suite: Any, cases: List[Any]) -> float:
    return float(suite.get("time") or sum(float(case.get("time") or 0) for case in cases) or 0)


def _test_line(name: str, case: Any) -> Line:
    return {"kind": "test", "suite": name[:200], "test": (case.get("name") or "")[:200],
            "time_s": round(float(case.get("time") or 0), 3), "status": _status(case)}


def _report_paths(root: str) -> List[str]:
    return sorted({path for pattern in JUNIT_GLOBS for path in glob.glob(os.path.join(root, pattern), recursive=True)})


def _unseen_suites(reports: List[Tuple[str, Any]]) -> List[Tuple[str, List[Any], Any]]:
    """(name, cases, suite) of every suite of the reports. sbt writes a suite's report twice (the root's
    target/test-reports and the module's): each is counted once."""
    found: List[Tuple[str, List[Any], Any]] = []
    seen: Set[Tuple[Any, ...]] = set()
    for path, root in reports:
        for suite in _suites_in(root):
            cases = list(suite.findall("testcase"))
            name = suite.get("name") or os.path.basename(path)
            full, short = (name, len(cases), suite.get("time"), suite.get("timestamp"), suite.get("hostname")), (name, len(cases), suite.get("time"))
            if full not in seen and short not in seen:
                found.append((name, cases, suite))
            seen.update((full, short))
    return found


def _totals(files: int, found: List[Tuple[str, List[Any], Any]], suites: List[Line]) -> Line:
    """The `tests` line's figures: reports read, tests, failed, skipped and the suites' total seconds."""
    return {"files": files, "tests": sum(s["tests"] for s in suites), "failed": sum(s["failed"] for s in suites),
            "skipped": sum(s["skipped"] for s in suites),
            "time_s": round(sum(_suite_seconds(suite, cases) for _, cases, suite in found), 1)}


def _slowest(suites: List[Line], tests: List[Line]) -> List[Line]:
    """The slowest suites and tests, then the failed tests the slowest do not already hold."""
    slow_tests = sorted(tests, key=lambda t: -t["time_s"])[:TOP]
    failed_tests = [t for t in tests if t["status"] == "failed"][:TOP]
    return sorted(suites, key=lambda s: -s["time_s"])[:TOP] + slow_tests + [t for t in failed_tests if t not in slow_tests]


def junit(root: str = ".") -> List[Line]:
    """Every JUnit report under the job's known report paths: totals, the slowest suites and tests."""
    reports = _parsed_reports(_report_paths(root))
    if not reports:
        return []
    found = _unseen_suites(reports)
    suites = [_suite_line(name, cases, suite) for name, cases, suite in found]
    tests = [_test_line(name, case) for name, cases, _ in found for case in cases]
    return [dict(kind="tests", suites=len(suites), **_totals(len(reports), found, suites))] + _slowest(suites, tests)


def psi_line(first: Optional[float], last: Optional[float], seconds: float) -> List[str]:
    """The memory-pressure part of the summary: [] unless the series read it at both ends and spans some time."""
    if seconds > 0 and first is not None and last is not None:
        return ["PSI mem some %.1f%%" % ((last - first) / seconds / 1e4)]
    return []


def _number_at(row: List[Any], columns: List[str], name: str) -> Optional[Any]:
    """The cell of column `name` where it is a number, else None."""
    value = row[columns.index(name)] if name in columns else None
    return value if isinstance(value, (int, float)) else None


def _series_parts(series: Line) -> List[str]:
    """The summary's machine part: CPU use, memory pressure and the sample count of a series of two or more rows."""
    columns, rows = series["columns"], series["rows"]
    first, last = rows[0], rows[-1]

    def delta(name: str) -> float:
        return (_number_at(last, columns, name) or 0) - (_number_at(first, columns, name) or 0)

    ticks, seconds, busy = delta("cpu_total"), delta("t"), delta("cpu_busy")
    cpu = ["CPU %.0f%% (%.0f CPU-s)" % (100.0 * busy / ticks, busy / float(TICKS_PER_SECOND))] if ticks > 0 else []
    pressure = psi_line(_number_at(first, columns, "psi_mem_some_us"), _number_at(last, columns, "psi_mem_some_us"), seconds)
    return cpu + pressure + ["%d samples" % len(rows)]


def _jvm_parts(jvm_lines: List[Line]) -> List[str]:
    if not jvm_lines:
        return []
    return ["GC %.1f s over %d JVMs (max pause %.0f ms)" % (
        sum(l["gc_pause_ms"] for l in jvm_lines) / 1000.0, len(jvm_lines), max(l["gc_max_pause_ms"] for l in jvm_lines))]


def _tests_parts(tests: Optional[Line]) -> List[str]:
    if not tests:
        return []
    return ["%d tests (%d failed, %d skipped) in %.0f s" % (tests["tests"], tests["failed"], tests["skipped"], tests["time_s"])]


def summary(lines: List[Line]) -> str:
    series = next((l for l in lines if l["kind"] == "series"), None)
    parts = (_series_parts(series) if series and len(series["rows"]) >= 2 else []) + \
        _jvm_parts([l for l in lines if l["kind"] == "jvm"]) + \
        _tests_parts(next((l for l in lines if l["kind"] == "tests"), None))
    return "metrics: " + (", ".join(parts) if parts else "nothing sampled")


def artifact_name(now: Optional[float] = None) -> str:
    """`ci-metrics-<job>-<runner>-<finish epoch>`: one runner runs several jobs of a run, the matrix rows of one job id
    among them (the landing check's identity shards), and without the time the second upload overwrote the first."""
    raw = "ci-metrics-%s-%s-%d" % (ci_machine.environment().get("GITHUB_JOB") or "job", ci_machine.environment().get("RUNNER_NAME") or str(os.getpid()),
                                  int(now if now is not None else ci_machine.wall()))
    return re.sub(r"[^A-Za-z0-9_.-]+", "-", raw)[:200]


def finish(env: Mapping[str, str] = ci_machine.environment(), root: str = ".") -> None:
    out = sampler_out()
    lines: List[Line] = []
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
    lines.append(dict(kind="job_end", t=round(ci_machine.wall(), 1), status=env.get("CI_JOB_STATUS") or None, **identity()))
    append(*lines)
    print(summary(lines))
    output = env.get("GITHUB_OUTPUT")
    if output:
        with open(output, "a", encoding="utf-8") as handle:
            handle.write("name=%s\n" % artifact_name())
            handle.write("dir=%s\n" % os.path.dirname(metrics_file()))
    # The raw GC logs travel with the metrics when the job failed, or when the caller's env sets CI_KEEP_GC_LOGS=true
    # (a composite action cannot read `vars`).
    if env.get("CI_JOB_STATUS") == "failure" or env.get("CI_KEEP_GC_LOGS") == "true":
        target = os.path.join(os.path.dirname(metrics_file()), "gc")
        os.makedirs(target, exist_ok=True)
        for path in glob.glob(os.path.join(gc_dir(), "gc-*.log")):
            with open(path, "rb") as source, open(os.path.join(target, os.path.basename(path)), "wb") as sink:
                sink.write(source.read())


def main(argv: Optional[List[str]] = None) -> int:
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
