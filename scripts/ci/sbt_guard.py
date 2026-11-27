#!/usr/bin/env python3
"""The one way CI runs sbt: every sbt setup on CI puts this wrapper first on the PATH (scripts/ci/sbt-guard/sbt)

THE ONE WAY CI RUNS SBT: every sbt setup on CI (sbt/setup-sbt, through setup-sbt-build, convergence-setup,
run-page-test or a job's own step) puts scripts/ci/sbt-guard first on PATH, so each `sbt ...` of a workflow or a CI
script comes here, and it runs the real sbt with the same arguments, output and exit status. (OneSbtPathSpec)

WHY. A compile can read a STALE TASTy file: `-Wsafe-init` follows a constructor into another file's compiled body,
and in zinc's first cycle that body can predate a signature change (project/StaleTastyRetry.scala). The build's
plugin recovers a batch run by dropping the configuration's classes and running the batch again, but only while it is
armed, and a batch could disarm it: Identity live drift #2001 (run 3141) began `set worker / Test / run / fork :=
false`, whose reload armed the plugin a second time, and failed every country. So the recovery is here too, outside
the build: a run that failed on the stale read and was NOT retried by the plugin drops the stale classes
(`staleTastyDrop`) and runs once more. A run the plugin already retried is left failed: a second retry would only
repeat it.

AND A DEPENDENCY A REMOTE REPOSITORY FAILED TO SERVE. lm-coursier 2.1.13 retries only a timeout or a 5xx
(scripts/ci/sbt-deps-resolve.sh), so one wrong answer from Maven Central failed the job: a 429 (PR #275, run
44250628374) or a 404 for a pom it served every other row of the same run that minute (Main #2678, e2e (rest):
`not found: https://repo1.maven.org/.../sentry-logback-8.65.0.pom`). A run that failed with lm-coursier's
`Error downloading` naming a remote URL as not found, forbidden or a download error runs again after each backoff
(SBT_GUARD_DOWNLOAD_BACKOFF, seconds, default "30 60 120"; empty, never), every attempt keeping what the ones before
it downloaded. Before each, coursier's negative records go (coursier-forget-errors.sh): a 404 for a jar whose pom
is cached leaves a `.<jar>.error` that coursier answers from without asking again, which would make the retry
repeat the failure from disk. A failure naming only a local path, or any other failure, is not retried.
A 429 (Maven Central rate-limiting the runner's address: the landing check's oldrefs, run 44807195909, 2026-11-20) waits
at least the Retry-After the server gives the URL it refused, asked with one HEAD request, bounded by
SBT_GUARD_RETRY_AFTER_MAX (seconds, default 300): a rate limit outlasts a 30 s backoff. What a check should not
download at all is cached instead (.github/actions/old-ref-deps: the old refs' sbt launcher and dependencies).

Stdlib only, and nothing from scripts/lib: every CI job puts the guard on its PATH, sparse checkouts included."""
from __future__ import annotations

import email.utils
import os
import re
import shlex
import subprocess
import sys
import threading
from typing import BinaryIO, List, Mapping, Optional
import ci_machine  # noqa: E402

HERE = os.path.dirname(os.path.realpath(__file__))
GUARD_DIR = os.path.join(HERE, "sbt-guard")
FORGET_ERRORS = os.path.join(HERE, "coursier-forget-errors.sh")
STALE_READ = "due to error in reading TASTy"
PLUGIN_RETRIED = "[stale TASTy] running the batch again"
REMOTE_FAILURE = re.compile(r"(not found|forbidden|download error)[^:]*: .*https?://")
RATE_LIMITED = re.compile(r"HTTP response code: 429 for URL: (https?://[^ )]+)")


def say(line: str) -> None:
    print(line, file=sys.stderr, flush=True)


def real_sbt(path: str) -> Optional[str]:
    """The first `sbt` on PATH that is not this guard's."""
    for directory in path.split(os.pathsep):
        candidate = os.path.join(directory, "sbt")
        if directory and os.path.isfile(candidate) and os.access(candidate, os.X_OK) \
                and os.path.realpath(directory) != GUARD_DIR:
            return candidate
    return None


def shell_status(code: int) -> int:
    return code if code >= 0 else 128 - code


def tee(source: BinaryIO, sink: BinaryIO, log: List[bytes]) -> None:
    while True:
        chunk = os.read(source.fileno(), 65536)
        if not chunk:
            return
        sink.write(chunk)
        sink.flush()
        log.append(chunk)


def run(real: str, args: List[str]) -> "tuple[int, str]":
    """The real sbt with its stdout and stderr kept apart (a caller may read either), each copied into the log: its
    status and the log."""
    proc = subprocess.Popen([real, *args], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    log: List[bytes] = []
    assert proc.stdout is not None and proc.stderr is not None
    readers = [threading.Thread(target=tee, args=(proc.stdout, sys.stdout.buffer, log)),
               threading.Thread(target=tee, args=(proc.stderr, sys.stderr.buffer, log))]
    for reader in readers:
        reader.start()
    status = shell_status(proc.wait())
    for reader in readers:
        reader.join()
    return status, b"".join(log).decode("utf-8", "replace")


def download_failed(log: str) -> bool:
    """Whether the run's log shows lm-coursier failing to download from a remote repository."""
    return "Error downloading " in log and any(REMOTE_FAILURE.search(line) for line in log.splitlines())


def rate_limited_url(log: str) -> Optional[str]:
    found = RATE_LIMITED.search(log)
    return found.group(1) if found else None


def retry_after_seconds(url: str) -> Optional[int]:
    """The seconds a server's Retry-After asks for that URL (delta-seconds or an HTTP-date), or None."""
    try:
        head = subprocess.run(["curl", "-sS", "--head", "--max-time", "10", url], stdin=subprocess.DEVNULL,
                              stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, text=True).stdout
    except OSError:
        return None
    for line in head.replace("\r", "").splitlines():
        name, _, value = line.partition(":")
        if name.lower() == "retry-after" and value.strip():
            return seconds_of(value.strip())
    return None


def seconds_of(value: str) -> Optional[int]:
    if value.isdigit():
        return int(value)
    try:
        when = email.utils.parsedate_to_datetime(value).timestamp()
    except (TypeError, ValueError):
        return None
    return max(0, int(when - ci_machine.wall()))


def wait_before_retry(log: str, step: int, env: Mapping[str, str]) -> None:
    """Say why and wait: the backoff step, or a 429's Retry-After when longer (bounded)."""
    url = rate_limited_url(log)
    if url:
        asked = retry_after_seconds(url)
        limit = env.get("SBT_GUARD_RETRY_AFTER_MAX") or "300"
        if asked is not None:
            step = max(step, min(asked, int(limit)))
        host = url.split("://", 1)[1].split("/", 1)[0]
        say(f"sbt-guard: {host} rate-limited the download (HTTP 429, Retry-After: "
            f"{'none' if asked is None else asked}, at most {limit}s honoured): running it again in {step}s")
    else:
        say(f"sbt-guard: the run failed downloading a dependency from a remote repository: running it again in {step}s")
    subprocess.run([FORGET_ERRORS], stdout=sys.stderr, check=False)
    subprocess.run([*shlex.split(env.get("SBT_GUARD_SLEEP") or "sleep"), str(step)], check=False)


def retried_after_download_failures(real: str, args: List[str], env: Mapping[str, str]) -> "tuple[int, str]":
    status, log = run(real, args)
    for step in env.get("SBT_GUARD_DOWNLOAD_BACKOFF", "30 60 120").split():
        if status == 0 or not download_failed(log):
            break
        wait_before_retry(log, int(step), env)
        status, log = run(real, args)
    return status, log


def main(args: List[str], env: Mapping[str, str]) -> int:
    real = real_sbt(env.get("PATH", ""))
    if real is None:
        say(f"sbt-guard: no sbt on PATH besides {GUARD_DIR}")
        return 127
    status, log = retried_after_download_failures(real, args, env)
    if status == 0 or STALE_READ not in log or PLUGIN_RETRIED in log:
        return status
    # The drop goes the way the run went: a `--client` run's to its server (whose build never arms the plugin), any
    # other as a batch of its own.
    drop = ["--client" if "--client" in args else "-batch", "staleTastyDrop"]
    say("sbt-guard: the run failed on a stale TASTy read and was not retried: dropping the stale classes and running it again")
    if subprocess.run([real, *drop]).returncode != 0:
        return status
    return run(real, args)[0]


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], ci_machine.environment()))
