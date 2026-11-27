"""A GITHUB DOWNLOAD, BOUNDED AND RETRIED, absent told from failed (scripts/lib; tool-conventions.sh's gh_download,
gh_rate_limit_wait, gh_release_public and prod_latest_run are its shell shims).

  python3 -I -B scripts/lib/gh_download.py download <seconds> <run|release> download <args...>
        -> 0 downloaded; 3 ABSENT (gh says there is nothing to download: no asset matches, no such or an expired
        artifact, a 404 or 410); 1 a FAILED read -- every attempt failed, each ended once it outlasts <seconds> (a hung
        gh is a failed attempt, not a wait forever).
  python3 -I -B scripts/lib/gh_download.py public <seconds> release download <args...>
        -> 0 fetched from the public asset URL; 3 that URL said 404; 4 not a download it serves (off, not github.com, a
        glob or several patterns, an option it does not know, a file it would have to overwrite without --clobber);
        1 any other failure, said on stderr. KINOWO_CURL overrides curl (a test's stub, a counting wrapper).
  python3 -I -B scripts/lib/gh_download.py rate-limit-wait
        -> the seconds until the spent API rate limit (REST core or GraphQL, whichever is at 0) resets, plus
        GH_RATE_LIMIT_SLACK (5) and up to GH_RATE_LIMIT_JITTER (10) seconds, so a matrix's legs do not ask in step;
        status 1 when none is spent, it cannot be read, or the reset is over GH_RATE_LIMIT_MAX_WAIT (600) seconds away.
        `gh api rate_limit` is not counted against the limit it reads.
  python3 -I -B scripts/lib/gh_download.py prod-latest <cc>
        -> the run id of the freshest COMPLETE nightly recording of that country (`--recording prod-latest`): its corpus
        scraped from prod that night, its enrichment tree filled live over it, as prod-latest-<cc>.txt on the
        convergence-fixtures release names it (.github/scripts/convergence-publish.sh latest). Not the hermetic pair
        the identity measure lands on (573). 1, said, when there is none yet or the release cannot be read.

RETRIES. A failed read is asked again, GH_DOWNLOAD_ATTEMPTS times in all (3), the wait doubling from GH_DOWNLOAD_BACKOFF
seconds (5). A THROTTLED one -- a 5xx, a 429 or a rate limit, Azure's "Egress is over the account limit" from the
artifact storage -- is asked up to GH_DOWNLOAD_TRANSIENT_ATTEMPTS times (10): 5, 10, 20, 40, 80, then 120 s
(GH_DOWNLOAD_MAX_WAIT) a wait, each plus up to half again of jitter, ~11-16 minutes in all. The storage account behind
Actions artifacts is shared by many repositories and throttles in bursts (run 38763455103: one leg's download refused
while its 16 sibling jobs' succeeded, the same artifact downloading in 3 s two hours later); jitter keeps a matrix's
legs from asking again in step, and past ~15 minutes it is an outage a re-run answers, not a wait worth a runner. An
absent one is an answer, never asked again -- and never confused with a failed one, which a caller treating "absent"
as "carry on with less" must not read as it.
A RELEASE THAT MUST EXIST: GH_DOWNLOAD_RELEASE_EXISTS=1 makes gh's "release not found" a failed read, asked again, not
an absent asset -- for a permanent release (the identity-recordings archive), whose tag GitHub's API now and then
answers 404 for a moment: Identity takes run 43317254260's es takes-base read the archive's 573 corpus as absent at
22:34:06 while its sibling legs downloaded the same asset from the same release around it.
A `run download`'s --dir is emptied before each retry (gh refuses to overwrite what a cut-short attempt left), so it
must be a directory of the download's own. gh's own messages go to stderr.

PUBLIC ASSETS, NO API. A `release download <tag> --pattern <one exact name>` is fetched from the release's public URL,
https://github.com/<repo>/releases/download/<tag>/<name>, which the CDN serves without the REST API -- where gh spends
three API requests on each (the release by tag, a GraphQL lookup, the asset). A convergence run downloads ~500 fills
that way; two runs side by side drew the repository's GITHUB_TOKEN budget dry (run 41228549464: every uk-2 retry 403
"API rate limit exceeded for installation" for 14 minutes). Taken when GH_DOWNLOAD_PUBLIC=1, and by default on an
Actions runner (GITHUB_ACTIONS). The repository is -R's, else GH_REPO's, else GITHUB_REPOSITORY's. A 404 there (a
private repository, a missing asset) and any other failure fall back to gh from then on: the API's answer decides
absent from failed, as it always did.
PROGRESS, NOT A CLOCK: the public fetch ends once it moves under GH_DOWNLOAD_STALL_BYTES (1024) bytes a second for
GH_DOWNLOAD_STALL_SECONDS (60) -- a stalled connection is a failed attempt within a minute -- and gh takes over.
RATE LIMIT: a gh refused with "API rate limit exceeded" waits for that limit's reset (rate-limit-wait) when the reset is
at most GH_RATE_LIMIT_MAX_WAIT seconds away: one request after the reset instead of blind ones inside the empty window.
Further than that, or unreadable, it backs off as any throttle.

Every gh and curl call runs under tool_bounded.py (a process group of its own, ended whole at its bound). gh is
$KINOWO_GH (default gh)."""
from __future__ import annotations

import os
import random
import re
import shutil
import subprocess
import sys
import tempfile
from typing import List, Mapping, MutableMapping, Optional, Sequence, Tuple

sys.path.insert(0, os.path.dirname(os.path.realpath(__file__)))  # run as `python3 -I`: its own directory is not there
import machine  # noqa: E402
import tool_python  # noqa: E402

HERE = os.path.dirname(os.path.realpath(__file__))
BOUNDED = os.path.join(HERE, "tool_bounded.py")
OK, FAILED, ABSENT, NOT_SERVED = 0, 1, 3, 4
ABSENT_ANSWERS = re.compile(r"no assets match|release not found|no artifact matches|no valid artifacts found|HTTP 404|"
                            r"HTTP 410")
THROTTLED = re.compile(r"HTTP 5[0-9][0-9]|HTTP 429|rate limit|Egress is over|ServerBusy", re.IGNORECASE)
RATE_LIMITED = re.compile(r"API rate limit exceeded", re.IGNORECASE)
URL_SAFE = re.compile(r"[A-Za-z0-9_.~+-]+")
REPOSITORY = re.compile(r"[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+")
OPTIONS = {"-R": "repo", "--repo": "repo", "-p": "name", "--pattern": "name", "-D": "dir", "--dir": "dir",
           "-O": "output", "--output": "output"}
RATE_LIMIT_RESET = "[.resources.core, .resources.graphql] | map(select(.remaining == 0) | .reset) | max // empty"


def bounded(seconds: int, argv: Sequence[str]) -> List[str]:
    return tool_python.isolated(BOUNDED, str(seconds), *argv, python=sys.executable)


def number(env: Mapping[str, str], name: str, default: int) -> int:
    return int(env.get(name) or default)


def gh(env: Mapping[str, str]) -> str:
    return env.get("KINOWO_GH") or "gh"


def say(line: str) -> None:
    sys.stderr.write(line + "\n")
    sys.stderr.flush()


def captured(env: MutableMapping[str, str], seconds: int, argv: Sequence[str], quiet: bool) -> Tuple[int, str]:
    """A bounded command's status and stdout; its stderr this process's, or dropped when `quiet`."""
    return machine.run(bounded(seconds, argv), machine.Ran(env=env, out=subprocess.PIPE,
                                                          err=subprocess.DEVNULL if quiet else None))


# ── the public asset URL ─────────────────────────────────────────────────────────────────────────────
def public_enabled(env: Mapping[str, str]) -> bool:
    switch = env.get("GH_DOWNLOAD_PUBLIC", "")
    return switch == "1" or (switch != "0" and env.get("GITHUB_ACTIONS") == "true")


def public_fetch(env: MutableMapping[str, str], seconds: int, args: Sequence[str]) -> int:
    if not public_enabled(env) or len(args) < 3 or list(args[:2]) != ["release", "download"]:
        return NOT_SERVED
    asked = public_asked(env, args[2], args[3:])
    if asked is None:
        return NOT_SERVED
    repo, tag, name, dest = asked
    return public_into(env, seconds, f"https://github.com/{repo}/releases/download/{tag}/{name}", dest,
                       f"{repo} {tag}/{name}")


def public_asked(env: Mapping[str, str], tag: str, words: Sequence[str]) -> Optional[Tuple[str, str, str, str]]:
    """(repo, tag, name, destination) of a download the public URL serves; None for any other."""
    given = {"repo": env.get("GH_REPO") or env.get("GITHUB_REPOSITORY") or "", "name": "", "dir": ".", "output": ""}
    clobber, i = False, 0
    while i < len(words):
        if words[i] == "--clobber":
            clobber, i = True, i + 1
            continue
        key = OPTIONS.get(words[i])
        if key is None or i + 1 >= len(words) or (key == "name" and given["name"]):
            return None
        given[key], i = words[i + 1], i + 2
    dest = given["output"] or os.path.join(given["dir"], given["name"])
    return servable(given["repo"], tag, given["name"], dest, clobber)


def servable(repo: str, tag: str, name: str, dest: str, clobber: bool) -> Optional[Tuple[str, str, str, str]]:
    """owner/name, and a tag and an asset name of URL-safe characters only: a glob, a host prefix or anything to escape
    stays gh's; nor a file it would overwrite without --clobber."""
    if not (REPOSITORY.fullmatch(repo) and URL_SAFE.fullmatch(tag) and URL_SAFE.fullmatch(name)):
        return None
    if dest != "-" and not clobber and os.path.lexists(dest):
        return None
    return repo, tag, name, dest


def part_of(dest: str) -> Optional[str]:
    """Where the fetch writes before it lands: a temporary file for stdout, else beside `dest`; None when its directory
    cannot be made."""
    if dest == "-":
        fd, part = tempfile.mkstemp()
        os.close(fd)
        return part
    try:
        os.makedirs(os.path.dirname(dest) or ".", exist_ok=True)
    except OSError:
        return None
    return dest + ".part"


def public_into(env: MutableMapping[str, str], seconds: int, url: str, dest: str, what: str) -> int:
    part = part_of(dest)
    if part is None:
        return FAILED
    status, code = captured(env, seconds, [env.get("KINOWO_CURL") or "curl", "-sSL", "--connect-timeout", "20",
                                           "--speed-limit", env.get("GH_DOWNLOAD_STALL_BYTES") or "1024",
                                           "--speed-time", env.get("GH_DOWNLOAD_STALL_SECONDS") or "60",
                                           "-o", part, "-w", "%{http_code}", url], quiet=False)
    if status == 0 and code == "200":
        return landed(part, dest)
    remove(part)
    if code == "404":
        return ABSENT
    # curl 28: the transfer stalled under the speed limit (or the bound ended it)
    say(f"public asset {what}: curl exit {status}, HTTP {code or 'none'}")
    return FAILED


def landed(part: str, dest: str) -> int:
    if dest == "-":
        with open(part, "rb") as f:
            shutil.copyfileobj(f, sys.stdout.buffer)
        sys.stdout.flush()
        remove(part)
    else:
        os.replace(part, dest)
    return OK


def remove(path: str) -> None:
    try:
        os.remove(path)
    except OSError:
        pass


# ── gh, until it answers ─────────────────────────────────────────────────────────────────────────────
def download(env: MutableMapping[str, str], seconds: int, args: Sequence[str]) -> int:
    if public_got(env, seconds, args):
        return OK
    attempt = 1
    while True:
        status, err = machine.errors(bounded(seconds, [gh(env), *args]), env=env, out=None)
        if err:
            sys.stderr.write(err.rstrip("\n") + "\n")
            sys.stderr.flush()
        verdict = settled(env, args, status, err, attempt)
        if verdict is not None:
            return verdict
        machine.sleep(backoff(env, args, err, status, attempt))
        emptied(args)
        attempt += 1


def what(args: Sequence[str]) -> str:
    return " ".join(list(args[:2]))


def public_got(env: MutableMapping[str, str], seconds: int, args: Sequence[str]) -> bool:
    if not public_enabled(env):
        return False
    status = public_fetch(env, seconds, args)
    if status not in (OK, NOT_SERVED):  # not a download the public URL serves: gh, quietly, as before
        say(f"gh {what(args)}: the public asset URL did not serve it (status {status}); asking the API")
    return status == OK


def settled(env: Mapping[str, str], args: Sequence[str], status: int, err: str, attempt: int) -> Optional[int]:
    """The answer this attempt settles, or None to ask again."""
    if status == 0:
        return OK
    exists = bool(env.get("GH_DOWNLOAD_RELEASE_EXISTS")) and "release not found" in err
    if ABSENT_ANSWERS.search(err) and not exists:
        return ABSENT
    if attempt >= attempts(env, err):
        say(f"gh {what(args)}: failed after {attempt} attempt(s) (exit {status})")
        return FAILED
    return None


def attempts(env: Mapping[str, str], err: str) -> int:
    if THROTTLED.search(err):
        return number(env, "GH_DOWNLOAD_TRANSIENT_ATTEMPTS", 10)
    return number(env, "GH_DOWNLOAD_ATTEMPTS", 3)


def backoff(env: MutableMapping[str, str], args: Sequence[str], err: str, status: int, attempt: int) -> int:
    reset = rate_limit_wait(env) if RATE_LIMITED.search(err) else None
    if reset is not None:
        say(f"gh {what(args)}: the API rate limit is spent; it resets in {reset} s -- asking again then "
            f"(attempt {attempt} of {attempts(env, err)})")
        return reset
    wait = min(number(env, "GH_DOWNLOAD_BACKOFF", 5) << (attempt - 1), number(env, "GH_DOWNLOAD_MAX_WAIT", 120))
    wait += random.randrange(wait // 2 + 1)
    say(f"gh {what(args)}: attempt {attempt} of {attempts(env, err)} failed (exit {status}); asking again in {wait}s")
    return wait


def emptied(args: Sequence[str]) -> None:
    """A `run download`'s --dir emptied: gh refuses to overwrite what a cut-short attempt left."""
    where = [args[i + 1] for i in range(len(args) - 1) if args[i] == "--dir"]
    if args[:1] == ["run"] and where and os.path.isdir(where[-1]):
        for entry in os.listdir(where[-1]):
            path = os.path.join(where[-1], entry)
            shutil.rmtree(path) if os.path.isdir(path) and not os.path.islink(path) else os.remove(path)


def rate_limit_wait(env: MutableMapping[str, str]) -> Optional[int]:
    status, reset = captured(env, 30, [gh(env), "api", "rate_limit", "--jq", RATE_LIMIT_RESET], quiet=True)
    reset = reset.strip()
    if status != 0 or not reset.isdigit():
        return None
    wait = max(0, int(reset) - int(machine.wall()))
    if wait > number(env, "GH_RATE_LIMIT_MAX_WAIT", 600):
        return None
    return wait + number(env, "GH_RATE_LIMIT_SLACK", 5) + random.randrange(number(env, "GH_RATE_LIMIT_JITTER", 10) + 1)


def prod_latest_run(env: MutableMapping[str, str], cc: str) -> int:
    release = env.get("FIXTURE_RELEASE_TAG") or "convergence-fixtures"
    scratch = tempfile.mkdtemp()
    try:
        got = download(env, number(env, "KINOWO_IDENTITY_GH_TIMEOUT", 60), [
            "release", "download", release, "-R", env.get("KINOWO_REPO") or "pawelkrupinski/movies",
            "--pattern", f"prod-latest-{cc}.txt", "--dir", scratch])
        if got != OK:
            say(f"no prod-latest {cc} recording (no prod-latest-{cc}.txt on {release}, or it could not be read): the "
                f"nightly \"Record scrape fixtures\" writes it once a {cc} leg completes")
            return FAILED
        with open(os.path.join(scratch, f"prod-latest-{cc}.txt")) as f:
            run = f.read().split("\n", 1)[0].split(" ", 1)[0]
    finally:
        shutil.rmtree(scratch, ignore_errors=True)
    if not run.isdigit():
        say(f"prod-latest-{cc}.txt names no run")
        return FAILED
    print(run, flush=True)
    return OK


def seconds_of(word: str) -> int:
    if not word.isdigit() or int(word) == 0:
        raise ValueError(word)
    return int(word)


def print_rate_limit_wait(env: MutableMapping[str, str]) -> int:
    wait = rate_limit_wait(env)
    if wait is None:
        return FAILED
    print(wait)
    return OK


def bounded_command(env: MutableMapping[str, str], command: str, rest: List[str]) -> int:
    try:
        seconds = seconds_of(rest[0])
    except ValueError as bad:
        print(f"gh_download.py: <seconds> must be a positive whole number, not '{bad}'", file=sys.stderr)
        return 64
    if command == "public":
        return public_fetch(env, seconds, rest[1:])
    return download(env, seconds, rest[1:])


def main(argv: List[str]) -> int:
    env = machine.environment()
    command, rest = (argv[0], argv[1:]) if argv else ("", [])
    if command in ("download", "public") and rest:
        return bounded_command(env, command, rest)
    if command == "rate-limit-wait" and not rest:
        return print_rate_limit_wait(env)
    if command == "prod-latest" and len(rest) == 1:
        return prod_latest_run(env, rest[0])
    print("usage: gh_download.py download|public <seconds> <gh args...> | rate-limit-wait | prod-latest <cc>",
          file=sys.stderr)
    return 64


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
