#!/usr/bin/env python3
"""Where a Claude Code session's subagents spent their time: hours by bucket, the longest agents, one agent's slowest
steps. The logic behind scripts/agent-time (its header has the usage); tests: scripts/agent_time_test.py.

Each subagent's transcript is a JSONL file in the session's tasks directory (`<id>.output`, its first line carrying
"agentId"; the `b*.output` beside them are background commands' logs). Every second between two of an agent's events
goes to ONE bucket, so parallel tool calls are never counted twice:
  - a tool call in flight: the call's bucket -- a foreground Bash command by its kind (an sbt run by its shape, the
    pre-push hook's `git push`, a measurement, a slice...), a polling loop or `wait-for` by what it waits for (`poll: <kind>`: a CI
    run, or a background command's output file, by the command that launched it);
  - nothing in flight and the next event a background task's notification: `bg wait: <kind>`, the kind of the command
    that launched the task (measure, slice, sbt suite, local convergence, CI wait, ...) or `monitor`;
  - nothing in flight otherwise: `idle gap >3m` past three minutes, else `model` (the model thinking and writing).
"""
from __future__ import annotations

import argparse
import datetime
import glob
import json
import os
import re
import subprocess
import sys
from collections import Counter
from typing import Any, Dict, List, NamedTuple, Optional, Set, Tuple, Union

IDLE_GAP_SECONDS = 180
POLL = re.compile(r"\bsleep\s+\d|(?<!-)\buntil\b(?!-)|\bwhile\b|for \w+ in \$\(seq|\bgh run watch\b|\bci-wait\b"
                  r"|(?:^|[\s/;&|])wait-for\b")
RUN_ID = re.compile(r"\b3\d{10}\b")
TASK_OUTPUT = re.compile(r"tasks/(b[a-z0-9]+)\.output")
LAUNCHED = re.compile(r"running in background with ID: (b[a-z0-9]+)")
NOTIFIED = re.compile(r"<task-notification>.*?<task-id>([a-z0-9]+)</task-id>", re.S)


class ToolUse(NamedTuple):
    tool_id: Optional[str]
    name: Optional[str]
    given: Dict[str, Any]


class ToolResult(NamedTuple):
    tool_id: Optional[str]
    text: str


# (time, kind, payload): use (a ToolUse), result (a ToolResult), notify (a task id), model, user (none)
Event = Tuple[datetime.datetime, str, Union[None, str, ToolUse, ToolResult]]
OpenCall = Tuple[datetime.datetime, str, str]  # (start, bucket, what)


def timestamp(text: str) -> datetime.datetime:
    return datetime.datetime.fromisoformat(text.replace("Z", "+00:00"))


def sbt_shape(command: str) -> str:
    if re.search(r"FilmScheduleEndToEnd|PageSnapshot|PageTest", command):
        return "sbt: snapshots / page tests"
    if re.search(r"testOnly", command):
        return "sbt: testOnly"
    if re.search(r"\btestUnit\b|\bitAll\b|[/ \"]test\b|\btest\"|IntegrationTest/test\b", command):
        return "sbt: suite"
    if re.search(r"runMain|\brun\b", command):
        return "sbt: runMain"
    if re.search(r"compile", command):
        return "sbt: compile"
    return "sbt: other"


LAND = re.compile(r"\bagentctl (land|inbox wait|status|wait)\b|\breland\b|\bwaitland")
WHAT_IF = "identity-what-if"


CONVERGENCE = re.compile(r"convergence-local|country-convergence|Convergence(Spec|Poland|Germany|Uk|Us|Spain)|convergence[A-Z]"
                         r"|convergence leg|legs\.sh", re.I)
CI_WAIT = re.compile(r"\bgh run (watch|view|list)\b|\bci-wait\b|\b3\d{10}\b")
SBT = re.compile(r"sbt-warm|\bsbt\b")
# a background command's kind by the first pattern its text matches
LAUNCH_KINDS = ((LAND, "land"), (re.compile(WHAT_IF), "what-if"), (re.compile("identity-measure"), "measure"),
                (re.compile("identity-slice"), "slice"), (CONVERGENCE, "local convergence"), (SBT, "sbt suite"),
                (CI_WAIT, "CI wait"), (re.compile(r"mongodump|mongoexport|\bdump\b"), "dump"), (re.compile("git push"), "pre-push"))


def launch_kind(command: str) -> str:
    """What a background command was, for the waits on it."""
    return next((kind for pattern, kind in LAUNCH_KINDS if pattern.search(command)), "other")


WRITES = re.compile(r"(?:>>?|\btee(?: -a)?)\s*\"?([^\s;|&\"<>()]+)")
QUICK = re.compile(r"^\s*(cd [^;&]+(;|&&)\s*)?(grep|sed|cat|ls|head|tail|wc|find|awk|echo|git (log|show|diff|status))\b")


PATH_TOKEN = re.compile(r"[^\s\"';|&<>()]*/[^\s\"';|&<>()]*")
CD = re.compile(r"\bcd\s+\"?([^\s;&\"]+)")
WAIT_FOR = re.compile(r"(?:^|[\s/;&|])wait-for\s+(?:--\S+\s+\S+\s+)*(\S+)\s*(\"[^\"]*\"|'[^']*'|\S+)?")


def resolved(path: str, command: str) -> str:
    """<path> made absolute against the command's `cd` directory; as written when it has none to resolve against."""
    if path.startswith("/"):
        return os.path.normpath(path)
    cd = CD.search(command)
    if cd and cd.group(1).startswith("/"):
        return os.path.normpath(os.path.join(cd.group(1), path))
    return os.path.normpath(path)


def same_file(a: str, b: str) -> bool:
    """Two resolved paths name one file: equal, or one is still relative and is the tail of the other's path (a
    command without a `cd` writing `target/x.log`, waited on as `/work/tree/target/x.log`)."""
    if a == b:
        return True
    if a.startswith("/") and not b.startswith("/"):
        a, b = b, a
    return b.startswith("/") and not a.startswith("/") and a.count("/") >= 1 and b.endswith("/" + a)


# (the file's name, the pattern waited for, the kind) by the first row that matches: either one
WAIT_TARGETS = ((re.compile("what-?if", re.I), None, "what-if"), (re.compile("measure", re.I), re.compile("LANDABLE"), "measure"),
                (re.compile("slice", re.I), None, "slice"), (re.compile("convergence|legs|conv-", re.I), None, "local convergence"),
                (re.compile("testUnit|itAll|sbt|PageTest|test", re.I), re.compile(r"\^exit "), "sbt suite"))


def waits_for_land(target: str, pattern: str) -> bool:
    return bool(re.search(r"(^|/)(re)?land[^/]*$", target, re.I)) or (bool(re.search(r"\bLANDED\b", pattern)) and "LANDABLE" not in pattern)


def wait_kind(command: str) -> str | None:
    """What a `wait-for <file> <pattern>` waits on, by the file's name and the pattern, when no launch explains it."""
    wait = WAIT_FOR.search(command)
    if not wait:
        return None
    target, pattern = wait.group(1), wait.group(2) or ""
    if waits_for_land(target, pattern):
        return "land"
    return next((kind for on_target, on_pattern, kind in WAIT_TARGETS
                 if on_target.search(target) or (on_pattern and on_pattern.search(pattern))), None)


class Launches:
    """Every background command of the session, by its task id and by the files it writes: what a wait on it waited
    for. Read from every transcript first, as one agent waits on what another launched."""

    def __init__(self):
        self.by_task = {}
        self.by_file = {}

    def add(self, task: str, command: str) -> None:
        kind = launch_kind(command)
        self.by_task[task] = kind
        for path in WRITES.findall(command):
            if "/" in path and not path.startswith("/dev/"):
                self.by_file[resolved(path, command)] = kind

    def waited_on(self, command: str) -> str | None:
        task = TASK_OUTPUT.search(command)
        if task and task.group(1) in self.by_task:
            return self.by_task[task.group(1)]
        waited = [resolved(token, command) for token in PATH_TOKEN.findall(command)]
        for path, kind in self.by_file.items():
            if any(same_file(path, token) for token in waited):
                return kind
        return None


# a polling command's bucket by the first pattern its text matches, once no launch or file explains it
POLL_KINDS = ((LAND, "poll: land"), (re.compile("identity-measure"), "poll: measure"), (re.compile("identity-slice"), "poll: slice"),
              (re.compile(r"\bgh (run|api)\b|\bci-wait\b|record-scrape|\b3\d{10}\b"), "poll: CI wait"),
              (re.compile(r"=== \w+ exit|legs|conv-|convergence|order\.log"), "poll: local convergence"),
              (re.compile(r"Total time|\[success\]|Tests: |\[error\]"), "poll: sbt suite"))
# a foreground command's bucket (a label, or a function of the command) by the first pattern its text matches
FOREGROUND_BUCKETS = ((re.compile("git push"), "pre-push (git push)"), (LAND, "land (foreground)"),
                      (re.compile(WHAT_IF), "what-if (foreground)"), (re.compile("identity-measure"), "measure (foreground)"),
                      (re.compile("identity-slice|identity-explain"), "slice (foreground)"), (re.compile("fail-before"), "fail-before"),
                      (re.compile("convergence-local"), "local convergence (foreground)"),
                      (re.compile(r"run-script-tests|-test\.sh|_test\.py"), "script tests"), (SBT, sbt_shape),
                      (re.compile(r"\bgh\b"), "gh"), (re.compile(r"\bgit\b"), "git"))


def poll_bucket(command: str, launches: Launches) -> str:
    kind = launches.waited_on(command)
    if kind and kind != "other":
        return "poll: " + kind
    kind = wait_kind(command)
    if kind:
        return "poll: " + kind
    return next((bucket for pattern, bucket in POLL_KINDS if pattern.search(command)), "poll: other")


def bash_bucket(command: str, launches: Launches) -> str:
    """A foreground Bash command's bucket."""
    if POLL.search(command):
        return poll_bucket(command, launches)
    for pattern, bucket in FOREGROUND_BUCKETS:
        if pattern.search(command):
            return bucket(command) if callable(bucket) else bucket
    return "bash: other"


def text_of(content: object) -> str:
    if isinstance(content, str):
        return content
    if isinstance(content, list):
        return " ".join(b.get("text", "") if isinstance(b, dict) else str(b) for b in content)
    return ""


def is_agent_transcript(path: str) -> bool:
    try:
        with open(path, encoding="utf-8") as f:
            return '"agentId"' in f.readline()
    except (OSError, UnicodeDecodeError):
        return False


def block_events(at: datetime.datetime, content: object) -> List[Event]:
    """The tool calls and results among a message's content blocks."""
    events: List[Event] = []
    for block in content if isinstance(content, list) else []:
        if not isinstance(block, dict):
            continue
        if block.get("type") == "tool_use":
            events.append((at, "use", ToolUse(block.get("id"), block.get("name"), block.get("input") or {})))
        elif block.get("type") == "tool_result":
            events.append((at, "result", ToolResult(block.get("tool_use_id"), text_of(block.get("content")))))
    return events


def record_events(record: Dict[str, Any]) -> List[Event]:
    """The events of one transcript record (none when it holds no timestamped message)."""
    when, message = record.get("timestamp"), record.get("message")
    if not when or not isinstance(message, dict):
        return []
    at, content = timestamp(when), message.get("content")
    events: List[Event] = [(at, "model", None)] if record.get("type") == "assistant" else []
    events += block_events(at, content)
    if record.get("type") == "user":
        notified = NOTIFIED.search(text_of(content))
        if notified:
            events.append((at, "notify", notified.group(1)))
        elif isinstance(content, str):
            events.append((at, "user", None))
    return events


def read_events(path: str) -> List[Event]:
    """(time, kind, payload) of one transcript: use (id, name, input), result (tool id, text), notify (task id),
    model, user."""
    events: List[Event] = []
    with open(path, encoding="utf-8") as f:
        for line in f:
            try:
                record = json.loads(line)
            except ValueError:
                continue
            events += record_events(record)
    events.sort(key=lambda e: e[0])
    return events


def background_launches(events: List[Event], launches: Launches) -> None:
    """The background commands one transcript launched, into <launches>."""
    commands: Dict[Optional[str], str] = {}
    for _, _, payload in events:
        if isinstance(payload, ToolUse) and payload.name in ("Bash", "bash") and payload.given.get("run_in_background"):
            # the description too: `bash scratchpad/legs.sh` says what it is only there ("Run four convergence legs")
            commands[payload.tool_id] = payload.given.get("command", "") + "\n" + str(payload.given.get("description", ""))
        elif isinstance(payload, ToolResult) and payload.tool_id in commands:
            launched = LAUNCHED.search(payload.text)
            if launched:
                launches.add(launched.group(1), commands[payload.tool_id])


class Agent:
    def __init__(self, agent_id: str, events: List[Event], launches: Launches, since: Optional[datetime.datetime] = None):
        self.id = agent_id
        self.since = since  # count only the seconds after it (a datetime), None: all
        self.buckets = Counter()
        self.steps: List[Tuple[datetime.datetime, float, str, str]] = []  # (start, seconds, bucket, what)
        self.span = (events[-1][0] - events[0][0]).total_seconds() if events else 0.0
        self._sweep(events, launches)

    def _sweep(self, events: List[Event], launches: Launches) -> None:
        monitors: Set[str] = set()  # task ids of Monitor watches
        background: Set[str] = set()  # tool ids of background launches and monitors: they return at once
        open_calls: Dict[Optional[str], OpenCall] = {}
        for (at, _, payload), following in zip(events, [*events[1:], None]):
            if isinstance(payload, ToolUse):
                self._open(at, payload, launches, background, open_calls)
            elif isinstance(payload, ToolResult):
                self._close(at, payload, monitors, background, open_calls)
            if following is None:
                break
            gap = (following[0] - at).total_seconds()
            if gap > 0 and (self.since is None or at >= self.since):
                self.buckets[self._gap_bucket(at, following, gap, open_calls, monitors, launches)] += gap

    def _open(self, at: datetime.datetime, payload: ToolUse, launches: Launches, background: Set[str],
              open_calls: Dict[Optional[str], OpenCall]) -> None:
        """A tool call begins: a background launch or monitor returns at once; any other stays open until its result."""
        tool_id, name, given = payload
        command = given.get("command", "")
        if (name in ("Bash", "bash") and given.get("run_in_background")) or name == "Monitor":
            background.add(str(tool_id) if name != "Monitor" else "monitor:" + str(tool_id))
            return
        bucket = bash_bucket(command, launches) if name in ("Bash", "bash") else "tool: " + str(name)
        if bucket == "bash: other" and QUICK.search(command):
            bucket = "quick command"
        open_calls[tool_id] = (at, bucket, command or str(given.get("description", name))[:200])

    def _close(self, at: datetime.datetime, payload: ToolResult, monitors: Set[str], background: Set[str],
               open_calls: Dict[Optional[str], OpenCall]) -> None:
        """A tool result: the call's step is recorded, a Monitor's result names the tasks it watches."""
        tool_id, text = payload
        if "monitor:" + str(tool_id) in background:
            monitors.update(re.findall(r"\b(b[a-z0-9]{7,})\b", text))
        if tool_id in open_calls:
            start, bucket, what = open_calls.pop(tool_id)
            if self.since is None or start >= self.since:
                self.steps.append((start, (at - start).total_seconds(), bucket, what))

    def _gap_bucket(self, at: datetime.datetime, following: Event, gap: float, open_calls: Dict[Optional[str], OpenCall],
                    monitors: Set[str], launches: Launches) -> str:
        """The bucket the seconds before the next event go to (a wait or an idle gap is also a step)."""
        if open_calls:
            return min(open_calls.values(), key=lambda call: call[0])[1]
        if following[1] == "notify":
            task = str(following[2])
            bucket = "bg wait: " + ("monitor" if task in monitors else "subagent" if re.fullmatch(r"a[0-9a-f]{16}", task)
                                    else launches.by_task.get(task, "other"))
            self.steps.append((at, gap, bucket, "until task " + task))
            return bucket
        if gap > IDLE_GAP_SECONDS:
            self.steps.append((at, gap, "idle gap >3m", "next: " + following[1]))
            return "idle gap >3m"
        return "model"


def load(tasks_dir: str, since: Optional[datetime.datetime] = None) -> List[Agent]:
    transcripts = {}
    launches = Launches()
    sessions = set()
    for path in sorted(glob.glob(os.path.join(tasks_dir, "*.output"))):
        if is_agent_transcript(path):
            # <project>/<session>/subagents/agent-<id>.jsonl: the session's own transcript, <project>/<session>.jsonl,
            # launched what its agents may wait on too
            sessions.add(os.path.dirname(os.path.dirname(os.path.realpath(path))) + ".jsonl")
            events = read_events(path)
            background_launches(events, launches)
            if len(events) >= 2:
                transcripts[os.path.basename(path)[: -len(".output")]] = events
    for session in sessions:
        if os.path.isfile(session):
            background_launches(read_events(session), launches)
    agents = [Agent(agent_id, events, launches, since) for agent_id, events in transcripts.items()]
    return [a for a in agents if sum(a.buckets.values()) > 0] if since is not None else agents


def hours(seconds: float) -> str:
    return f"{seconds / 3600:.1f}"


def minutes(seconds: float) -> str:
    return f"{seconds / 60:.0f}m"


def report(agents: List[Agent], tasks_dir: str, longest: int = 10) -> str:
    total = Counter()
    for agent in agents:
        total.update(agent.buckets)
    whole = sum(total.values()) or 1.0
    lines = [f"agent-time: {tasks_dir}", f"{len(agents)} agents, {hours(whole)} agent-hours", "",
             f"  {'bucket':34s} {'hours':>6s} {'share':>6s}"]
    for bucket, seconds in total.most_common():
        lines.append(f"  {bucket:34s} {hours(seconds):>6s} {100 * seconds / whole:5.1f}%")
    lines += ["", f"the {min(longest, len(agents))} longest agents:"]
    for agent in sorted(agents, key=lambda a: -sum(a.buckets.values()))[:longest]:
        top = ", ".join(f"{bucket} {minutes(seconds)}" for bucket, seconds in agent.buckets.most_common(3))
        lines.append(f"  {agent.id:18s} {hours(sum(agent.buckets.values())):>5s}h  {top}")
    return "\n".join(lines)


def timeline(agent: Agent, top: int = 20) -> str:
    whole = sum(agent.buckets.values())
    lines = [f"agent {agent.id}: {hours(whole)} h, " +
             ", ".join(f"{bucket} {minutes(seconds)}" for bucket, seconds in agent.buckets.most_common(5)),
             f"its {top} slowest steps, in order:"]
    slowest = sorted(sorted(agent.steps, key=lambda s: -s[1])[:top], key=lambda s: s[0])
    for start, seconds, bucket, what in slowest:
        what = " ".join(what.split())
        lines.append(f"  {start.strftime('%m-%d %H:%M:%S')} {minutes(seconds):>5s}  {bucket:30s} {what[:110]}")
    return "\n".join(lines)


def default_tasks_dir() -> str | None:
    """The newest session's tasks directory of this checkout's Claude Code project (its main checkout's path)."""
    try:
        common = subprocess.run(["git", "rev-parse", "--path-format=absolute", "--git-common-dir"],
                                capture_output=True, text=True, check=True).stdout.strip()
        project = os.path.dirname(common) if common.endswith("/.git") else os.getcwd()
    except (OSError, subprocess.CalledProcessError):
        project = os.getcwd()
    slug = re.sub(r"[^A-Za-z0-9]", "-", project)
    roots = [os.environ["AGENT_TIME_ROOT"]] if os.environ.get("AGENT_TIME_ROOT") else \
        [f"/private/tmp/claude-{os.getuid()}", f"/tmp/claude-{os.getuid()}"]
    found = [d for root in roots for d in glob.glob(os.path.join(root, slug, "*", "tasks")) if os.path.isdir(d)]
    return max(found, key=os.path.getmtime) if found else None


DOCSTRING = __doc__ or ""  # None under python -OO


def main(argv: List[str]) -> int:
    parser = argparse.ArgumentParser(prog="agent-time", description=DOCSTRING.splitlines()[0])
    parser.add_argument("tasks_dir", nargs="?", help="a session's tasks directory (default: the newest session's)")
    parser.add_argument("--agent", help="one agent's slowest steps (its id, or a prefix of it)")
    parser.add_argument("--top", type=int, default=20, help="steps --agent lists (default 20)")
    parser.add_argument("--since", help="count only the time after this ISO instant (e.g. 2026-11-19T00:00Z)")
    parser.add_argument("--longest", type=int, default=10, help="agents the summary lists (default 10)")
    args = parser.parse_args(argv)
    tasks_dir = args.tasks_dir or default_tasks_dir()
    if not tasks_dir or not os.path.isdir(tasks_dir):
        print(f"agent-time: no tasks directory ({tasks_dir or 'none found for this checkout'})", file=sys.stderr)
        return 2
    agents = load(tasks_dir, timestamp(args.since) if args.since else None)
    if args.agent:
        matching = [a for a in agents if a.id.startswith(args.agent)]
        if len(matching) != 1:
            print(f"agent-time: {len(matching)} agents match {args.agent}", file=sys.stderr)
            return 2
        print(timeline(matching[0], args.top))
        return 0
    if not agents:
        print(f"agent-time: no subagent transcripts in {tasks_dir}", file=sys.stderr)
        return 1
    print(report(agents, tasks_dir, args.longest))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
