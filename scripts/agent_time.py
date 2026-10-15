#!/usr/bin/env python3
"""Where a Claude Code session's subagents spent their time: hours by bucket, the longest agents, one agent's slowest
steps. The logic behind scripts/agent-time (its header has the usage); tests: scripts/agent_time_test.py.

Each subagent's transcript is a JSONL file in the session's tasks directory (`<id>.output`, its first line carrying
"agentId"; the `b*.output` beside them are background commands' logs). Every second between two of an agent's events
goes to ONE bucket, so parallel tool calls are never counted twice:
  - a tool call in flight: the call's bucket -- a foreground Bash command by its kind (an sbt run by its shape, the
    pre-push hook's `git push`, a measurement, a slice...), a polling loop by what it waits for (`poll: <kind>`: a CI
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

IDLE_GAP_SECONDS = 180
POLL = re.compile(r"\bsleep\s+\d|\buntil\b|\bwhile\b|for \w+ in \$\(seq|\bgh run watch\b")
RUN_ID = re.compile(r"\b3\d{10}\b")
TASK_OUTPUT = re.compile(r"tasks/(b[a-z0-9]+)\.output")
LAUNCHED = re.compile(r"running in background with ID: (b[a-z0-9]+)")
NOTIFIED = re.compile(r"<task-notification>.*?<task-id>([a-z0-9]+)</task-id>", re.S)


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


def launch_kind(command: str) -> str:
    """What a background command was, for the waits on it."""
    if "identity-measure" in command:
        return "measure"
    if "identity-slice" in command:
        return "slice"
    if re.search(r"convergence-local|country-convergence|Convergence(Spec|Poland|Germany|Uk|Us|Spain)|convergence[A-Z]"
                 r"|convergence leg|legs\.sh", command, re.I):
        return "local convergence"
    if re.search(r"sbt-warm|\bsbt\b", command):
        return "sbt suite"
    if re.search(r"\bgh run (watch|view|list)\b", command) or RUN_ID.search(command):
        return "CI wait"
    if re.search(r"mongodump|mongoexport|\bdump\b", command):
        return "dump"
    if "git push" in command:
        return "pre-push"
    return "other"


WRITES = re.compile(r"(?:>>?|\btee(?: -a)?)\s*\"?([^\s;|&\"<>()]+)")
QUICK = re.compile(r"^\s*(cd [^;&]+(;|&&)\s*)?(grep|sed|cat|ls|head|tail|wc|find|awk|echo|git (log|show|diff|status))\b")


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
                self.by_file[path] = kind

    def waited_on(self, command: str) -> str | None:
        task = TASK_OUTPUT.search(command)
        if task and task.group(1) in self.by_task:
            return self.by_task[task.group(1)]
        for path, kind in self.by_file.items():
            if path in command:
                return kind
        return None


def bash_bucket(command: str, launches: Launches) -> str:
    """A foreground Bash command's bucket."""
    if POLL.search(command):
        kind = launches.waited_on(command)
        if kind and kind != "other":
            return "poll: " + kind
        if "identity-measure" in command:
            return "poll: measure"
        if "identity-slice" in command:
            return "poll: slice"
        if re.search(r"\bgh (run|api)\b|record-scrape", command) or RUN_ID.search(command):
            return "poll: CI wait"
        if re.search(r"=== \w+ exit|legs|conv-|convergence|order\.log", command):
            return "poll: local convergence"
        if re.search(r"Total time|\[success\]|Tests: |\[error\]", command):
            return "poll: sbt suite"
        return "poll: other"
    if "git push" in command:
        return "pre-push (git push)"
    if "identity-measure" in command:
        return "measure (foreground)"
    if re.search(r"identity-slice|identity-explain", command):
        return "slice (foreground)"
    if "fail-before" in command:
        return "fail-before"
    if "convergence-local" in command:
        return "local convergence (foreground)"
    if re.search(r"run-script-tests|-test\.sh|_test\.py", command):
        return "script tests"
    if re.search(r"sbt-warm|\bsbt\b", command):
        return sbt_shape(command)
    if re.search(r"\bgh\b", command):
        return "gh"
    if re.search(r"\bgit\b", command):
        return "git"
    return "bash: other"


def text_of(content) -> str:
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


def read_events(path: str) -> list:
    """(time, kind, payload) of one transcript: use (id, name, input), result (tool id, text), notify (task id),
    model, user."""
    events = []
    with open(path, encoding="utf-8") as f:
        for line in f:
            try:
                record = json.loads(line)
            except ValueError:
                continue
            when = record.get("timestamp")
            message = record.get("message")
            if not when or not isinstance(message, dict):
                continue
            at = timestamp(when)
            content = message.get("content")
            if record.get("type") == "assistant":
                events.append((at, "model", None))
            if isinstance(content, list):
                for block in content:
                    if not isinstance(block, dict):
                        continue
                    if block.get("type") == "tool_use":
                        events.append((at, "use", (block.get("id"), block.get("name"), block.get("input") or {})))
                    elif block.get("type") == "tool_result":
                        events.append((at, "result", (block.get("tool_use_id"), text_of(block.get("content")))))
            if record.get("type") == "user":
                notified = NOTIFIED.search(text_of(content))
                if notified:
                    events.append((at, "notify", notified.group(1)))
                elif isinstance(content, str):
                    events.append((at, "user", None))
    events.sort(key=lambda e: e[0])
    return events


def background_launches(events: list, launches: Launches) -> None:
    """The background commands one transcript launched, into <launches>."""
    commands = {}
    for _, kind, payload in events:
        if kind == "use" and payload[1] in ("Bash", "bash") and isinstance(payload[2], dict) \
                and payload[2].get("run_in_background"):
            # the description too: `bash scratchpad/legs.sh` says what it is only there ("Run four convergence legs")
            commands[payload[0]] = payload[2].get("command", "") + "\n" + str(payload[2].get("description", ""))
        elif kind == "result" and payload[0] in commands:
            launched = LAUNCHED.search(payload[1])
            if launched:
                launches.add(launched.group(1), commands[payload[0]])


class Agent:
    def __init__(self, agent_id: str, events: list, launches: Launches):
        self.id = agent_id
        self.buckets = Counter()
        self.steps = []  # (start, seconds, bucket, what)
        self.span = (events[-1][0] - events[0][0]).total_seconds() if events else 0.0
        self._sweep(events, launches)

    def _sweep(self, events: list, launches: Launches) -> None:
        monitors = set()  # task ids of Monitor watches
        background = set()  # tool ids of background launches and monitors: they return at once
        open_calls = {}  # tool id -> (start, bucket, what)
        for (at, kind, payload), following in zip(events, events[1:] + [None]):
            if kind == "use":
                tool_id, name, given = payload
                command = given.get("command", "") if isinstance(given, dict) else ""
                if (name in ("Bash", "bash") and given.get("run_in_background")) or name == "Monitor":
                    background.add(tool_id if name != "Monitor" else "monitor:" + str(tool_id))
                else:
                    bucket = bash_bucket(command, launches) if name in ("Bash", "bash") else "tool: " + str(name)
                    if bucket == "bash: other" and QUICK.search(command):
                        bucket = "quick command"
                    open_calls[tool_id] = (at, bucket, command or str(given.get("description", name))[:200])
            elif kind == "result":
                tool_id, text = payload
                if "monitor:" + str(tool_id) in background:
                    monitors.update(re.findall(r"\b(b[a-z0-9]{7,})\b", text))
                if tool_id in open_calls:
                    start, bucket, what = open_calls.pop(tool_id)
                    self.steps.append((start, (at - start).total_seconds(), bucket, what))
            if following is None:
                break
            gap = (following[0] - at).total_seconds()
            if gap <= 0:
                continue
            if open_calls:
                bucket = min(open_calls.values(), key=lambda call: call[0])[1]
            elif following[1] == "notify":
                task = following[2]
                bucket = "bg wait: " + ("monitor" if task in monitors else "subagent" if re.fullmatch(r"a[0-9a-f]{16}", task)
                                        else launches.by_task.get(task, "other"))
                self.steps.append((at, gap, bucket, "until task " + following[2]))
            elif gap > IDLE_GAP_SECONDS:
                bucket = "idle gap >3m"
                self.steps.append((at, gap, bucket, "next: " + following[1]))
            else:
                bucket = "model"
            self.buckets[bucket] += gap


def load(tasks_dir: str) -> list:
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
    return [Agent(agent_id, events, launches) for agent_id, events in transcripts.items()]


def hours(seconds: float) -> str:
    return f"{seconds / 3600:.1f}"


def minutes(seconds: float) -> str:
    return f"{seconds / 60:.0f}m"


def report(agents: list, tasks_dir: str, longest: int = 10) -> str:
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


def main(argv: list) -> int:
    parser = argparse.ArgumentParser(prog="agent-time", description=__doc__.splitlines()[0])
    parser.add_argument("tasks_dir", nargs="?", help="a session's tasks directory (default: the newest session's)")
    parser.add_argument("--agent", help="one agent's slowest steps (its id, or a prefix of it)")
    parser.add_argument("--top", type=int, default=20, help="steps --agent lists (default 20)")
    parser.add_argument("--longest", type=int, default=10, help="agents the summary lists (default 10)")
    args = parser.parse_args(argv)
    tasks_dir = args.tasks_dir or default_tasks_dir()
    if not tasks_dir or not os.path.isdir(tasks_dir):
        print(f"agent-time: no tasks directory ({tasks_dir or 'none found for this checkout'})", file=sys.stderr)
        return 2
    agents = load(tasks_dir)
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
