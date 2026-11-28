"""THE WORKFLOWS' `run:` BLOCKS, read as shell (the owner's 2026-11-28 rule: a `run:` is a shell script written inside a
YAML file, so it is held to the shell gate -- scripts/design-rules/shell_gate.py -- and one past its limit moves to a
script, Python by the migration rule). Read from the text of a workflow (`.github/workflows/*.yml`) or a composite action
(`.github/actions/<name>/action.yml`): each step's `run:` value, a block scalar dedented and a one-line value as written,
with every `${{ ... }}` expression replaced by a word (an expression's `&&` is no shell decision).

A block is keyed `<file>#<job>/<step name>` (a step without a name: `step<N>`, N counting the job's steps from 1; a
composite action's job is `action`; a name given twice in a job: `#2`, `#3`...), so inserting a step never re-keys the
others, and a rename of a step is a new key (which a frozen row does not follow: it falls with the old name)."""
from __future__ import annotations

import re
from dataclasses import dataclass, field
from typing import Dict, List, Optional

JOBS = re.compile(r"^jobs:\s*(#.*)?$")
JOB = re.compile(r"^( +)([\w.-]+):\s*(#.*)?$")
STEPS = re.compile(r"^(\s*)steps:\s*(#.*)?$")
DASH = re.compile(r"^(\s*)- ")
KEY = re.compile(r"^(\s*)(?:- )?([\w-]+):\s?(.*)$")
EXPRESSION = re.compile(r"\$\{\{.*?\}\}", re.S)
UNSAFE_IN_KEY = re.compile(r"[#:\s]+")
ACTION = "action"


@dataclass
class Step:
    index: int
    name: str = ""
    run: Optional[str] = None


@dataclass
class Reader:
    """The state of one pass down a workflow's lines: the job and the steps list it is in, and the step being read."""
    job: str = ACTION
    in_jobs: bool = False
    job_indent: Optional[int] = None  # the column of the job ids, once the first one is seen
    item_indent: Optional[int] = None  # the column of a step's `- `, once the first one of the list is seen
    steps_indent: Optional[int] = None  # the column of the `steps:` key
    step: Optional[Step] = None
    count: int = 0
    found: Dict[str, Step] = field(default_factory=lambda: {})

    def leave_steps(self) -> None:
        self.flush()
        self.item_indent = self.steps_indent = None

    def flush(self) -> None:
        if self.step is not None and self.step.run is not None:
            name = UNSAFE_IN_KEY.sub("-", EXPRESSION.sub("X", self.step.name).strip("'\" ")).strip("-") or f"step{self.step.index}"
            key = f"{self.job}/{name}"
            seen = sum(1 for k in self.found if k == key or k.startswith(key + "#"))
            self.found[key if not seen else f"{key}#{seen + 1}"] = self.step
        self.step = None


def indent_of(line: str) -> int:
    return len(line) - len(line.lstrip())


def block_value(lines: List[str], start: int, key_indent: int) -> "tuple[str, int]":
    """The text of the block scalar whose `|` / `>` line is `lines[start]`, and the index of the line after it."""
    end = start + 1
    while end < len(lines) and (not lines[end].strip() or indent_of(lines[end]) > key_indent):
        end += 1
    body = [line for line in lines[start + 1:end]]
    margin = min((indent_of(line) for line in body if line.strip()), default=0)
    return "\n".join(line[margin:] for line in body).rstrip("\n"), end


def read_key(reader: Reader, lines: List[str], i: int, found: "re.Match[str]") -> int:
    """A `name:` or `run:` key of the current step at `lines[i]`: its value taken; the index of the next line to read."""
    key, value = found.group(2), found.group(3).strip()
    key_indent = indent_of(lines[i]) + (2 if DASH.match(lines[i]) else 0)
    if reader.step is None or key_indent != (reader.item_indent or 0) + 2:
        return i + 1
    if key == "name":
        reader.step.name = value
    elif key == "run" and value[:1] in ("|", ">"):
        reader.step.run, end = block_value(lines, i, key_indent)
        return end
    elif key == "run":
        reader.step.run = value.strip("'\"")
    return i + 1


def read_job(reader: Reader, line: str) -> None:
    """A line outside any steps list: a job id, once under `jobs:`, names the job the next steps belong to."""
    job = JOB.match(line)
    if job and reader.in_jobs and reader.job_indent in (None, len(job.group(1))):
        reader.job, reader.job_indent = job.group(2), len(job.group(1))


def read_step_line(reader: Reader, lines: List[str], i: int) -> int:
    """A line inside a steps list: a step starts at its `- `, its `name:` and `run:` keys are taken; the next index."""
    line = lines[i]
    dash = DASH.match(line)
    if indent_of(line) <= (reader.steps_indent or 0) and not dash:
        reader.leave_steps()
        return i  # the line that ended the list may open the next job
    if dash and (reader.item_indent is None or len(dash.group(1)) == reader.item_indent):
        reader.flush()
        reader.item_indent = len(dash.group(1))
        reader.count += 1
        reader.step = Step(reader.count)
    found = KEY.match(line)
    return read_key(reader, lines, i, found) if found else i + 1


def read_line(reader: Reader, lines: List[str], i: int) -> int:
    """One line's effect on the reader; the index of the next line to read."""
    line = lines[i]
    if not line.strip() or line.lstrip().startswith("#"):
        return i + 1
    steps = STEPS.match(line)
    if steps:
        reader.flush()
        reader.steps_indent, reader.item_indent, reader.count = len(steps.group(1)), None, 0
    elif JOBS.match(line):
        reader.in_jobs = True
    elif reader.steps_indent is None:
        read_job(reader, line)
    else:
        return read_step_line(reader, lines, i)
    return i + 1


def run_blocks(text: str) -> Dict[str, str]:
    """Each step's `run:` of the workflow or action: label (`<job>/<step>`) -> its shell text."""
    lines = text.split("\n")
    reader = Reader()
    i = 0
    while i < len(lines):
        i = read_line(reader, lines, i)
    reader.flush()
    return {label: EXPRESSION.sub("X", step.run or "") for label, step in reader.found.items()}


def holds_runs(path: str) -> bool:
    """A workflow or a composite action: the files whose steps may carry a `run:`."""
    return (path.startswith(".github/workflows/") and path.endswith((".yml", ".yaml"))) or (
        path.startswith(".github/actions/") and path.endswith(("/action.yml", "/action.yaml")))
