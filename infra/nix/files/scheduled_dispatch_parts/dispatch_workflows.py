"""The workflows' own `on.schedule` crons: reading them, the generated schedule file, and the drift check.

A part of nix/files/scheduled_dispatch.py, which carries the argument for all of it (the header there).
"""
from __future__ import annotations
import glob
import json
import os
import re
from functools import partial
from typing import Callable, Dict, List, Mapping, Optional, Sequence, TypedDict
from dispatch_cron import ScheduledWorkflow, is_daily_or_rarer, on_calendar, staggering


EVENT_TYPE_PREFIX = "scheduled-"

JSON_PATH = "infra/nix/files/scheduled-workflows.json"

WORKFLOWS_DIR = ".github/workflows"

SCHEDULE_EQ = "github.event_name == 'schedule'"

SCHEDULE_NE = "github.event_name != 'schedule'"

DISPATCH_EQ = "(github.event_name == 'schedule' || github.event_name == 'repository_dispatch')"

DISPATCH_NE = "(github.event_name != 'schedule' && github.event_name != 'repository_dispatch')"


# ---------------------------------------------------------------- the workflows (no PyYAML: macOS's python3 has none)

def _indent(line: str) -> int:
    return len(line) - len(line.lstrip(" "))


def _take_until(lines: Sequence[str], ends: Callable[[str], bool]) -> List[str]:
    """The lines before the first one that `ends` the block."""
    block: List[str] = []
    for line in lines:
        if ends(line):
            break
        block.append(line)
    return block


def _leaves_on_block(line: str) -> bool:
    return bool(line.strip()) and not line.startswith((" ", "#"))


def _on_block(text: str) -> List[str]:
    """The lines of the workflow's top-level `on:` block."""
    lines = text.splitlines()
    for i, line in enumerate(lines):
        if re.match(r"^(on|\"on\"|'on'):\s*(#.*)?$", line):
            return _take_until(lines[i + 1:], _leaves_on_block)
    return []


def _dedents_to(depth: int, line: str) -> bool:
    return bool(line.strip()) and not line.strip().startswith("#") and _indent(line) <= depth


def _sub_block(block: Sequence[str], key: str) -> Optional[List[str]]:
    """The lines under `key:` within an `on:` block, or None when the key is absent."""
    for i, line in enumerate(block):
        if line.strip().split("#")[0].strip() == f"{key}:":
            return _take_until(block[i + 1:], partial(_dedents_to, _indent(line)))
    return None


def workflow_crons(text: str) -> List[str]:
    schedule = _sub_block(_on_block(text), "schedule") or []
    return [m.group(2) for m in (re.match(r"\s*-\s*cron:\s*(['\"])(.+?)\1", line) for line in schedule) if m]


def dispatch_types(text: str) -> List[str]:
    """The repository_dispatch types a workflow listens to ([] when it does not)."""
    block = _sub_block(_on_block(text), "repository_dispatch")
    if block is None:
        return []
    for line in block:
        m = re.match(r"\s*types:\s*\[(.*)\]", line)
        if m:
            return [t.strip().strip("'\"") for t in m.group(1).split(",") if t.strip()]
    return []


def _read(path: str) -> str:
    with open(path) as f:
        return f.read()


def read_workflows(root: str) -> Dict[str, str]:
    folder = os.path.join(root, WORKFLOWS_DIR)
    paths = sorted(glob.glob(os.path.join(folder, "*.yml")) + glob.glob(os.path.join(folder, "*.yaml")))
    return {os.path.basename(path): _read(path) for path in paths}


def stem(file_name: str) -> str:
    return file_name.rsplit(".", 1)[0]


class Document(TypedDict):
    generatedBy: str
    workflows: Dict[str, ScheduledWorkflow]
    excluded: Dict[str, str]


def generate(workflows: Mapping[str, str]) -> Document:
    covered: Dict[str, ScheduledWorkflow] = {}
    excluded: Dict[str, str] = {}
    for name, text in sorted(workflows.items()):
        crons = workflow_crons(text)
        if not crons:
            continue
        if all(is_daily_or_rarer(c) for c in crons):
            covered[stem(name)] = {"file": name, "crons": crons,
                                   "onCalendar": [e for c in crons for e in on_calendar(c)]}
        else:
            excluded[name] = "fires more than once a day: its next slot recovers a dropped one"
    return {"generatedBy": "infra/nix/files/scheduled_dispatch.py generate (do not edit by hand)",
            "workflows": covered, "excluded": excluded}


def render(document: Document) -> str:
    return json.dumps(document, indent=2, sort_keys=True) + "\n"


def _wiring_problems(name: str, text: str, covered: bool) -> List[str]:
    """What is wrong with one workflow's dispatch wiring: a covered one listens to exactly its own event and reads its
    scheduled-run conditions as `schedule || repository_dispatch`; an uncovered one listens to no scheduler event."""
    scheduled_types = [t for t in dispatch_types(text) if t.startswith(EVENT_TYPE_PREFIX)]
    if not covered:
        return ([f"{name}: listens to {scheduled_types} but is not covered by the external scheduler"]
                if scheduled_types else [])
    found: List[str] = []
    want = EVENT_TYPE_PREFIX + stem(name)
    if scheduled_types != [want]:
        found.append(f"{name}: `on: repository_dispatch: types:` must list exactly [{want}] "
                     f"(the external scheduler's event), not {scheduled_types}")
    bare = text.replace(DISPATCH_EQ, "").replace(DISPATCH_NE, "")
    found += [f"{name}: `{needle}` misses the external scheduler's runs: write `{fix}`"
              for needle, fix in ((SCHEDULE_EQ, DISPATCH_EQ), (SCHEDULE_NE, DISPATCH_NE)) if needle in bare]
    if "github.event.schedule" in text:
        found.append(f"{name}: reads github.event.schedule, which a repository_dispatch run does not carry")
    return found


def drift(workflows: Mapping[str, str], current: str) -> List[str]:
    """Everything that has drifted: the generated file (`current`), and each workflow's dispatch wiring and conditions."""
    document = generate(workflows)
    found: List[str] = []
    if current != render(document):
        found.append(f"{JSON_PATH} is not what the workflows' crons generate: "
                     "run `python3 infra/nix/files/scheduled_dispatch.py generate`")
    found += staggering(document["workflows"])
    covered = {w["file"] for w in document["workflows"].values()}
    for name, text in sorted(workflows.items()):
        found += _wiring_problems(name, text, name in covered)
    return found


def _read_or_empty(path: str) -> str:
    try:
        return _read(path)
    except OSError:
        return ""


def problems(root: str) -> List[str]:
    return drift(read_workflows(root), _read_or_empty(os.path.join(root, JSON_PATH)))
