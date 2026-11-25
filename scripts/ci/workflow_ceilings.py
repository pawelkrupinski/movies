"""The ceilings of a workflow's jobs, read off its file: each job's `timeout-minutes` (a literal one), github.com's
default for a job naming none, and None for a call or an expression (its job echoes it: ci_timings.py `CEILING_LINE`).
Read by indentation, not a YAML parser: the runners' python has no PyYAML.

    from workflow_ceilings import workflow_ceilings, ceiling_of
"""
import os
import re
from typing import Dict, Iterable, List, Optional, Pattern, Sequence, Tuple

Attributes = Dict[str, str]
# One job of a workflow: (name pattern, the length of its literal name, its ceiling in minutes or None).
Ceiling = Tuple[Pattern[str], int, Optional[int]]

# github.com's ceiling for a job that names none.
DEFAULT_TIMEOUT_MINUTES = 360
EXPRESSION = re.compile(r"\$\{\{.*?\}\}")
WORKFLOWS = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", ".github", "workflows")


def indent(line: str) -> int:
    return len(line) - len(line.lstrip(" "))


def scalar(value: str) -> str:
    """A plain or quoted YAML scalar without its trailing comment: `40 # longest green row 31 min` is 40, as YAML reads
    it; a quoted one keeps a ` #` inside its quotes."""
    value = value.strip()
    if value[:1] in ("'", '"') and value.find(value[0], 1) > 0:
        return value[1:value.find(value[0], 1)]
    return re.sub(r"(^|\s)#.*$", "", value).strip()


def job_attributes(block: Sequence[str]) -> Attributes:
    """A job's top-level attributes: the lines one level under its own key, comments and blanks left out."""
    inner = [line for line in block if line.strip() and not line.lstrip().startswith("#")]
    level = indent(inner[0]) if inner else 0
    attrs: Attributes = {}
    for line in inner:
        if indent(line) == level and ":" in line:
            name, _, value = line.strip().partition(":")
            attrs[name] = scalar(value)
    return attrs


def jobs_body(lines: Sequence[str]) -> List[str]:
    """The lines under `jobs:` up to the next top-level key; none when the file has no `jobs:`."""
    if "jobs:" not in lines:
        return []
    body: List[str] = []
    for line in lines[lines.index("jobs:") + 1:]:
        if line.strip() and not line.startswith((" ", "#")):
            break
        body.append(line)
    return body


def is_content(line: str) -> bool:
    return bool(line.strip()) and not line.lstrip().startswith("#")


def workflow_jobs(text: str) -> Dict[str, Tuple[Attributes, str]]:
    """Every job of a workflow file: {key: (its top-level attributes, its whole block)}. Read by indentation, not a
    YAML parser: the runners' python has no PyYAML, and a job's keys are the lines one level under its own."""
    body = jobs_body(text.splitlines())
    content = [line for line in body if is_content(line)]
    if not content:
        return {}
    job_indent = indent(content[0])
    jobs: Dict[str, Tuple[Attributes, str]] = {}
    key: Optional[str] = None
    block: List[str] = []
    for line in body + ["end:"]:
        if is_content(line) and indent(line) <= job_indent:
            if key:
                jobs[key] = (job_attributes(block), "\n".join(block))
            key, block = line.strip().rstrip(":"), []
        else:
            block.append(line)
    return jobs


def name_pattern(key: str, attrs: Attributes) -> Tuple[Pattern[str], int]:
    """The pattern a job's name on github.com matches, and the length of its literal parts."""
    literal = EXPRESSION.split(attrs.get("name") or key)
    if not attrs.get("name") and "strategy" in attrs:
        # A matrix job with no `name:` is `<key> (<row>)` on github.com (plain `<key>` if skipped before it starts).
        return re.compile(re.escape(key) + r"(?: \(.+\))?"), sum(map(len, literal))
    return re.compile(".+".join(re.escape(part) for part in literal)), sum(map(len, literal))


def literal_ceiling(attrs: Attributes) -> Optional[int]:
    """A job's own `timeout-minutes` when a plain number, github.com's default when it names none, else None."""
    timeout = attrs.get("timeout-minutes")
    if timeout is None:
        return DEFAULT_TIMEOUT_MINUTES
    return int(timeout) if timeout.isdigit() else None


def workflow_ceilings(path: str, root: str = WORKFLOWS, seen: Tuple[str, ...] = ()) -> List[Ceiling]:
    """(name pattern, its literal length, ceiling) for each job of a workflow and of the workflows it calls. The
    ceiling is minutes, or None for a call (it runs no steps itself) or an expression (its job echoes it)."""
    file = os.path.join(root, os.path.basename(path))
    if file in seen or not os.path.isfile(file):
        return []
    with open(file, encoding="utf-8") as handle:
        jobs = workflow_jobs(handle.read())
    found: List[Ceiling] = []
    for key, (attrs, _) in jobs.items():
        pattern, length = name_pattern(key, attrs)
        uses = attrs.get("uses", "")
        ceiling: Optional[int] = None
        if uses.startswith("./.github/workflows/"):
            found += workflow_ceilings(uses, root, seen + (file,))
        else:
            ceiling = literal_ceiling(attrs)
        found.append((pattern, length, ceiling))
    return found


def ceiling_of(name: str, ceilings: Iterable[Ceiling]) -> Optional[int]:
    """A job's ceiling in minutes, from the most specific pattern its name matches: `leg (poland)` is the caller row
    `leg (${{ matrix.country }})`, not the callee's `${{ matrix.phase }} (${{ inputs.country }})`. GitHub prefixes a
    called job with its caller's name (`poland / takes (poland)`)."""
    matching = [(length, ceiling) for pattern, length, ceiling in ceilings
                if pattern.fullmatch(name) or pattern.fullmatch(name.split(" / ")[-1])]
    return max(matching, key=lambda match: match[0])[1] if matching else None
