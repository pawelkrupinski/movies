#!/usr/bin/env python3
"""Every call of a reusable workflow (`uses: ./.github/workflows/<file>.yml`) hands it inputs its `on.workflow_call`
declares: no input the callee lacks, every required one without a default, and no string where it takes a number or a
boolean. GitHub answers a call that breaks any of these by dropping it -- the caller's job never appears, the run
fails with no failed job and no annotation (Identity takes #1, run 42515079814: the rows' "55" and "true" strings).

Usage: reusable_workflow_inputs.py [workflow dir]   (default: this repository's .github/workflows) -- prints each
problem, exits 1 on any. A value that is one whole `${{ }}` expression is typed by GitHub when it runs and passes, unless
the expression is a quoted literal; a literal is typed as YAML types it."""

import glob
import os
import re
import sys

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
LOCAL_CALL = re.compile(r"^\./\.github/workflows/([\w.-]+\.ya?ml)$")
WHOLE_EXPRESSION = re.compile(r"^\$\{\{(.*)}}$", re.S)
QUOTED = re.compile(r"""^\s*(?:'(?:[^']|'')*'|"[^"]*")\s*$""")
NUMBER = re.compile(r"^[-+]?(\d+(\.\d*)?|\.\d+)([eE][-+]?\d+)?$")


def read(path):
    with open(path) as f:
        return f.read()


def strip_comment(line):
    """`line` without a trailing `# comment` (one outside quotes and `${{ }}`), and a whole-comment line as empty."""
    quote, depth, i = None, 0, 0
    while i < len(line):
        c = line[i]
        if quote:
            if c == quote:
                quote = None
        elif line.startswith("${{", i):
            depth += 1
            i += 3
            continue
        elif depth and line.startswith("}}", i):
            depth -= 1
            i += 2
            continue
        elif c in "'\"" and not depth:
            quote = c
        elif c == "#" and not depth and (i == 0 or line[i - 1] in " \t"):
            return line[:i].rstrip()
        i += 1
    return line.rstrip()


def mapping(lines):
    """A block mapping's entries at its first line's indent, as {key: (inline value, the lines nested under it)}."""
    lines = [strip_comment(l) for l in lines]
    lines = [l for l in lines if l.strip()]
    if not lines:
        return {}
    indent = len(lines[0]) - len(lines[0].lstrip())
    out, key = {}, None
    for line in lines:
        here = len(line) - len(line.lstrip())
        if here == indent and not line.lstrip().startswith("- "):
            k, _, v = line.strip().partition(":")
            key = k.strip().strip("'\"")
            out[key] = (v.strip(), [])
        elif here > indent and key is not None:
            out[key][1].append(line)
    return out


def scalar(value):
    """A literal as YAML types it: ('string'|'number'|'boolean', the text)."""
    if QUOTED.match(value):
        return "string", value.strip()[1:-1]
    if value in ("true", "false", "True", "False", "TRUE", "FALSE"):
        return "boolean", value
    if NUMBER.match(value):
        return "number", value
    return "string", value


def declared_inputs(text):
    """{name: {type, required, default?}} of a workflow's `on.workflow_call.inputs`; None for a workflow not reusable."""
    top = mapping(text.split("\n"))
    on = top.get("on") or top.get("true")  # YAML 1.1 reads a bare `on` as true
    if on is None:
        return None
    call = mapping(on[1]).get("workflow_call")
    if call is None:
        return None
    inputs = mapping(call[1]).get("inputs")
    out = {}
    for name, (_, body) in mapping(inputs[1] if inputs else []).items():
        fields = {k: v for k, (v, _) in mapping(body).items()}
        out[name] = {"type": fields.get("type", "string"), "required": fields.get("required") == "true",
                     "default": "default" in fields}
    return out


def calls(text):
    """(job, callee file, {input: value}) for each job of `text` that calls a local reusable workflow."""
    jobs = mapping(text.split("\n")).get("jobs")
    for job, (_, body) in mapping(jobs[1] if jobs else []).items():
        keys = mapping(body)
        uses = LOCAL_CALL.match(keys.get("uses", ("", []))[0].strip("'\""))
        if not uses:
            continue
        with_ = keys.get("with")
        given = {k: (v if v not in ("|", ">", "|-", ">-") else "\n".join(l.strip() for l in nested))
                 for k, (v, nested) in mapping(with_[1] if with_ else []).items()}
        yield job, uses.group(1), given


def value_problem(kind, value):
    """Why `value` cannot be handed to an input of type `kind`, or None."""
    if kind not in ("number", "boolean"):
        return None
    whole = WHOLE_EXPRESSION.match(value.strip())
    if whole:
        return f"a quoted literal {whole.group(1).strip()} to a {kind} input" if QUOTED.match(whole.group(1)) else None
    typed, _ = scalar(value.strip())
    return None if typed == kind else f"{value.strip()!r} (a {typed}) to a {kind} input"


MATRIX_REFERENCE = re.compile(r"^\$\{\{\s*matrix\.([\w-]+)\s*}}$")
PYTHON_TYPES = {"number": (int, float), "boolean": (bool,)}


def matrix_problems(given, declared, rows):
    """A planned matrix's rows (a preflight's JSON, typed only when it runs) against the inputs: each `${{ matrix.k }}`
    handed to a number or boolean input must be one in every row -- a JSON "55" or "true" is a string."""
    found = []
    for name, value in sorted(given.items()):
        ref, kind = MATRIX_REFERENCE.match(value.strip()), declared.get(name, {}).get("type")
        if not ref or kind not in PYTHON_TYPES:
            continue
        for row in rows:
            got = row.get(ref.group(1))
            if not isinstance(got, PYTHON_TYPES[kind]) or (kind == "number" and isinstance(got, bool)):
                found.append(f"row {row.get('code', row)}: `{name}` takes matrix.{ref.group(1)} = {got!r}, not a {kind}")
    return found


def problems(workflow_dir):
    texts = {os.path.basename(p): read(p) for p in sorted(glob.glob(os.path.join(workflow_dir, "*.y*ml")))}
    found = []
    for caller, text in texts.items():
        for job, callee, given in calls(text):
            where = f"{caller}: job {job} calls {callee}"
            if callee not in texts:
                found.append(f"{where}, which does not exist")
                continue
            declared = declared_inputs(texts[callee])
            if declared is None:
                found.append(f"{where}, which declares no `on: workflow_call`")
                continue
            for name in sorted(set(given) - set(declared)):
                found.append(f"{where} with `{name}`, an input it does not declare")
            for name, spec in sorted(declared.items()):
                if spec["required"] and not spec["default"] and name not in given:
                    found.append(f"{where} without `{name}`, which it requires")
                elif name in given and (why := value_problem(spec["type"], given[name])):
                    found.append(f"{where} passing `{name}` {why}")
    return found


def main(argv):
    found = problems(argv[0] if argv else os.path.join(ROOT, ".github/workflows"))
    for line in found:
        print(line)
    return 1 if found else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
