"""THE SHELL MEASURE (the rule core's; scripts/design-rules/shell_gate.py gates it): how complex one shell script is,
read from its text -- bash is not ratcheted like the other languages but MEASURED, to say when a script must become
Python (the owner, 2026-11-28: "bash gets migrated to python whenever it gets complex enough").

A script's bodies are its functions (`name() {` / `function name {`, to the `}` closing it at the same indent) and its
TOP-LEVEL code (every line outside a function, `<top>`). Per body:

  decisions   each `if` / `elif` / `while` / `until` / `for` / `select` at a command's position, each `case` arm
              (`;;`, `;&`, `;;&`) and each `&&` / `||`: the cyclomatic count less one
  nesting     the deepest nest of `if` / loop / `case` blocks (depth 4 costs a cognitive 1+2+3+4 = 10: the limit)
  lines       a function's code lines (top-level code is counted only in its file's lines)

and per file its code lines: every line but blank ones and comment-only ones, a here-document's body included. Quoted
text and comments are blanked before the keywords are read, so `echo "if done"` decides nothing."""
from __future__ import annotations

import re
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple

TOP = "<top>"
FUNCTION = re.compile(r"^(\s*)(?:function\s+([A-Za-z_][\w:.-]*)\s*(?:\(\))?|([A-Za-z_][\w:.-]*)\s*\(\))\s*[{(]?")
HEREDOC = re.compile(r"(?<!<)<<(?!<)(-?)\s*\\?(['\"]?)([A-Za-z_][\w]*)\2")
KEYWORD = re.compile(r"(?<![\w$./-])(if|elif|while|until|for|select|case|fi|done|esac)(?![\w./=-])")
COMMAND_END = re.compile(r"(^|[;&|(){!]|\bthen|\bdo|\belse|\btime)\s*$")
ARM_END = re.compile(r";;&?|;&")
LOGIC = re.compile(r"&&|\|\|")
OPENERS = {"if", "while", "until", "for", "select", "case"}
CLOSERS = {"fi", "done", "esac"}
DECIDING = {"if", "elif", "while", "until", "for", "select"}


@dataclass
class Body:
    name: str
    lines: int = 0
    decisions: int = 0
    nesting: int = 0
    depth: int = field(default=0, repr=False)


@dataclass(frozen=True)
class Line:
    number: int
    code: str      # the line with its quoted text and comment blanked (a here-document's body: "")
    counted: bool  # a code line: not blank, not a comment alone


class Blanker:
    """Blanks quoted text and comments, line by line, a quote left open carried to the next line."""

    def __init__(self) -> None:
        self.quote = ""

    def blank(self, line: str) -> str:
        out: List[str] = []
        i = 0
        while i < len(line):
            i, kept = self.step(line, i)
            out.append(kept)
        return "".join(out)

    def step(self, line: str, i: int) -> Tuple[int, str]:
        """The character at `i` as kept (quoted text as `S`), and where the next one starts."""
        ch = line[i]
        if self.quote:
            return self.quoted(line, i)
        if ch == "\\":
            return i + 2, "  "
        if ch in "'\"":
            self.quote = ch
            return i + 1, "S"
        if ch == "#" and (i == 0 or line[i - 1] in " \t;"):
            return len(line), ""
        return i + 1, ch

    def quoted(self, line: str, i: int) -> Tuple[int, str]:
        ch = line[i]
        if ch == "\\" and self.quote == '"':
            return i + 2, ""
        if ch == self.quote:
            self.quote = ""
        return i + 1, ""


def heredoc_end(code: str, raw: str) -> Optional[Tuple[str, bool]]:
    """The word ending a here-document this line opens (`<<` outside quotes; the word read from the raw line, where a
    quoted `'EOF'` still reads), and whether its end may be indented by tabs (`<<-`)."""
    found = HEREDOC.search(raw) if HEREDOC.search(code.replace("<<S", "<<x")) else None
    return (found.group(3), found.group(1) == "-") if found else None


def ends(raw: str, ending: Tuple[str, bool]) -> bool:
    """Whether the raw line is the here-document's end word (tab-indented for `<<-`)."""
    return (raw.lstrip("\t") if ending[1] else raw) == ending[0]


class Splitter:
    """A script's lines read in order: a here-document's body taken as counted text, every other line blanked."""

    def __init__(self) -> None:
        self.blanker = Blanker()
        self.ending: Optional[Tuple[str, bool]] = None

    def line(self, number: int, raw: str) -> Line:
        if self.ending is not None:
            self.ending = None if ends(raw, self.ending) else self.ending
            return Line(number, "", True)
        code = self.blanker.blank(raw)
        self.ending = heredoc_end(code, raw) if not self.blanker.quote else None
        return Line(number, code, bool(code.strip()))


def lines_of(text: str) -> List[Line]:
    splitter = Splitter()
    return [splitter.line(number, raw) for number, raw in enumerate(text.splitlines(), 1)]


def at_command(code: str, start: int) -> bool:
    return bool(COMMAND_END.search(code[:start]))


def read_line(body: Body, code: str) -> None:
    """One line's decisions and nesting, added to its body."""
    for found in KEYWORD.finditer(code):
        word = found.group(1)
        if not at_command(code, found.start()):
            continue
        body.decisions += word in DECIDING
        body.depth += (word in OPENERS) - (word in CLOSERS)
        body.nesting = max(body.nesting, body.depth)
    body.decisions += len(ARM_END.findall(code)) + len(LOGIC.findall(code))


def function_start(code: str) -> Optional[Tuple[str, int, str]]:
    """A function's name, its indent and the rest of its opening line, when the line opens one."""
    found = FUNCTION.match(code)
    if not found or "=" in code[:found.end()]:
        return None
    return found.group(2) or found.group(3), len(found.group(1)), code[found.end():]


def closes(code: str, indent: int) -> bool:
    stripped = code.lstrip()
    return stripped.startswith(("}", ")")) and len(code) - len(stripped) <= indent


class Reader:
    """Splits a script's lines into its bodies: a function from its opening line to its closing one, the rest top-level."""

    def __init__(self) -> None:
        self.top = Body(TOP)
        self.functions: List[Body] = []
        self.current: Optional[Body] = None
        self.indent = 0

    def read(self, line: Line) -> None:
        if self.current is None:
            self.outside(line)
        else:
            self.inside(self.current, line)

    def outside(self, line: Line) -> None:
        start = function_start(line.code)
        if start is None:
            read_line(self.top, line.code)
            return
        name, self.indent, opened = start
        self.current = Body(name, lines=1)
        self.functions.append(self.current)
        read_line(self.current, opened)
        if opened.rstrip().endswith(("}", ")")):  # a one-line function
            self.current = None

    def inside(self, body: Body, line: Line) -> None:
        body.lines += line.counted
        read_line(body, line.code)
        if closes(line.code, self.indent):
            self.current = None


def bodies(text: str) -> Tuple[int, List[Body]]:
    """The script's code lines, and its bodies: the top level first, then each function in order."""
    reader = Reader()
    lines = lines_of(text)
    for line in lines:
        reader.read(line)
    return sum(line.counted for line in lines), [reader.top] + reader.functions


def keyed(path: str, found: List[Body]) -> Dict[str, Body]:
    """Each body under `<path>:<name>`, a repeated name `#2`, `#3`... (as the Python measure keys its functions)."""
    seen: Dict[str, int] = {}
    out: Dict[str, Body] = {}
    for body in found:
        seen[body.name] = seen.get(body.name, 0) + 1
        out[f"{path}:{body.name}" + (f"#{seen[body.name]}" if seen[body.name] > 1 else "")] = body
    return out


def rows(path: str, text: str) -> List[Tuple[str, str, int]]:
    """Every measured row of the script: `shell-lines <path>`, and per body `shell-decisions` / `shell-nesting`
    `<path>:<name>`, per function `shell-function-lines`."""
    lines, found = bodies(text)
    out: List[Tuple[str, str, int]] = [("shell-lines", path, lines)]
    for key, body in keyed(path, found).items():
        out += [("shell-decisions", key, body.decisions), ("shell-nesting", key, body.nesting)]
        if body.name != TOP:
            out.append(("shell-function-lines", key, body.lines))
    return out
