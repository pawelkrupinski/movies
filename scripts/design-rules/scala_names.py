"""THE SCALA EXTRACTOR of the general rule core's NAMING and MIXED rules: a production Scala source's top-level types
(and how many public defs each holds), their members' names, and every def's own effects and decisions (a def's body is
its lines to the next line indented as little as it; a nested def's lines are its own), read from the text with
comments and string literals blanked. The Scala coupling, data and quality measures have their own readers
(project/Coupling.scala from the class files, the `quality` module from scalameta)."""
from __future__ import annotations

import os
import re
from typing import Dict, List, Tuple

from model import Function, Module, Name

MODULES = ("geodata", "base", "http", "common", "identity", "scrapers", "worker", "web")
DECLARATION = re.compile(r"^(?:(?:private|protected)(?:\[\w+\])?\s+)?(?:(?:final|sealed|abstract|case|implicit|open|transparent|infix)\s+)*"
                         r"(class|object|trait|enum)\s+([A-Z]\w*)", re.M)
PACKAGE = re.compile(r"^package\s+([\w.]+)", re.M)
PUBLIC_DEF = re.compile(r"^  (?:(?:override|final|inline)\s+)*def\s+(\w+)", re.M)


# What a def's own text calls that reads or changes the world (model.INJECTED's kinds), by name: the JDK's and the
# drivers' entry points. An injected clock, client or repository called through a parameter is no effect here.
SCALA_EFFECTS: Dict[str, "re.Pattern[str]"] = {
    "clock": re.compile(r"\b(?:Instant|LocalDate|LocalDateTime|ZonedDateTime|OffsetDateTime|LocalTime)\.now\b"
                        r"|\bSystem\.(?:currentTimeMillis|nanoTime)\b|\bClock\.system\w*|\bThread\.sleep\b"),
    "env": re.compile(r"\bSystem\.(?:getenv|getProperty)\b|\bsys\.(?:env|props)\b"),
    "process": re.compile(r"\bnew ProcessBuilder\b|\bsys\.process\b|\bRuntime\.getRuntime\b"),
    "files": re.compile(r"\bFiles\.(?:create\w*|delete\w*|read\w*|write\w*|exists|notExists|is[A-Z]\w*|new\w*|list"
                        r"|walk|move|copy|size|lines|getLastModifiedTime|setLastModifiedTime)\b"
                        r"|\bnew (?:File|FileInputStream|FileOutputStream|FileReader|FileWriter|RandomAccessFile)\("
                        r"|\bSource\.fromFile\b"),
    "storage": re.compile(r"\bMongoClients?\.create\b|\bDriverManager\.\w+|\.(?:insertOne|insertMany|updateOne|updateMany"
                          r"|replaceOne|deleteOne|deleteMany|bulkWrite|countDocuments|findOneAndUpdate|findOneAndReplace"
                          r"|createIndex)\("),
    "network": re.compile(r"\bHttpClient\.new\w*|\bHttpRequest\.newBuilder\b|\bnew URL\(|\.openConnection\("
                          r"|\bnew Socket\("),
}
SCALA_DECISIONS = re.compile(r"\b(?:if|while|for|catch)\b|\bcase\b(?!\s+(?:class|object))|&&|\|\|")
DEF = re.compile(r"^(\s*)(?:(?:private|protected)(?:\[\w+\])?\s+|(?:override|final|inline|implicit|transparent)\s+)*"
                 r"def\s+([\w$]+|`[^`]+`)")
TYPE = re.compile(r"^(\s*)(?:(?:private|protected)(?:\[\w+\])?\s+)?(?:(?:final|sealed|abstract|case|implicit|open)\s+)*"
                  r"(?:class|object|trait|enum)\s+(\w+)")
BLANKED = re.compile(r'"""(?:.|\n)*?"""|"(?:\\.|[^"\\\n])*"|//[^\n]*|/\*(?:.|\n)*?\*/')


def blank(text: str) -> str:
    """Comments and string literals blanked to spaces, every newline kept (a line number still means its line)."""
    return BLANKED.sub(lambda m: re.sub(r"[^\n]", " ", m.group(0)), text)


def ends_at(lines: List[str], start: int, indent: int) -> int:
    """The line after a declaration's body: the next non-blank one indented as little as it, a closing `)` / `]` (a
    multi-line signature's) aside -- a closing `}` at its indent is its own."""
    for i in range(start + 1, len(lines)):
        line = lines[i]
        if not line.strip():
            continue
        depth = len(line) - len(line.lstrip())
        if depth < indent or (depth == indent and not line.lstrip().startswith((")", "]", "}"))):
            return i
        if depth == indent and line.lstrip().startswith("}"):
            return i + 1
    return len(lines)


def spans(lines: List[str]) -> List[Tuple[str, str, int, int]]:
    """(kind, name, first line, end) of every def and type declaration, in order."""
    out: List[Tuple[str, str, int, int]] = []
    for i, line in enumerate(lines):
        for kind, pattern in (("def", DEF), ("type", TYPE)):
            m = pattern.match(line)
            if m:
                out.append((kind, m.group(2).strip("`"), i, ends_at(lines, i, len(m.group(1)))))
                break
    return out


def functions(text: str) -> Tuple[Function, ...]:
    """Every def, qualified by the types and defs holding it: its own lines' effects and decisions."""
    lines = blank(text).split("\n")
    found = spans(lines)
    out: List[Function] = []
    for index, (kind, name, start, end) in enumerate(found):
        if kind != "def":
            continue
        holders = [n for _k, n, s, e in found[:index] if s < start < e]
        nested = [(s, e) for k, _n, s, e in found[index + 1:] if start < s < end]
        own = "\n".join(line for i, line in enumerate(lines[start:end], start) if not any(s <= i < e for s, e in nested))
        effects = {k: len(p.findall(own)) for k, p in SCALA_EFFECTS.items() if p.search(own)}
        out.append(Function(".".join([*holders, name]), start + 1, effects, frozenset(),
                            1 + len(SCALA_DECISIONS.findall(own))))
    return tuple(out)


def production(path: str) -> bool:
    return path.endswith(".scala") and path.split("/", 1)[0] in MODULES and "/src/main/" in path


def sources(root: str) -> List[str]:
    out: List[str] = []
    for module in MODULES:
        for d, _, files in os.walk(os.path.join(root, module, "src", "main")):
            out += [os.path.relpath(os.path.join(d, f), root) for f in files if f.endswith(".scala")]
    return sorted(out)


def top_level(text: str) -> List[Tuple[str, str, int]]:
    """(name, body, line) of each top-level type: its declaration at the start of a line, its body to the next one."""
    found = list(DECLARATION.finditer(text))
    return [(m.group(2), text[m.end():found[i + 1].start() if i + 1 < len(found) else len(text)],
             text.count("\n", 0, m.start()) + 1) for i, m in enumerate(found)]


def package_of(text: str, path: str) -> str:
    parts = PACKAGE.findall(text)
    return ".".join(parts) if parts else os.path.dirname(path).split("/src/main/scala/")[-1].replace("/", ".")


def module(path: str, text: str, new: bool = False) -> Module:
    names: List[Name] = []
    for name, body, line in top_level(text):
        members = PUBLIC_DEF.findall(body)
        names.append(Name("type", name, len(members), line))
        names += [Name("function", d, 0, line) for d in sorted(set(members))]
    return Module(language="scala", path=path, package=package_of(text, path), names=tuple(names), new=new,
                  functions=functions(text))
