"""THE SCALA EXTRACTOR of the general rule core's NAMING rules: a production Scala source's top-level types (and how
many public defs each holds) and their members' names, read from the text. The Scala coupling, data and quality
measures have their own readers (project/Coupling.scala from the class files, the `quality` module from scalameta);
only the names come from here."""
from __future__ import annotations

import os
import re
from typing import List, Tuple

from model import Module, Name

MODULES = ("geodata", "base", "http", "common", "identity", "scrapers", "worker", "web")
DECLARATION = re.compile(r"^(?:(?:private|protected)(?:\[\w+\])?\s+)?(?:(?:final|sealed|abstract|case|implicit|open|transparent|infix)\s+)*"
                         r"(class|object|trait|enum)\s+([A-Z]\w*)", re.M)
PACKAGE = re.compile(r"^package\s+([\w.]+)", re.M)
PUBLIC_DEF = re.compile(r"^  (?:(?:override|final|inline)\s+)*def\s+(\w+)", re.M)


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
    return Module(language="scala", path=path, package=package_of(text, path), names=tuple(names), new=new)
