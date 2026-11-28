"""THE TOOLING CLOSURE (scripts/lib; the same import graph as sandbox_closure.py): the files a tool's cache and verdict keys
are hashed over -- identity-what-if's published verdicts, identity-measure's result cache -- read from the tool's own `import`
lines instead of listed by hand, so splitting a tool into modules needs no edit to a file list.

  own_files(files, root, entries, extra=()) -> [relative path]
      the modules of `scripts/` the entry points import, transitively (the tool's OWN code: the shared `scripts/lib`
      adapters and `scripts/ci` modules keep their own change history and are not the tool's), with `extra` -- the
      files no import reaches (the shell entry, the shell library, a workflow) -- sorted, so the order is the names' own.

The key was versioned once when the lists went: a hash over the closure covers the sorted set where the old one covered a
hand-ordered list, so every key differs from before this change, and a cached verdict or result of the earlier tooling is
simply not found again."""
from __future__ import annotations

import os
from typing import Iterable, List

from harness import sandbox_closure
from seams import tool_files

OWN_DIRECTORY = "scripts"


def own_files(files: tool_files.Files, root: str, entries: Iterable[str], extra: Iterable[str] = ()) -> List[str]:
    """The tool's own modules in the checkout at `root` (see the module's header), as paths from the repository root."""
    reached = sandbox_closure.closure(lambda p: files.read_text(f"{root}/{p}"), lambda p: files.is_file(f"{root}/{p}"), entries)
    return sorted({*(p for p in reached if os.path.dirname(p) == OWN_DIRECTORY), *extra})
