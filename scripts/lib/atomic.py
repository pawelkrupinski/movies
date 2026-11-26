"""ATOMIC FILE WRITES, the one way the Python tools replace a file (scripts/lib): the new content goes into a uniquely
named temporary file in the SAME directory (a rename is atomic only within one filesystem; a fixed `path + ".tmp"` is
one name two concurrent writers share, each renaming the other's half-written file), then is renamed over `path` --
a reader sees the old file or the whole new one, never a torn one, and a failed write leaves `path` as it was and no
temporary behind.

  with replacing(path, mode=None) as out: out.write(...)   `out` a text file; `mode` the permissions the result gets
  write_text(path, text, mode=None)                         the text, whole
  write_json(path, value, **dumps_options)                  the value as JSON"""
from __future__ import annotations

import contextlib
import json
import os
import tempfile
from types import TracebackType
from typing import Any, Optional, TextIO


class Replacement:
    """The file `path` being written: a text file the writer fills, renamed over `path` when its block ends well."""

    def __init__(self, path: str, mode: Optional[int]) -> None:
        self.path, self.mode = path, mode
        directory = os.path.dirname(os.path.abspath(path))
        os.makedirs(directory, exist_ok=True)
        descriptor, self.temporary = tempfile.mkstemp(dir=directory, prefix=f".{os.path.basename(path)}.", suffix=".tmp")
        self.out = os.fdopen(descriptor, "w", encoding="utf-8")

    def __enter__(self) -> TextIO:
        return self.out

    def __exit__(self, kind: Optional[type], _value: Optional[BaseException], _trace: Optional[TracebackType]) -> None:
        self.out.close()
        if kind is not None:
            with contextlib.suppress(FileNotFoundError):
                os.unlink(self.temporary)
            return
        if self.mode is not None:
            os.chmod(self.temporary, self.mode)
        os.replace(self.temporary, self.path)


def replacing(path: str, mode: Optional[int] = None) -> Replacement:
    return Replacement(path, mode)


def write_text(path: str, text: str, mode: Optional[int] = None) -> None:
    with replacing(path, mode) as out:
        out.write(text)


def write_json(path: str, value: Any, **dumps_options: Any) -> None:
    with replacing(path) as out:
        json.dump(value, out, **dumps_options)
