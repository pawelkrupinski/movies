"""ATOMIC FILE WRITES, the one way the Python tools replace a file (scripts/lib): the new content goes into a uniquely
named temporary file in the SAME directory (a rename is atomic only within one filesystem; a fixed `path + ".tmp"` is
one name two concurrent writers share, each renaming the other's half-written file), then is renamed over `path` --
a reader sees the old file or the whole new one, never a torn one, and a failed write leaves `path` as it was and no
temporary behind.

  with replacing(path, mode=None) as out: out.write(...)   `out` a text file; `mode` the permissions the result gets
  write_text(path, text, mode=None)                         the text, whole
  write_bytes(path, data, mode=None)                        the bytes, whole (tool_files.LocalDisk's replacement)
  write_json(path, value, **dumps_options)                  the value as JSON"""
from __future__ import annotations

import contextlib
import json
import os
import tempfile
from types import TracebackType
from typing import Any, Optional, TextIO


class Temporary:
    """The uniquely named temporary beside `path` (its directory made): renamed over `path` when the write ended well,
    removed when it did not."""

    def __init__(self, path: str, mode: Optional[int]) -> None:
        self.path, self.mode = path, mode
        directory = os.path.dirname(os.path.abspath(path))
        os.makedirs(directory, exist_ok=True)
        self.descriptor, self.name = tempfile.mkstemp(dir=directory, prefix=f".{os.path.basename(path)}.", suffix=".tmp")

    def finish(self, failed: bool) -> None:
        if failed:
            with contextlib.suppress(FileNotFoundError):
                os.unlink(self.name)
            return
        if self.mode is not None:
            os.chmod(self.name, self.mode)
        os.replace(self.name, self.path)


class Replacement:
    """The file `path` being written: a text file the writer fills, renamed over `path` when its block ends well."""

    def __init__(self, path: str, mode: Optional[int]) -> None:
        self.temporary = Temporary(path, mode)
        self.out = os.fdopen(self.temporary.descriptor, "w", encoding="utf-8")

    def __enter__(self) -> TextIO:
        return self.out

    def __exit__(self, kind: Optional[type], _value: Optional[BaseException], _trace: Optional[TracebackType]) -> None:
        self.out.close()
        self.temporary.finish(failed=kind is not None)


def replacing(path: str, mode: Optional[int] = None) -> Replacement:
    return Replacement(path, mode)


def write_text(path: str, text: str, mode: Optional[int] = None) -> None:
    with replacing(path, mode) as out:
        out.write(text)


def write_bytes(path: str, data: bytes, mode: Optional[int] = None) -> None:
    temporary = Temporary(path, mode)
    try:
        with os.fdopen(temporary.descriptor, "wb") as out:
            out.write(data)
    except BaseException:
        temporary.finish(failed=True)
        raise
    temporary.finish(failed=False)


def write_json(path: str, value: Any, **dumps_options: Any) -> None:
    with replacing(path) as out:
        json.dump(value, out, **dumps_options)
