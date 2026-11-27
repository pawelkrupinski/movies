"""FILES, INJECTED (scripts/lib; CLAUDE.md, "Design rules": files are an injected effect since 2026-11-27): a tool
reads and writes files through the `Files` its composition root hands it -- `Files.here()` over the real disk,
`Files.in_memory()` in a unit test -- never `open` / `os.*` itself. `Files` holds every rule (a path taken against its
base and normalised, text as UTF-8, a missing file told apart, a listing sorted, a replacement whole or not at all, a
write's directories made); a `Disk` is only the boundary, bytes at absolute paths: `LocalDisk` (atomic.py for a
replacement) or `InMemoryDisk` (a dict), held alike by tool_files_test.py's contract suite.

  files.read_text(path) -> str                    FileNotFoundError when there is none
  files.read_text_or(path, default) -> str        `default` when there is none
  files.write_text(path, text)                    its directories made
  files.copy_script(source, target)               a text file copied whole, executable if it starts with `#!`
  files.replace_text(path, text, mode=None)       atomic: a reader sees the old file or the whole new one
  with files.replacing(path) as out: out.write()  the same, written piece by piece; a block that raises leaves `path` be
  files.exists(path) / files.is_file(path) / files.is_dir(path)
  files.list(directory, suffix="") -> [name]      sorted; FileNotFoundError when there is no such directory
  files.under(directory) -> [path]                every file below it, sorted (`os.walk`)
  files.remove(path, missing_ok=True)
  files.mtime(path) -> float                      seconds since the epoch; FileNotFoundError when there is none
  files.make_directories(path)                    the directory and those above it
  files.rename(source, target) -> bool            renamed whole (a directory too); False when it could not be (onto a
                                                  filled directory: rename(2))
  files.path(*parts) -> str                       the absolute path the parts name, against the base

Real-filesystem INTEGRATION tests stay where the filesystem IS the behaviour (atomic.py's temporaries and their races,
lock files, the CLI black-box harness): this seam is for a tool's own reads and writes."""
from __future__ import annotations

import io
import os
from types import TracebackType
from typing import Callable, Dict, Iterator, List, Optional, Protocol, Set

import atomic


class Disk(Protocol):
    """The boundary: bytes at absolute, normalised paths. A file's directory exists before it is written."""

    def read(self, path: str) -> bytes: ...                                # FileNotFoundError

    def write(self, path: str, data: bytes) -> None: ...

    def replace(self, path: str, data: bytes, mode: Optional[int]) -> None: ...  # whole or not at all

    def make_directories(self, path: str) -> None: ...

    def kind(self, path: str) -> Optional[str]: ...                        # "file", "directory" or None

    def names(self, directory: str) -> List[str]: ...                      # FileNotFoundError; any order

    def remove(self, path: str) -> None: ...                               # FileNotFoundError

    def mtime(self, path: str) -> float: ...                               # FileNotFoundError

    def rename(self, source: str, target: str) -> None: ...                # OSError; never onto a filled directory


class LocalDisk:
    """The real disk."""

    def read(self, path: str) -> bytes:
        with open(path, "rb") as f:
            return f.read()

    def write(self, path: str, data: bytes) -> None:
        with open(path, "wb") as f:
            f.write(data)

    def replace(self, path: str, data: bytes, mode: Optional[int]) -> None:
        atomic.write_bytes(path, data, mode)

    def make_directories(self, path: str) -> None:
        os.makedirs(path, exist_ok=True)

    def kind(self, path: str) -> Optional[str]:
        return "file" if os.path.isfile(path) else "directory" if os.path.isdir(path) else None

    def names(self, directory: str) -> List[str]:
        return os.listdir(directory)

    def remove(self, path: str) -> None:
        os.remove(path)

    def mtime(self, path: str) -> float:
        return os.path.getmtime(path)

    def rename(self, source: str, target: str) -> None:
        os.rename(source, target)


class InMemoryDisk:
    """A dict of files and a set of directories; a write stamped by the injected clock (`now`)."""

    def __init__(self, now: Callable[[], float] = lambda: 0.0) -> None:
        self.now = now
        self.files: Dict[str, bytes] = {}
        self.stamps: Dict[str, float] = {}
        self.directories: Set[str] = {os.sep}

    def read(self, path: str) -> bytes:
        if path not in self.files:
            raise FileNotFoundError(path)
        return self.files[path]

    def write(self, path: str, data: bytes) -> None:
        if os.path.dirname(path) not in self.directories:
            raise FileNotFoundError(path)
        if path in self.directories:
            raise IsADirectoryError(path)
        self.files[path] = bytes(data)
        self.stamps[path] = self.now()

    def replace(self, path: str, data: bytes, mode: Optional[int]) -> None:
        self.write(path, data)

    def make_directories(self, path: str) -> None:
        while path not in self.directories:
            if path in self.files:
                raise FileExistsError(path)
            self.directories.add(path)
            path = os.path.dirname(path)

    def kind(self, path: str) -> Optional[str]:
        return "file" if path in self.files else "directory" if path in self.directories else None

    def names(self, directory: str) -> List[str]:
        if directory not in self.directories:
            raise FileNotFoundError(directory)
        inside = [p for p in [*self.files, *self.directories] if p != directory and os.path.dirname(p) == directory]
        return [os.path.basename(p) for p in inside]

    def remove(self, path: str) -> None:
        if path not in self.files:
            raise FileNotFoundError(path)
        del self.files[path], self.stamps[path]

    def mtime(self, path: str) -> float:
        if path not in self.stamps:
            raise FileNotFoundError(path)
        return self.stamps[path]

    def rename(self, source: str, target: str) -> None:
        if self.kind(source) is None or os.path.dirname(target) not in self.directories:
            raise FileNotFoundError(source)
        if target in self.files or any(os.path.dirname(p) == target for p in [*self.files, *self.directories]):
            raise OSError(f"{target} is not empty")
        self.files = {moved(p, source, target): data for p, data in self.files.items()}
        self.stamps = {moved(p, source, target): when for p, when in self.stamps.items()}
        self.directories = {moved(p, source, target) for p in self.directories}


def moved(path: str, source: str, target: str) -> str:
    """`path` after `source` is renamed to `target`."""
    return target + path[len(source):] if path == source or path.startswith(source + os.sep) else path


class Replacing:
    """`path`'s new text, gathered and handed to the disk whole when the block ends well; nothing when it raises."""

    def __init__(self, files: "Files", path: str, mode: Optional[int]) -> None:
        self.files, self.path, self.mode = files, path, mode
        self.out = io.StringIO()

    def __enter__(self) -> io.StringIO:
        return self.out

    def __exit__(self, kind: Optional[type], _value: Optional[BaseException], _trace: Optional[TracebackType]) -> None:
        if kind is None:
            self.files.replace_text(self.path, self.out.getvalue(), self.mode)


class Files:
    """Every rule of a tool's file reads and writes, over a `Disk`; paths taken against `base`."""

    def __init__(self, disk: Disk, base: str) -> None:
        self.disk = disk
        self.base = os.path.normpath(os.path.join(os.sep, base))

    @classmethod
    def here(cls) -> "Files":
        return cls(LocalDisk(), os.getcwd())

    @classmethod
    def in_memory(cls, base: str = "/work", now: Callable[[], float] = lambda: 0.0) -> "Files":
        return cls(InMemoryDisk(now), base)

    def path(self, *parts: str) -> str:
        return os.path.normpath(os.path.join(self.base, *parts))

    def read_text(self, path: str) -> str:
        return self.disk.read(self.path(path)).decode("utf-8")

    def read_text_or(self, path: str, default: str) -> str:
        try:
            return self.read_text(path)
        except FileNotFoundError:
            return default

    def write_text(self, path: str, text: str) -> None:
        full = self.path(path)
        self.disk.make_directories(os.path.dirname(full))
        self.disk.write(full, text.encode("utf-8"))

    def replace_text(self, path: str, text: str, mode: Optional[int] = None) -> None:
        full = self.path(path)
        self.disk.make_directories(os.path.dirname(full))
        self.disk.replace(full, text.encode("utf-8"), mode)

    def copy_script(self, source: str, target: str) -> None:
        """`source`'s text whole into `target` (its directories made): executable when it starts with a shebang."""
        text = self.read_text(source)
        self.replace_text(target, text, 0o755 if text.startswith("#!") else 0o644)

    def replacing(self, path: str, mode: Optional[int] = None) -> Replacing:
        return Replacing(self, path, mode)

    def exists(self, path: str) -> bool:
        return self.disk.kind(self.path(path)) is not None

    def is_file(self, path: str) -> bool:
        return self.disk.kind(self.path(path)) == "file"

    def is_dir(self, path: str) -> bool:
        return self.disk.kind(self.path(path)) == "directory"

    def list(self, directory: str, suffix: str = "") -> List[str]:
        return sorted(n for n in self.disk.names(self.path(directory)) if n.endswith(suffix))

    def under(self, directory: str) -> List[str]:
        return sorted(self.walk(self.path(directory)))

    def walk(self, directory: str) -> Iterator[str]:
        for name in self.disk.names(directory):
            full = os.path.join(directory, name)
            if self.disk.kind(full) == "directory":
                yield from self.walk(full)
            else:
                yield full

    def remove(self, path: str, missing_ok: bool = True) -> None:
        try:
            self.disk.remove(self.path(path))
        except FileNotFoundError:
            if not missing_ok:
                raise

    def mtime(self, path: str) -> float:
        return self.disk.mtime(self.path(path))

    def make_directories(self, path: str) -> None:
        self.disk.make_directories(self.path(path))

    def rename(self, source: str, target: str) -> bool:
        try:
            self.disk.rename(self.path(source), self.path(target))
        except OSError:
            return False
        return True
