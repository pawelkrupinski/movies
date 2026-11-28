"""STATE AND KEY-VALUE STORAGE, INJECTED (scripts/lib; CLAUDE.md, "Design rules": storage is an injected effect since
2026-11-27): the JSON and TSV state files a tool keeps, over the injected tool_files.Files (so a unit test hands in
`Files.in_memory()`), and a key-value store over a `Table` -- `SqliteTable` on disk, `InMemoryTable` in a test -- each
held alike by tool_state_test.py's contract suite. The rules live above the boundary: the encodings, a missing state
told apart from an unreadable one, a key normalised and checked, a value JSON both ways.

  JsonState(files, path).load(default) -> value   `default` when there is none; StateUnreadable when it is no JSON
  JsonState(files, path).save(value)              atomically: keys sorted, two-space indented, a trailing newline
  TsvState(files, path).rows() -> [[cell]]        `#` and blank lines aside; [] when there is none
  TsvState(files, path).save(rows, comment="")    atomically; a tab or newline in a cell a space; `comment` the
                                                  `# ` lines on top
  KeyValue(table).get(key, default=None) / .put(key, value) / .delete(key) -> whether it was there / .keys(prefix="")
  KeyValue.sqlite(path, name="kv") / KeyValue.in_memory()

A key is NFC-normalised (an `ó` typed two ways is one key) and may be neither empty nor hold a NUL; a value is anything
JSON holds, read back as JSON gives it. SQLite's own locking is tested on the real file (the contract suite's
SqliteTable leg), never assumed of the in-memory one."""
from __future__ import annotations

import json
import re
import sqlite3
import unicodedata
from typing import Any, Dict, Iterable, List, Optional, Protocol, Sequence

from seams.tool_files import Files


class StateUnreadable(ValueError):
    """A state file that is there but holds no JSON (torn by a writer that did not replace it atomically)."""


class JsonState:
    def __init__(self, files: Files, path: str) -> None:
        self.files, self.path = files, path

    def load(self, default: Any = None) -> Any:
        try:
            text = self.files.read_text(self.path)
        except FileNotFoundError:
            return default
        try:
            return json.loads(text)
        except ValueError as e:
            raise StateUnreadable(f"{self.path}: no JSON ({e})") from None

    def save(self, value: Any) -> None:
        self.files.replace_text(self.path, json.dumps(value, indent=2, sort_keys=True, ensure_ascii=False) + "\n")


def cell(value: object) -> str:
    return str(value).replace("\t", " ").replace("\n", " ")


class TsvState:
    def __init__(self, files: Files, path: str) -> None:
        self.files, self.path = files, path

    def rows(self) -> List[List[str]]:
        lines = self.files.read_text_or(self.path, "").splitlines()
        return [line.split("\t") for line in lines if line.strip() and not line.startswith("#")]

    def save(self, rows: Iterable[Sequence[object]], comment: str = "") -> None:
        head = "".join(f"# {line}\n" for line in comment.splitlines())
        self.files.replace_text(self.path, head + "".join("\t".join(cell(c) for c in row) + "\n" for row in rows))


class Table(Protocol):
    """The boundary: text values by text keys, already normalised and checked."""

    def read(self, key: str) -> Optional[str]: ...

    def write(self, key: str, text: str) -> None: ...

    def delete(self, key: str) -> bool: ...

    def keys(self) -> List[str]: ...                                       # any order


class InMemoryTable:
    def __init__(self) -> None:
        self.values: Dict[str, str] = {}

    def read(self, key: str) -> Optional[str]:
        return self.values.get(key)

    def write(self, key: str, text: str) -> None:
        self.values[key] = text

    def delete(self, key: str) -> bool:
        return self.values.pop(key, None) is not None

    def keys(self) -> List[str]:
        return list(self.values)


TABLE_NAME = re.compile(r"[A-Za-z_][A-Za-z0-9_]*\Z")


class SqliteTable:
    """One table of a SQLite file; each call its own connection and transaction (SQLite's lock, waited on up to
    `timeout` seconds, serialises writers across processes)."""

    def __init__(self, path: str, name: str = "kv", timeout: float = 30.0) -> None:
        if not TABLE_NAME.match(name):
            raise ValueError(f"not a table name: {name!r}")
        self.path, self.name, self.timeout = path, name, timeout
        self.run(f"CREATE TABLE IF NOT EXISTS {name} (key TEXT PRIMARY KEY, value TEXT NOT NULL)")

    def run(self, sql: str, *parameters: str) -> List[Any]:
        connection = sqlite3.connect(self.path, timeout=self.timeout)
        try:
            with connection:
                cursor = connection.execute(sql, parameters)
                return [cursor.rowcount, *cursor.fetchall()]
        finally:
            connection.close()

    def read(self, key: str) -> Optional[str]:
        found = self.run(f"SELECT value FROM {self.name} WHERE key = ?", key)[1:]
        return str(found[0][0]) if found else None

    def write(self, key: str, text: str) -> None:
        self.run(f"INSERT INTO {self.name} (key, value) VALUES (?, ?) "
                 "ON CONFLICT(key) DO UPDATE SET value = excluded.value", key, text)

    def delete(self, key: str) -> bool:
        return int(self.run(f"DELETE FROM {self.name} WHERE key = ?", key)[0]) > 0

    def keys(self) -> List[str]:
        return [str(row[0]) for row in self.run(f"SELECT key FROM {self.name}")[1:]]


def normalised(key: str) -> str:
    if not isinstance(key, str) or not key or "\0" in key:  # pyright: ignore[reportUnnecessaryIsInstance]
        raise ValueError(f"not a key: {key!r} (a non-empty string without NUL)")
    return unicodedata.normalize("NFC", key)


class KeyValue:
    def __init__(self, table: Table) -> None:
        self.table = table

    @classmethod
    def sqlite(cls, path: str, name: str = "kv") -> "KeyValue":
        return cls(SqliteTable(path, name))

    @classmethod
    def in_memory(cls) -> "KeyValue":
        return cls(InMemoryTable())

    def get(self, key: str, default: Any = None) -> Any:
        text = self.table.read(normalised(key))
        return default if text is None else json.loads(text)

    def put(self, key: str, value: Any) -> None:
        self.table.write(normalised(key), json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=False))

    def delete(self, key: str) -> bool:
        return self.table.delete(normalised(key))

    def keys(self, prefix: str = "") -> List[str]:
        return sorted(k for k in self.table.keys() if k.startswith(unicodedata.normalize("NFC", prefix)))
