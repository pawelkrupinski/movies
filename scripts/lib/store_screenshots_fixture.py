"""The store-screenshot drivers' TEST FIXTURE (android/scripts/store_screenshots_test.py, ios/scripts/
store_screenshots_test.py): a backend of the test's own on localhost, a scratch copy of the drivers' layout, and the
PNG bytes a fake device hands back. Nothing here reaches a store, a device or the live backend: the drivers read every
country's backend at `$STORE_API_BASE/<country>` and skip every pause under `STORE_NAPS=0`.

The backend serves what both apps read: `/<cc>/api/catalog` (`{"cities": [{slug, name, country}]}`),
`/<cc>/<slug>/api/repertoire` (a list of films, each `{"title"}`) and `/<cc>/<slug>/api/cinemas` (`{"areas": [...]}`),
or a 500 for a slug the test says is broken."""
from __future__ import annotations

import glob
import json
import os
import shutil
import struct
import threading
import zlib
from dataclasses import dataclass, field
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from typing import Dict, List, Optional, Tuple

import cli_harness

# What a driver takes with it into the scratch layout, whichever of them exist: the shell drivers and what they
# source, or the Python drivers and the modules they import
LAYOUT_GLOBS = ["android/scripts/store*", "ios/scripts/store*", "scripts/store*", "scripts/log.sh", "scripts/lib/*.py"]


@dataclass
class City:
    slug: str
    name: str
    films: List[str] = field(default_factory=lambda: ["Dune"])
    areas: int = 0
    # the cinemas lookup answers 500: the area count is unknown
    cinemas_broken: bool = False


class Backend:
    """Each country's cities, served on 127.0.0.1 while the case runs (`start` / `stop`)."""

    def __init__(self) -> None:
        self.countries: Dict[str, List[City]] = {}
        self.server: Optional[ThreadingHTTPServer] = None
        self.requests: List[str] = []

    def add(self, country: str, *cities: City) -> None:
        self.countries.setdefault(country, []).extend(cities)

    def answer(self, path: str) -> Tuple[int, object]:
        parts = [p for p in path.split("?")[0].split("/") if p]
        if len(parts) == 3 and parts[1:] == ["api", "catalog"]:
            cities = [{"slug": c.slug, "name": c.name, "country": parts[0]} for c in self.countries.get(parts[0], [])]
            return 200, {"cities": cities}
        if len(parts) != 4 or parts[2] != "api":
            return 404, {}
        city = next((c for c in self.countries.get(parts[0], []) if c.slug == parts[1]), None)
        if city is None:
            return 404, {}
        if parts[3] == "repertoire":
            return 200, [{"title": title} for title in city.films]
        if city.cinemas_broken:
            return 500, {}
        return 200, {"areas": [{"name": f"Area {n}"} for n in range(city.areas)]}

    def start(self) -> str:
        backend = self

        class Handler(BaseHTTPRequestHandler):
            def do_GET(self) -> None:  # noqa: N802 -- http.server's name
                backend.requests.append(self.path)
                code, body = backend.answer(self.path)
                data = json.dumps(body).encode()
                self.send_response(code)
                self.send_header("Content-Type", "application/json")
                self.send_header("Content-Length", str(len(data)))
                self.end_headers()
                self.wfile.write(data)

            def log_message(self, format: str, *args: object) -> None:  # noqa: A002 -- the base class's name
                pass

        self.server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        threading.Thread(target=self.server.serve_forever, daemon=True).start()
        return f"http://127.0.0.1:{self.server.server_address[1]}"

    def stop(self) -> None:
        if self.server is not None:
            self.server.shutdown()
            self.server.server_close()


def copy_layout(destination: str, skip: Tuple[str, ...] = ()) -> None:
    """The drivers' files (LAYOUT_GLOBS) copied under `destination` at their repository paths."""
    for pattern in LAYOUT_GLOBS:
        for source in glob.glob(os.path.join(cli_harness.ROOT, pattern)):
            relative = os.path.relpath(source, cli_harness.ROOT)
            if os.path.isfile(source) and relative not in skip:
                os.makedirs(os.path.join(destination, os.path.dirname(relative)), exist_ok=True)
                shutil.copy2(source, os.path.join(destination, relative))


def png(width: int, height: int, size: int = 0) -> bytes:
    """A PNG whose IHDR says `width` x `height`, padded with a trailing chunk to at least `size` bytes: a rendered
    frame's byte count, as a capture's size thresholds read it."""
    def chunk(kind: bytes, data: bytes) -> bytes:
        return struct.pack(">I", len(data)) + kind + data + struct.pack(">I", zlib.crc32(kind + data) & 0xFFFFFFFF)
    head = b"\x89PNG\r\n\x1a\n" + chunk(b"IHDR", struct.pack(">IIBBBBB", width, height, 8, 2, 0, 0, 0))
    filler = chunk(b"tEXt", b"x" * max(0, size - len(head) - 24))
    return head + filler + chunk(b"IEND", b"")
