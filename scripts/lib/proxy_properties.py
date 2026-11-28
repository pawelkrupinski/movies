"""THE RESIDENTIAL PROXY POOL, as the CI probes read it (scripts/lib): the worker's committed residential-proxy.properties
(`host=` and a comma-separated `ports=`), a Java properties file -- `key=value` lines, `#` comments and blanks aside.

  parse(text) -> {key: value}          every property of the file's text
  pool(text) -> (host, [port, ...])    the pool the text names

Pure: the caller reads DEFAULT (or any file) through its Files and hands the text in."""
from __future__ import annotations

import os
from typing import Dict, List, Tuple

DEFAULT = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))),
                       "worker", "src", "main", "resources", "residential-proxy.properties")


def parse(text: str) -> Dict[str, str]:
    properties: Dict[str, str] = {}
    for line in text.splitlines():
        line = line.strip()
        if line and not line.startswith("#") and "=" in line:
            key, value = line.split("=", 1)
            properties[key.strip()] = value.strip()
    return properties


def pool(text: str) -> Tuple[str, List[str]]:
    properties = parse(text)
    return properties["host"], [port.strip() for port in properties["ports"].split(",") if port.strip()]
