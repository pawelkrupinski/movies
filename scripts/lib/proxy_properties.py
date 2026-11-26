"""THE RESIDENTIAL PROXY POOL, as the CI probes read it (scripts/lib): the worker's committed residential-proxy.properties
(`host=` and a comma-separated `ports=`), a Java properties file -- `key=value` lines, `#` comments and blanks aside.

  read(path) -> {key: value}          every property
  read_pool(path=DEFAULT) -> (host, [port, ...])"""
from __future__ import annotations

import os
from typing import Dict, List, Tuple

DEFAULT = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))),
                       "worker", "src", "main", "resources", "residential-proxy.properties")


def read(path: str) -> Dict[str, str]:
    properties: Dict[str, str] = {}
    with open(path, encoding="utf-8") as source:
        for line in source:
            line = line.strip()
            if line and not line.startswith("#") and "=" in line:
                key, value = line.split("=", 1)
                properties[key.strip()] = value.strip()
    return properties


def read_pool(path: str = DEFAULT) -> Tuple[str, List[str]]:
    properties = read(path)
    return properties["host"], [port.strip() for port in properties["ports"].split(",") if port.strip()]
