"""THE DEPLOYED SCALA SOURCES (scripts/lib): the modules whose `src/main` ships (quality.QualityReport.ProductionModules)
and whether a path is one of their Scala sources, as the tools that measure or judge only what ships read it.

  is_production("worker/src/main/scala/services/X.scala") -> True; a test, a resource or a testkit file -> False"""
from __future__ import annotations

PRODUCTION_MODULES = ("geodata", "base", "http", "common", "identity", "scrapers", "worker", "web")


def is_production(path: str) -> bool:
    parts = path.split("/")
    return len(parts) > 3 and parts[0] in PRODUCTION_MODULES and parts[1:3] == ["src", "main"] and path.endswith(".scala")
