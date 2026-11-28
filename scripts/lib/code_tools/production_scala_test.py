#!/usr/bin/env python3
"""production_scala.py: only a deployed module's `src/main` Scala source is production."""
from __future__ import annotations

import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from code_tools import production_scala  # noqa: E402


class ProductionScalaTest(unittest.TestCase):
    def test_a_deployed_modules_main_scala_is_production(self) -> None:
        self.assertTrue(production_scala.is_production("scrapers/src/main/scala/services/cinemas/Genres.scala"))

    def test_tests_resources_other_modules_and_other_languages_are_not(self) -> None:
        for path in ("scrapers/src/test/scala/services/cinemas/GenresSpec.scala", "testkit/src/main/scala/tools/X.scala",
                     "scrapers/src/main/resources/x.tsv", "worker/src/main/scala.sbt", "X.scala"):
            self.assertFalse(production_scala.is_production(path), path)


if __name__ == "__main__":
    unittest.main()
