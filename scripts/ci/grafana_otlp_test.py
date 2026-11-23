#!/usr/bin/env python3
"""grafana_otlp.py: the payload POSTed as JSON with the bearer token to <url>/ci/otlp/v1/metrics, bounded; None when
pushed, else why not (the status, the error), never raising. The default opener's no-redirect is ci_timings_test's."""
from __future__ import annotations

import json
import os
import sys
import unittest
from typing import Any, List

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import grafana_otlp  # noqa: E402


class Answer:
    def __init__(self, status: int) -> None:
        self.status = status

    def __enter__(self) -> "Answer":
        return self

    def __exit__(self, *exc: object) -> None:
        return None


class PostTest(unittest.TestCase):
    def test_posts_and_says_why_not(self) -> None:
        sent: List[Any] = []

        def opener(status: int):
            def open_(request: Any, timeout: float) -> Answer:
                sent.append((request, timeout))
                return Answer(status)
            return open_

        self.assertIsNone(grafana_otlp.post({"a": 1}, "https://g/", "tok", opener(200)))
        request, timeout = sent[0]
        self.assertEqual(("https://g/ci/otlp/v1/metrics", "POST", "Bearer tok", {"a": 1}, 20),
                         (request.full_url, request.get_method(), request.get_header("Authorization"),
                          json.loads(request.data), timeout))
        self.assertEqual("HTTP 500", grafana_otlp.post({}, "https://g", "t", opener(500)))

        def refuse(request: Any, timeout: float) -> Answer:
            raise OSError("connection refused")

        self.assertEqual("connection refused", grafana_otlp.post({}, "https://g", "t", refuse))


if __name__ == "__main__":
    unittest.main()
