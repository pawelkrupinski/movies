#!/usr/bin/env python3
"""hook_event.py: the event is read within a bound, and a silent writer or a terminal gives {} instead of a wait."""
from __future__ import annotations

import os
import sys
import threading
import time
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import hook_event  # noqa: E402


class ReadEventTest(unittest.TestCase):
    def test_a_complete_event_is_read(self) -> None:
        read, write = os.pipe()
        os.write(write, b'{"tool_input": {"command": "ls"}}')
        os.close(write)
        self.assertEqual(hook_event.read_event(1, read), {"tool_input": {"command": "ls"}})

    def test_a_writer_that_never_finishes_gives_nothing_within_the_bound(self) -> None:
        read, write = os.pipe()
        os.write(write, b'{"tool_in')  # the pipe stays open
        started = time.monotonic()
        self.assertEqual(hook_event.read_event(0.3, read), {})
        self.assertLess(time.monotonic() - started, 2)
        os.close(write)

    def test_a_slow_writer_inside_the_bound_is_read(self) -> None:
        read, write = os.pipe()

        def late() -> None:
            time.sleep(0.2)
            os.write(write, b"{}")
            os.close(write)

        threading.Thread(target=late).start()
        self.assertEqual(hook_event.read_event(2, read), {})

    def test_a_document_that_is_no_object_gives_nothing(self) -> None:
        read, write = os.pipe()
        os.write(write, b"[1, 2]")
        os.close(write)
        self.assertEqual(hook_event.read_event(1, read), {})


if __name__ == "__main__":
    unittest.main()
