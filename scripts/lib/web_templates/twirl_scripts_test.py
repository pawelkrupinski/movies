#!/usr/bin/env python3
"""twirl_scripts.py: a template's inline scripts read as JavaScript -- data blocks and `src` scripts left out, Twirl
comments and expressions neutralised, a `@for` body kept and an `@if` kept as a JS `if`."""
from __future__ import annotations

import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from web_templates import twirl_scripts  # noqa: E402
from seams import tool_files  # noqa: E402

T = "web/src/main/twirl/views/x.scala.html"


class TwirlScriptsTest(unittest.TestCase):
    def test_only_inline_javascript_is_a_script(self) -> None:
        text = ("<script src=\"@a\"></script><script type=\"application/json\">@x</script>"
                "<script id=\"i\">var a = 1;</script> @* <script>commented()</script> *@<script type=\"module\">b()</script>")
        self.assertEqual({f"{T}.script1.js": "var a = 1;", f"{T}.script2.js": "b()"}, twirl_scripts.scripts(T, text))

    def test_expressions_become_a_name_and_double_at_a_literal_at(self) -> None:
        js = twirl_scripts.scripts(T, '<script>var a = "@city.slug", b = @{x + 1}, c = @f(g(1)).h, d = "a@@b";</script>')
        self.assertEqual('var a = "twirl", b = twirl, c = twirl, d = "a@b";', js[f"{T}.script1.js"])

    def test_an_if_stays_an_if_and_a_for_keeps_its_body(self) -> None:
        js = twirl_scripts.scripts(T, '<script>@if(a) { go(); }\nvar m = {@for(c <- cs) {"@c":"@c.u",}};</script>')
        self.assertEqual('if (twirl) { go(); }\nvar m = {"twirl":"twirl",};', js[f"{T}.script1.js"])

    def test_the_templates_of_a_checkout_are_materialized(self) -> None:
        files = tool_files.Files.in_memory()
        files.write_text("/r/" + T, "<script>x()</script>")
        files.write_text("/r/web/src/main/twirl/views/y.txt", "<script>no()</script>")
        self.assertEqual(1, twirl_scripts.materialize(files, "/r", "/s"))
        self.assertEqual("x()", files.read_text(f"/s/{T}.script1.js"))

    def test_a_measuring_pass_over_the_scripts_appends_its_rows_and_not_its_limits(self) -> None:
        files = tool_files.Files.in_memory()
        files.write_text("/r/" + T, "<script>x()</script>")
        files.write_text("/out/over-limit.tsv", "limit\tcognitive\t10\n")

        def pass_over(scratch: str, where: str) -> int:
            self.assertEqual("x()", files.read_text(f"{scratch}/{T}.script1.js"))
            files.write_text(f"{where}/over-limit.tsv", "limit\tcognitive\t10\ncognitive\tf.js:g\t12\n")
            return 0

        self.assertEqual(0, twirl_scripts.measure(files, "/r", pass_over, "/out/over-limit.tsv"))
        self.assertEqual("limit\tcognitive\t10\ncognitive\tf.js:g\t12\n", files.read_text("/out/over-limit.tsv"))

    def test_a_failed_pass_appends_nothing_and_no_script_runs_no_pass(self) -> None:
        files = tool_files.Files.in_memory()
        files.write_text("/out/over-limit.tsv", "limit\tcognitive\t10\n")
        self.assertEqual(0, twirl_scripts.measure(files, "/r", lambda *_: 1 / 0, "/out/over-limit.tsv"))
        files.write_text("/r/" + T, "<script>x()</script>")
        self.assertEqual(3, twirl_scripts.measure(files, "/r", lambda *_: 3, "/out/over-limit.tsv"))
        self.assertEqual("limit\tcognitive\t10\n", files.read_text("/out/over-limit.tsv"))


if __name__ == "__main__":
    unittest.main()
