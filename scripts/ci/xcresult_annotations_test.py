#!/usr/bin/env python3
"""xcresult_annotations.py over a trimmed real `xcresulttool get test-results tests` capture: the result bundle of
Order independence #31's failed RepertoireLaunchUITests (run 41324431918, Xcode 26.6, iOS 26.5).
Run: python3 scripts/ci/xcresult_annotations_test.py"""
import contextlib
import io
import json
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import xcresult_annotations  # noqa: E402

CHECKOUT = "/Users/runner/work/movies/movies"
CAPTURE = {
    "devices": [{"architecture": "arm64", "deviceName": "iPhone 17", "osVersion": "26.5",
                 "platform": "iOS Simulator"}],
    "testNodes": [{
        "name": "Kinowo", "nodeType": "Test Plan", "result": "Failed",
        "children": [{
            "name": "KinowoUITests", "nodeType": "UI test bundle", "result": "Failed",
            "children": [{
                "name": "RepertoireLaunchUITests", "nodeType": "Test Suite", "result": "Failed",
                "children": [
                    {"name": "testAppLaunches()", "nodeType": "Test Case", "result": "Failed",
                     "duration": "34s", "durationInSeconds": 34.09842503070831,
                     "nodeIdentifier": "RepertoireLaunchUITests/testAppLaunches()",
                     "children": [{
                         "name": "XCTAssertTrue failed - Expected at least one film card after launch",
                         "nodeType": "Failure Message",
                         "sourceLocation": {
                             "filePath": CHECKOUT + "/ios/KinowoUITests/RepertoireLaunchUITests.swift",
                             "lineNumber": 25}}]},
                    {"name": "testEmptyStateIfNoNetwork()", "nodeType": "Test Case", "result": "Skipped",
                     "duration": "0,081s", "durationInSeconds": 0.08077692985534668,
                     "children": [{"name": "Test skipped - needs KINOWO_FORCE_OFFLINE wiring in RepertoireClient",
                                   "nodeType": "Skip Message"}]},
                ]}]}]}],
}


def run(tree, *args, checkout=CHECKOUT):
    """The script's stdout and exit status for `tree`, as if run from `checkout`."""
    path = Path(tempfile.mkdtemp()) / "tests.json"
    path.write_text(json.dumps(tree))
    out = io.StringIO()
    with contextlib.redirect_stdout(out):
        status = xcresult_annotations.main(["xcresult_annotations.py", *args, "--json", str(path)], checkout)
    return out.getvalue().splitlines(), status


class XcresultAnnotationsTest(unittest.TestCase):
    def test_a_failure_is_an_error_carrying_its_test_and_assertion_at_its_line(self):
        lines, status = run(CAPTURE)
        self.assertEqual(lines, [
            "::error file=ios/KinowoUITests/RepertoireLaunchUITests.swift,line=25,"
            "title=RepertoireLaunchUITests.testAppLaunches::"
            "RepertoireLaunchUITests.testAppLaunches: "
            "XCTAssertTrue failed - Expected at least one film card after launch"])
        self.assertEqual(status, 0)

    def test_warning_level(self):
        [line], _ = run(CAPTURE, "--level", "warning")
        self.assertTrue(line.startswith("::warning file=ios/KinowoUITests/"), line)

    def test_skips_and_passes_say_nothing(self):
        tree = json.loads(json.dumps(CAPTURE))
        suite = tree["testNodes"][0]["children"][0]["children"][0]
        suite["children"][0]["result"] = "Passed"
        del suite["children"][0]["children"]
        self.assertEqual(run(tree), ([], 0))

    def test_newlines_percents_colons_and_commas_cannot_break_the_command(self):
        tree = json.loads(json.dumps(CAPTURE))
        case = tree["testNodes"][0]["children"][0]["children"][0]["children"][0]
        case["name"] = "test50%Off(a:b,c)"
        case["children"][0]["name"] = "first line: 100%\nsecond line"
        [line], _ = run(tree)
        self.assertEqual(line.split("title=")[1].split("::")[0], "RepertoireLaunchUITests.test50%25Off(a%3Ab%2Cc)")
        self.assertTrue(line.endswith("::RepertoireLaunchUITests.test50%25Off(a:b,c): first line: 100%25%0Asecond line"),
                        line)

    def test_a_location_outside_the_checkout_keeps_the_message_and_drops_the_file(self):
        [line], _ = run(CAPTURE, checkout="/somewhere/else")
        self.assertTrue(line.startswith("::error title=RepertoireLaunchUITests.testAppLaunches::"), line)

    def test_an_unreadable_bundle_is_a_warning_never_a_failure_of_its_own(self):
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            status = xcresult_annotations.main(["xcresult_annotations.py", "--json", "/nonexistent/tests.json"],
                                               CHECKOUT)
        self.assertEqual(status, 0)
        self.assertTrue(out.getvalue().startswith("::warning title=xcresult::could not read"), out.getvalue())


if __name__ == "__main__":
    unittest.main()
