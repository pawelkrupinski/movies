#!/usr/bin/env python3
"""xcresult_junit.py over a trimmed real `xcresulttool get test-results tests` capture (Xcode 26,
a local KinowoUITests run with a failure, a skip and a pass). Run: python3 scripts/ci/xcresult_junit_test.py"""
import json
import sys
import tempfile
import unittest
import xml.etree.ElementTree as ET
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import xcresult_junit  # noqa: E402

CAPTURE = {
    "devices": [{"architecture": "arm64", "deviceName": "iPhone 17", "platform": "iOS Simulator"}],
    "testNodes": [{
        "name": "Kinowo", "nodeType": "Test Plan", "result": "Failed",
        "children": [{
            "name": "KinowoUITests", "nodeType": "UI test bundle", "result": "Failed",
            "children": [
                {"name": "CityChoiceSearchUITests", "nodeType": "Test Suite", "result": "Failed",
                 "children": [{
                     "name": "testDiacriticTypedQueryFindsThePolishCity()", "nodeType": "Test Case",
                     "result": "Failed", "duration": "10s", "durationInSeconds": 10.776026964187622,
                     "children": [{
                         "name": "XCTAssertTrue failed - 'lodz' did not surface 'Łódź'",
                         "nodeType": "Failure Message",
                         "sourceLocation": {"filePath": "/w/ios/KinowoUITests/CityChoiceSearchUITests.swift",
                                            "lineNumber": 57}}]}]},
                {"name": "FilterSheetUITests", "nodeType": "Test Suite", "result": "Passed",
                 "children": [
                     {"name": "testClosingFiltrySheet()", "nodeType": "Test Case", "result": "Skipped",
                      "durationInSeconds": 0.07328999042510986,
                      "children": [{"name": "Test skipped - Filtry button a11y tree needs flattening",
                                    "nodeType": "Skip Message"}]},
                     {"name": "testLaunchHookOpensFiltrySheet()", "nodeType": "Test Case", "result": "Passed",
                      "duration": "7s", "durationInSeconds": 7.611055970191956}]},
            ]}]}],
}


class XcresultJunitTest(unittest.TestCase):
    def convert(self, tree):
        path = Path(tempfile.mkdtemp()) / "tests.json"
        path.write_text(json.dumps(tree))
        return ET.fromstring(xcresult_junit.junit(xcresult_junit.load_tree(["x", "--json", str(path)])))

    def case(self, root, name):
        [found] = [c for c in root.iter("testcase") if c.get("name") == name]
        return found

    def test_every_test_case_lands_under_its_own_class_not_uncategorized(self):
        root = self.convert(CAPTURE)
        self.assertEqual([s.get("name") for s in root.iter("testsuite")],
                         ["CityChoiceSearchUITests", "FilterSheetUITests"])
        self.assertEqual(self.case(root, "testLaunchHookOpensFiltrySheet").get("classname"), "FilterSheetUITests")
        self.assertEqual(root.get("tests"), "3")

    def test_a_failure_carries_its_assertion_and_line(self):
        failure = self.case(self.convert(CAPTURE), "testDiacriticTypedQueryFindsThePolishCity").find("failure")
        self.assertIsNotNone(failure)
        self.assertIn("did not surface 'Łódź'", failure.get("message"))
        self.assertIn("CityChoiceSearchUITests.swift:57", failure.text)

    def test_skips_and_passes_are_told_apart(self):
        root = self.convert(CAPTURE)
        self.assertIsNotNone(self.case(root, "testClosingFiltrySheet").find("skipped"))
        passed = self.case(root, "testLaunchHookOpensFiltrySheet")
        self.assertEqual(list(passed), [])
        self.assertEqual(passed.get("time"), "7.611")
        self.assertEqual(root.find("testsuite").get("failures"), "1")

    def test_a_failure_nested_under_a_repetition_is_still_reported(self):
        tree = json.loads(json.dumps(CAPTURE))
        case = tree["testNodes"][0]["children"][0]["children"][0]["children"][0]
        case["children"] = [{"name": "First Run", "nodeType": "Repetition", "result": "Failed",
                             "children": case["children"]}]
        failure = self.case(self.convert(tree), "testDiacriticTypedQueryFindsThePolishCity").find("failure")
        self.assertIn("did not surface", failure.get("message"))


if __name__ == "__main__":
    unittest.main()
