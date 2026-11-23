"""The Android store-screenshot driver's SCREENS (android/scripts/store_screenshots.py): the gate (country pills, city
search, city rows) is driven through the accessibility tree, tapping by LABEL rather than by pixel. A coordinate
cannot tell "the screen I meant" from whatever is on screen: a tap sent before the gate rendered is swallowed and
every later tap lands on the wrong screen (that is how `en-GB Liverpool` once produced GERMAN screenshots). Waiting for
a node with the expected text makes each step assert the state it thinks it is in."""
from __future__ import annotations

import os
import re
from typing import Optional, Tuple

from store_screenshots_common import ALL_COUNTRIES, COUNTRY, city_area_count, fold, naps
from store_screenshots_device import Device

Point = Tuple[int, int]
NODE = re.compile(r"<node[^>]*>")
TEXT = re.compile(r" text=\"([^\"]*)\"")
BOUNDS = re.compile(r" bounds=\"\[(\d+),(\d+)\]\[(\d+),(\d+)\]\"")
POLISH = {"ą": "a", "ć": "c", "ę": "e", "ł": "l", "ń": "n", "ó": "o", "ś": "s", "ź": "z", "ż": "z"}


def centre(node: str) -> Optional[Point]:
    bounds = BOUNDS.search(node)
    if not bounds:
        return None
    x1, y1, x2, y2 = (int(v) for v in bounds.groups())
    return (x1 + x2) // 2, (y1 + y2) // 2


def node_center(xml: str, text: str) -> Optional[Point]:
    """The centre of the first node reading `text` (case and diacritics folded, as the app's city search does), its
    text fields skipped: typing "Liverpool" makes the search box itself read "Liverpool"."""
    want = fold(text)
    for match in NODE.finditer(xml):
        node = match.group(0)
        label = TEXT.search(node)
        if "EditText" not in node and label and BOUNDS.search(node) and fold(label.group(1)) == want:
            return centre(node)
    return None


def field(xml: str) -> Optional[str]:
    """The gate's search box: its only EditText (unlabelled, so found by class)."""
    return next((m.group(0) for m in NODE.finditer(xml) if "android.widget.EditText" in m.group(0)), None)


def search_term(city: str) -> str:
    """The ASCII to type so the app filters to `city`: `adb shell input text` cannot send ł/ó/ü and mangles spaces,
    & and commas, and the app folds ONLY the nine Polish diacritics and matches by SUBSTRING -- so the longest
    [a-z0-9] token of the Polish-folded name ("Kraków" → krakow, "München" → nchen, "Newcastle upon Tyne" →
    newcastle)."""
    folded = "".join(POLISH.get(c, c) for c in city.lower())
    tokens = re.findall(r"[a-z0-9]+", folded)
    return max(tokens, key=len) if tokens else ""


class Screen:
    def __init__(self, device: Device) -> None:
        self.device = device
        self.rig = device.rig

    def nap(self, seconds: float) -> None:
        naps(self.rig.env, seconds)

    def ui_xml(self) -> str:
        """The accessibility tree. uiautomator now and then returns the window frame with an EMPTY semantics tree,
        indistinguishable from a blank screen except that it carries no text: such a dump is retried."""
        xml = ""
        for _ in range(4):
            self.device.adb("shell", "uiautomator", "dump", "/sdcard/kinowo-ui.xml")
            xml = self.device.read("shell", "cat", "/sdcard/kinowo-ui.xml")
            if re.search(r'text="[^"]+"', xml):
                return xml
            self.nap(1)
        return xml

    def wait_text(self, text: str, limit: int = 120) -> Optional[Point]:
        waited = 0
        while waited < limit:
            point = node_center(self.ui_xml(), text)
            if point:
                return point
            self.nap(3)
            waited += 3
        return None

    def tap_text(self, text: str, limit: int = 120) -> None:
        point = self.wait_text(text, limit)
        if not point:
            raise self.rig.output.die(f"'{text}' never appeared on screen — the app is on an unexpected screen.")
        self.device.tap(point)

    def wait_gate(self, limit: int = 150) -> bool:
        """The gate is up once ANY country pill is drawn: the row opens wherever the persisted country left it."""
        waited = 0
        while waited < limit:
            xml = self.ui_xml()
            if any(node_center(xml, COUNTRY[c].name) for c in ALL_COUNTRIES):
                return True
            self.nap(3)
            waited += 3
        return False

    def screen_width(self) -> int:
        size = re.search(r".*: *([0-9]+)x[0-9]+", self.device.read("shell", "wm", "size").replace("\r", ""))
        return int(size.group(1)) if size else 1080

    def scroll_to_country(self, name: str) -> Optional[Point]:
        """The pill reading `name`, the row swiped leftwards (at a visible pill's height, within the screen's WIDTH)
        until it shows: uiautomator reports only what is on screen. Bounded by time, not swipes -- the row fills
        from /api/catalog a moment after the first pill renders."""
        point = node_center(self.ui_xml(), name)
        if point:
            return point
        xml = self.ui_xml()
        rows = [p[1] for p in (node_center(xml, COUNTRY[c].name) for c in ALL_COUNTRIES) if p]
        if not rows:
            return None
        width = self.screen_width()
        waited = 0
        while waited < 60:
            self.device.adb("shell", "input", "swipe", str(width * 4 // 5), str(rows[0]), str(width // 5),
                            str(rows[0]), "300")
            self.nap(2)
            waited += 2
            point = node_center(self.ui_xml(), name)
            if point:
                return point
        return None

    def tap_field(self) -> None:
        box = field(self.ui_xml())
        point = centre(box) if box else None
        if not point:
            raise self.rig.output.die("no search field on the gate — the app is on an unexpected screen.")
        self.device.tap(point)

    def field_text(self) -> str:
        box = field(self.ui_xml())
        label = TEXT.search(box) if box else None
        return label.group(1) if label else ""

    def type_field_verified(self, want: str) -> bool:
        """`want` typed into the focused search box and read back, cleared and retyped until it matches: `input
        text` drops and doubles characters on a starved emulator ("Warszawa" once came out "WWars")."""
        for _ in range(4):
            got = self.field_text()
            if got == want:
                return True
            if got:
                self.device.adb("shell", "input", "keyevent", "KEYCODE_MOVE_END")
                for _ in range(len(got) + 2):
                    self.device.adb("shell", "input", "keyevent", "67")
            self.nap(1)
            self.device.type_(want)
            self.nap(2)
        return self.field_text() == want

    def wait_frame(self, limit: int, least: int) -> bool:
        """A screencap polled until it is bigger than `least` bytes (a blank frame compresses tiny)."""
        frame = self.rig.noise + ".frame"
        waited = 0
        try:
            while waited < limit:
                if self.device.snap(frame) and os.path.getsize(frame) > least:
                    return True
                self.nap(2)
                waited += 2
            return False
        finally:
            if os.path.exists(frame):
                os.remove(frame)

    def showing(self, text: str) -> bool:
        return node_center(self.ui_xml(), text) is not None

    def dismiss_area_picker(self, country: str, term: str, limit: int) -> None:
        """A SPLIT city's first-visit area picker confirmed (every area arrives pre-ticked: the flat default). The
        backend says which cities are split, so a flat city never waits; a split one whose picker never shows is
        on some other screen and fails; an unknown count (-1) carries on."""
        areas = city_area_count(self.rig.env, country, term)
        if areas == 0:
            return
        label = COUNTRY[country].showlist_label
        point = self.wait_text(label, limit)
        if not point:
            if areas > 0:
                raise self.rig.output.die(f"{term} is split into {areas} areas but its picker never appeared — "
                                          "refusing to capture the wrong screen.")
            return
        for _ in range(2):
            self.device.tap(point)
            self.nap(3)
            if not self.showing(label):
                return
        raise self.rig.output.die(f"{term}'s area picker would not dismiss — '{label}' is still on screen after two "
                                  "taps.")

