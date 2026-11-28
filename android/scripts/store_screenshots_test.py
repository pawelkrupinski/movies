#!/usr/bin/env python3
"""android/scripts/store-screenshots.sh, black-box, over a scratch copy of its layout: a fake adb that plays the app's
screens (the gate's country pills and city search, the area picker, the listing), a fake emulator, Gradle, sips and
`open`, and a backend of the test's own (scripts/lib/mobile/store_screenshots_fixture.py). Nothing reaches a device, a store
or the live backend. The fake app's labels are the app's own strings, so a capture hunting a wrong one fails as it would
on the emulator. Ported from store-screenshots-test.sh case for case; the same cases hold the shell tool and its Python
rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import json
import os
import signal
import subprocess
import sys
import unicodedata
import unittest
from typing import Dict, List, Optional

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "..", "scripts", "lib"))
from harness import cli_harness  # noqa: E402
from mobile.store_screenshots_fixture import Backend, City, copy_layout, png  # noqa: E402

TOOL = cli_harness.tool_path("android/scripts/store_screenshots.py", "android/scripts/store-screenshots.sh")
LISTINGS = "android/app/src/main/play/listings"
# The fake app. Its labels are the app's: Country.displayName, the gate's header, R.string.all, the picker's confirm.
ADB = r'''
import fcntl, re, time, unicodedata
NAMES = {"pl": "Polska", "uk": "United Kingdom", "de": "Deutschland", "us": "United States", "es": "España"}
HEADERS = {"pl": "Kraj", "uk": "Country", "de": "Land", "us": "Country", "es": "País"}
MARKERS = {"pl": "Wszystkie", "uk": "All", "de": "Alle", "us": "All", "es": "Todo"}
CONFIRM = {"pl": "Pokaż repertuar", "uk": "Show listings", "de": "Programm anzeigen", "us": "Show listings",
           "es": "Ver la cartelera"}
POLISH = {"ą": "a", "ć": "c", "ę": "e", "ł": "l", "ń": "n", "ó": "o", "ś": "s", "ź": "z", "ż": "z"}
app = json.load(open(env["FAKE_APP"]))
serial = argv[1] if argv[:1] == ["-s"] else "emulator-5554"
args = argv[2:] if argv[:1] == ["-s"] else argv
line = " ".join(args)
path = os.path.join(env["FAKE_DIR"], serial + ".json")
lock = open(path + ".lock", "a")
fcntl.flock(lock, fcntl.LOCK_EX)
state = json.load(open(path)) if os.path.exists(path) else {"booted": serial in app.get("running", []), "screen": "home"}
def save():
    json.dump(state, open(path, "w"))
def node(text, x1, y1, x2, y2, cls="android.widget.TextView"):
    return '<node class="%s" text="%s" bounds="[%d,%d][%d,%d]"/>' % (cls, text, x1, y1, x2, y2)
def pills():
    row = [c for c in NAMES if c not in app.get("hidden", [])]
    if state.get("swipes", 0) >= app.get("late", 1):
        row += [c for c in NAMES if c in app.get("hidden", []) and c not in app.get("absent", [])]
    return [(NAMES[c], 100 + i * 250, 204, 300 + i * 250, 264) for i, c in enumerate(row)]
def rows():
    typed = state.get("field", "").lower()
    names = app.get("cities", {}).get(state.get("country", "pl"), [])
    hits = [n for n in names if typed and typed in "".join(POLISH.get(ch, ch) for ch in n.lower())]
    return [(n, 100, 600 + i * 100, 1200, 680 + i * 100) for i, n in enumerate(hits)]
def screen_nodes():
    country = state.get("country", "pl")
    if state["screen"] == "gate":
        return ([(HEADERS[country], 100, 100, 400, 160)] + pills() + rows(), (100, 400, 1200, 500))
    shown = [(MARKERS[country], 1000, 140, 1280, 190), ("Dune", 100, 1200, 400, 1350)]
    if state["screen"] == "picker":
        shown.append((CONFIRM[country], 400, 880, 600, 920))
    return shown, None
def xml():
    state["since_switch"] = state.get("since_switch", -1000) + 1
    if state["since_switch"] == app.get("empty_dump_after_switch"):
        return '<hierarchy><node text="" bounds="[0,0][1344,2992]"/></hierarchy>'
    shown, field = screen_nodes()
    nodes = [node(*n) for n in shown]
    if field:
        nodes.append(node(state.get("field", ""), *field, cls="android.widget.EditText"))
    return "<hierarchy>" + "".join(nodes) + "</hierarchy>"
def hit(x, y):
    shown, _ = screen_nodes()
    return next((n[0] for n in shown if n[1] <= x <= n[3] and n[2] <= y <= n[4]), None)
def tap(x, y):
    label = hit(x, y)
    country = state.get("country", "pl")
    if state["screen"] == "gate" and label in NAMES.values():
        state["country"] = next(c for c, n in NAMES.items() if n == label)
        state["field"] = ""
        state["since_switch"] = 0
    elif state["screen"] == "gate" and label in app.get("cities", {}).get(country, []):
        state["city"] = label
        split = label in app.get("split", []) and label not in app.get("no_picker", [])
        state["screen"] = "browser" if label in app.get("browser", []) else ("picker" if split else "listing")
    elif state["screen"] == "picker" and label == CONFIRM[country] and label and state["city"] not in app.get("stuck", []):
        state["screen"] = "listing"
def shell(words):
    text = " ".join(words)
    if text == "getprop sys.boot_completed":
        out("1" if state["booted"] and serial not in app.get("never_boots", []) else "0")
    elif text == "wm size":
        out("Physical size: 1344x2992")
    elif text.startswith("pm list packages"):
        out("package:net.pawel.kinowo")
    elif text.startswith("pm clear"):
        state.update(cleared=state.get("cleared", 0) + 1, country="pl", field="", screen="home")
    elif text.startswith("am start"):
        block = app.get("block_launch")
        if block:
            open(block + ".reached", "w").close()
            deadline = time.time() + 120
            while not os.path.exists(block) and time.time() < deadline:
                time.sleep(0.05)
        state["screen"] = "gate"
    elif text == "cat /sdcard/kinowo-ui.xml":
        out(xml(), end="")
    elif words[:2] == ["input", "tap"]:
        tap(int(words[2]), int(words[3]))
    elif words[:2] == ["input", "text"]:
        state["field"] = state.get("field", "") + words[2].replace("%s", " ")
    elif words[:3] == ["input", "keyevent", "67"]:
        state["field"] = state.get("field", "")[:-1]
    elif words[:2] == ["input", "swipe"]:
        state["swipes"] = state.get("swipes", 0) + 1
    elif text.startswith("dumpsys activity activities"):
        package = "com.android.chrome" if state["screen"] == "browser" else "net.pawel.kinowo"
        out("  topResumedActivity=ActivityRecord{1 u0 %s/.Main t2}" % package)
def main():
    if args[:1] == ["get-state"]:
        if serial == app.get("dies") and state.get("cleared", 0) >= 1 and not state.get("died"):
            state.update(booted=False, died=True)
        if not state["booted"]:
            err("error: device '%s' not found" % serial)
            return 1
        out("device")
    elif args[:1] == ["get-serialno"]:
        out(serial)
    elif args[:2] == ["emu", "kill"]:
        state["booted"] = False
    elif args[:3] == ["exec-out", "screencap", "-p"]:
        sys.stdout.flush()
        sys.stdout.buffer.write(open(env["FAKE_FRAME"], "rb").read())
    elif args[:1] == ["shell"]:
        if not state["booted"]:
            return 1
        shell(args[1:])
    return 0
code = main()
save()
exit(code)
'''
EMULATOR = r'''
import fcntl
if argv == ["-list-avds"]:
    out(env.get("FAKE_AVDS", "kinowo_xl"))
    exit(0)
port = argv[argv.index("-port") + 1] if "-port" in argv else "5554"
path = os.path.join(env["FAKE_DIR"], "emulator-%s.json" % port)
lock = open(path + ".lock", "a")
fcntl.flock(lock, fcntl.LOCK_EX)
state = json.load(open(path)) if os.path.exists(path) else {"screen": "home"}
state["booted"] = True
json.dump(state, open(path, "w"))
'''
SIPS = r'''
import shutil
shutil.copyfile(argv[argv.index("--out") - 1], argv[argv.index("--out") + 1])
'''
GRADLEW = r'''
case "$*" in *assembleDebug*) mkdir -p app/build/outputs/apk/debug && echo apk > app/build/outputs/apk/debug/app-debug.apk;; esac
[ -z "${FAKE_GRADLE_FAILS:-}" ]
'''


class AndroidCase(cli_harness.ToolCase):
    def setUp(self) -> None:
        super().setUp()
        self.layout = self.path("layout")
        copy_layout(self.layout)
        self.tool = os.path.join(self.layout, os.path.relpath(TOOL, cli_harness.ROOT))
        self.backend = Backend()
        self.base = self.backend.start()
        self.addCleanup(self.backend.stop)
        self.app: Dict[str, object] = {"running": ["emulator-5554"], "cities": {}}
        sdk = self.path("sdk")
        for name, body, folder in (("adb", ADB, "platform-tools"), ("emulator", EMULATOR, "emulator")):
            os.makedirs(os.path.join(sdk, folder), exist_ok=True)
            os.symlink(self.fakes.program(name, body), os.path.join(sdk, folder, name))
        self.fakes.program("sips", SIPS)
        self.fakes.script("open", ":")
        self.fakes.script("pkill", ":")
        gradlew = self.fakes.script("gradlew", GRADLEW, shell="/usr/bin/env bash")
        os.makedirs(self.path("layout/android"), exist_ok=True)
        os.rename(gradlew, self.path("layout/android/gradlew"))
        with open(self.path("frame.png"), "wb") as f:
            f.write(png(1344, 2992, 1_000_000))
        os.makedirs(self.path("device"))

    def city(self, country: str, name: str, films: int = 5, **kind: object) -> None:
        """A city the backend ranks with `films` films and the gate lists; `kind` its areas / split / browser..."""
        slug = slug_of(name)
        self.backend.add(country, City(slug, name, films=[f"Film {n}" for n in range(films)],
                                       areas=int(kind.get("areas", 0)),  # type: ignore[arg-type]
                                       cinemas_broken=bool(kind.get("cinemas_broken"))))
        cities = self.app["cities"]
        assert isinstance(cities, dict)
        cities.setdefault(country, []).append(name)  # type: ignore[union-attr]
        for key in ("split", "browser", "no_picker", "stuck"):
            if kind.get(key) or (key == "split" and kind.get("areas")):
                self.app.setdefault(key, []).append(name)  # type: ignore[union-attr]

    def shoot(self, *args: str, env: Optional[Dict[str, Optional[str]]] = None) -> cli_harness.Done:
        with open(self.path("app.json"), "w") as f:
            json.dump(self.app, f)
        return self.run_tool(*args, tool=self.tool, env={**self.tool_env(), **(env or {})})

    def tool_env(self) -> Dict[str, Optional[str]]:
        return {"ANDROID_SDK_ROOT": self.path("sdk"), "ANDROID_HOME": None, "HOME": self.work,
                "TMPDIR": self.work, "LOCK_ROOT": None, "STORE_API_BASE": self.base, "STORE_NAPS": "0", "FAKE_APP": self.path("app.json"),
                "FAKE_DIR": self.path("device"), "FAKE_FRAME": self.path("frame.png"), "EMULATORS": None,
                "COUNTRIES": None, "NO_OPEN": None, "BUILD": None, "CLEAN_FILM": None, "AVD": None}

    def adb(self) -> List[List[str]]:
        return self.fakes.calls("adb")

    def typed(self) -> List[str]:
        return [c[-1] for c in self.adb() if c[-3:-1] == ["input", "text"]]

    def candidates(self, locale: str) -> str:
        return os.path.join(self.layout, LISTINGS, locale, "graphics/phone-screenshots/candidates")

    def shots(self, folder: str) -> List[str]:
        return sorted(os.listdir(folder)) if os.path.isdir(folder) else []


class CaptureTest(AndroidCase):
    def test_every_country_is_shot_through_its_own_pill_header_listing_and_picker(self) -> None:
        cities = {"pl": ("pl-PL", "Poznań"), "uk": ("en-GB", "London"), "de": ("de-DE", "Berlin"),
                  "us": ("en-US", "Chicago"), "es": ("es-ES", "Madrid")}
        for country, (_, name) in cities.items():
            self.city(country, name, areas=3)
        for country, (locale, name) in cities.items():
            out = self.path("out", country)
            done = self.shoot(locale, name, out)
            self.assertEqual(0, done.code, done.output)
            self.assertEqual(["001.png", "002.png", "003.png", "004.png"], self.shots(out), done.output)
            self.assertIn(f"/{country}/api/catalog", self.backend.requests)
        # the picker's confirm was tapped by its centre, in every country's own words
        self.assertEqual(5, sum(1 for c in self.adb() if c[-4:] == ["input", "tap", "500", "900"]))

    def test_an_unknown_locale_is_refused(self) -> None:
        done = self.shoot("fr-FR", "Paris")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("unknown locale 'fr-FR'", done.stderr)

    def test_a_capture_lands_in_the_candidates_dir_of_the_published_one(self) -> None:
        self.city("de", "Hamburg")
        done = self.shoot("de-DE", "Hamburg")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(["001.png", "002.png", "003.png", "004.png"], self.shots(self.candidates("de-DE")))
        self.assertIn("wrote " + self.candidates("de-DE") + "/{001..004}.png", done.stdout)

    def test_shots_are_padded_play_safe_and_a_clean_film_is_typed_with_its_spaces_encoded(self) -> None:
        self.city("uk", "Leeds")
        done = self.shoot("en-GB", "Leeds", self.path("out"), env={"CLEAN_FILM": "Toy Story 5"})
        self.assertEqual(0, done.code, done.output)
        pads = self.fakes.calls("sips")
        self.assertEqual(4, len(pads))
        self.assertEqual(["--padToHeightWidth", "2992", "1512", "--padColor", "000000"], pads[0][:5])
        self.assertIn("Toy%sStory%s5", self.typed())

    def test_a_second_run_appends_past_the_highest_shot_numerically(self) -> None:
        self.city("uk", "Leeds")
        out = self.path("out")
        os.makedirs(out)
        for name in ("001.png", "008.png", "009.png", "010.png", "keep.png"):
            open(os.path.join(out, name), "w").close()
        done = self.shoot("en-GB", "Leeds", out)
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(["011.png", "012.png", "013.png", "014.png"],
                         [s for s in self.shots(out) if s[0].isdigit() and s >= "011"])

    def test_a_city_is_typed_as_the_one_ascii_token_the_apps_search_matches(self) -> None:
        expected = {"pl": {"Kraków": "krakow", "Wrocław": "wroclaw", "Łódź": "lodz"},
                    "de": {"München": "nchen", "Köln": "ln"},
                    "uk": {"Newcastle upon Tyne": "newcastle", "Bishop's Stortford": "stortford",
                           "Manchester": "manchester"}}
        for country, names in expected.items():
            for rank, name in enumerate(names):
                self.city(country, name, films=10 - rank)
        for country, names in expected.items():
            self.fakes.clear()
            done = self.shoot("--country-top", country, str(len(names)), env={"NO_OPEN": "1"})
            self.assertEqual(0, done.code, done.output)
            self.assertEqual(list(names.values()), self.typed(), done.output)
            self.assertIn(f"{len(names)}/{len(names)} cities from rank 1", done.stdout)

    def test_the_help_is_the_whole_header_and_stops_at_the_code(self) -> None:
        for args in (["--help"], ["-h"], []):
            done = self.shoot(*args)
            self.assertEqual(0, done.code, done.output)
            for text in ("--all-top", "--all-top 2 4", "--country-top", "ONE capture run at a time",
                         "re-boots it on the same port", "SKIPPED, not fatal", "EMULATORS", "NO_OPEN"):
                self.assertIn(text, done.stdout)
            self.assertNotIn("set -euo", done.stdout)
            self.assertEqual([], self.adb())

    def test_a_missing_avd_is_named(self) -> None:
        self.app["running"] = []
        self.city("uk", "Leeds")
        done = self.shoot("en-GB", "Leeds", env={"AVD": "nope"})
        self.assertEqual(1, done.code, done.output)
        self.assertIn("AVD 'nope' not found", done.stderr)

    def test_a_cold_emulator_is_booted_with_private_dns_off_and_shut_down_after(self) -> None:
        self.app["running"] = []
        self.city("uk", "Leeds")
        done = self.shoot("en-GB", "Leeds", self.path("out"))
        self.assertEqual(0, done.code, done.output)
        self.assertIn(["-avd", "kinowo_xl", "-no-snapshot-load", "-no-boot-anim", "-netdelay", "none", "-netspeed",
                       "full"], self.fakes.calls("emulator"))
        self.assertIn(["shell", "settings", "put", "global", "private_dns_mode", "off"], self.adb())
        self.assertIn(["-s", "emulator-5554", "emu", "kill"], self.adb())

    def test_an_emulator_already_running_is_reused_and_left_running(self) -> None:
        self.city("uk", "Leeds")
        done = self.shoot("en-GB", "Leeds", self.path("out"))
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([], [c for c in self.fakes.calls("emulator") if c != ["-list-avds"]])
        self.assertEqual([], [c for c in self.adb() if c[-2:] == ["emu", "kill"]])
        self.assertEqual([], self.fakes.calls("gradlew"))

    def test_build_1_reinstalls_the_app(self) -> None:
        self.city("uk", "Leeds")
        done = self.shoot("en-GB", "Leeds", self.path("out"), env={"BUILD": "1"})
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([[":app:installDebug"]], self.fakes.calls("gradlew"))
        with open(self.path("layout/android/local.properties")) as f:
            self.assertIn("sdk.dir=", f.read())

    def test_a_single_capture_opens_its_four_shots_unless_no_open(self) -> None:
        self.city("uk", "Leeds")
        out = self.path("out")
        done = self.shoot("en-GB", "Leeds", out)
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([["-a", "Preview"] + [os.path.join(out, f"00{n}.png") for n in range(1, 5)]],
                         self.fakes.calls("open"))
        self.fakes.clear()
        self.assertEqual(0, self.shoot("en-GB", "Leeds", out, env={"NO_OPEN": "1"}).code)
        self.assertEqual([], self.fakes.calls("open"))


class ScreensTest(AndroidCase):
    def calls_after_the_city_tap(self) -> List[str]:
        calls = [" ".join(c) for c in self.adb()]
        start = max(i for i, c in enumerate(calls) if c == "shell input tap 650 640")
        end = next(i for i, c in enumerate(calls) if i > start and c.startswith("shell dumpsys"))
        return calls[start + 1:end]

    def test_a_flat_city_never_waits_for_a_picker(self) -> None:
        self.city("uk", "Manchester")
        done = self.shoot("en-GB", "Manchester", self.path("out"))
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([], [c for c in self.calls_after_the_city_tap() if "uiautomator" in c])
        self.assertNotIn(["shell", "input", "tap", "500", "900"], self.adb())

    def test_a_split_city_whose_picker_never_shows_fails_loudly(self) -> None:
        self.city("uk", "London", areas=5, no_picker=True)
        done = self.shoot("en-GB", "London", self.path("out"))
        self.assertEqual(1, done.code, done.output)
        self.assertIn("London is split into 5 areas but its picker never appeared", done.stderr)
        self.assertEqual([], self.shots(self.path("out")))

    def test_an_unreachable_area_count_does_not_fail_the_city(self) -> None:
        self.city("uk", "London", cinemas_broken=True)
        done = self.shoot("en-GB", "London", self.path("out"))
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(4, len(self.shots(self.path("out"))))

    def test_a_picker_that_will_not_dismiss_is_tapped_twice_then_fails(self) -> None:
        self.city("uk", "London", areas=5, stuck=True)
        done = self.shoot("en-GB", "London", self.path("out"))
        self.assertEqual(1, done.code, done.output)
        self.assertIn("London's area picker would not dismiss — 'Show listings' is still on screen", done.stderr)
        self.assertEqual(2, self.adb().count(["shell", "input", "tap", "500", "900"]))

    def test_a_city_falling_out_of_the_app_fails(self) -> None:
        self.city("uk", "Leeds", browser=True)
        done = self.shoot("en-GB", "Leeds", self.path("out"))
        self.assertEqual(1, done.code, done.output)
        self.assertIn("app fell out of foreground", done.stderr)

    def test_an_empty_semantics_dump_is_retried_rather_than_read_as_a_blank_screen(self) -> None:
        # the second dump after the switch is the search field's, read once: an empty one would strand the run
        self.app["empty_dump_after_switch"] = 2
        self.city("uk", "Leeds")
        done = self.shoot("en-GB", "Leeds", self.path("out"))
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(4, len(self.shots(self.path("out"))))

    def scrolled(self, late: int) -> cli_harness.Done:
        self.app.update(hidden=["us", "es"], late=late)
        self.city("es", "Madrid")
        return self.shoot("--country-top", "es", "1", env={"NO_OPEN": "1"})

    def test_an_off_screen_pill_is_scrolled_into_view_at_the_rows_height_within_the_width(self) -> None:
        done = self.scrolled(1)
        self.assertEqual(0, done.code, done.output)
        self.assertIn("1/1 cities from rank 1", done.stdout)
        swipes = [c for c in self.adb() if c[-7:-5] == ["input", "swipe"]]
        self.assertEqual(1, len(swipes), done.output)
        x1, y1, x2, y2, ms = (int(v) for v in swipes[0][-5:])
        self.assertEqual((234, 234, 300), (y1, y2, ms))
        self.assertTrue(0 < x2 < x1 < 1344, swipes[0])

    def test_it_keeps_scrolling_while_the_catalog_is_still_loading(self) -> None:
        done = self.scrolled(9)
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(9, len([c for c in self.adb() if c[-7:-5] == ["input", "swipe"]]))

    def test_a_country_absent_from_the_row_fails_the_city(self) -> None:
        self.app["absent"] = ["es"]
        done = self.scrolled(1)
        self.assertIn("'España' is not in the country row, even after scrolling it", done.output)
        self.assertIn("es-ES: NOTHING captured", done.output)


class PoolTest(AndroidCase):
    def top(self, *args: str, **env: str) -> cli_harness.Done:
        return self.shoot(*args, env={"NO_OPEN": "1", **env})

    def test_one_read_only_emulator_per_country_on_its_own_port(self) -> None:
        done = self.top("--all-top", "1", COUNTRIES="pl uk de", EMULATORS="3")
        self.assertEqual(0, done.code, done.output)
        booted = [c for c in self.fakes.calls("emulator") if c != ["-list-avds"]]
        # launched in port order, but each in the background: the fakes log in whatever order they start
        self.assertEqual(["5554", "5556", "5558"], sorted(c[c.index("-port") + 1] for c in booted))
        self.assertTrue(all("-read-only" in c for c in booted), booted)
        for serial in ("emulator-5554", "emulator-5556", "emulator-5558"):
            self.assertIn(["-s", serial, "shell", "settings", "put", "global", "private_dns_mode", "off"], self.adb())
            self.assertIn(["-s", serial, "install", "-r",
                           self.path("layout/android/app/build/outputs/apk/debug/app-debug.apk")], self.adb())
            self.assertIn(["-s", serial, "emu", "kill"], self.adb())
        self.assertEqual([[":app:assembleDebug"]], self.fakes.calls("gradlew"))
        self.assertIn(["-f", "qemu-system.*kinowo_xl"], self.fakes.calls("pkill"))

    def test_countries_are_dealt_round_robin_each_exactly_once(self) -> None:
        for k, workers in (("3", ["pl", "uk", "de"]), ("2", ["pl de", "uk"]), ("1", ["pl uk de"])):
            done = self.top("--all-top", "1", COUNTRIES="pl uk de", EMULATORS=k)
            self.assertEqual(0, done.code, done.output)
            for w, slice_ in enumerate(workers):
                self.assertRegex(done.stdout, rf"worker {w} → emulator-{5554 + 2 * w} → {slice_} ?\n")
            for locale in ("pl-PL", "en-GB", "de-DE"):
                self.assertEqual(1, done.output.count(f"{locale}: NOTHING captured"), done.output)

    def test_the_emulator_count_is_clamped_to_the_countries(self) -> None:
        for asked, used in (("9", "3"), ("2", "2"), ("0", "1"), ("xx", "1")):
            done = self.top("--all-top", "1", COUNTRIES="pl uk de", EMULATORS=asked)
            self.assertEqual(0, done.code, done.output)
            self.assertIn(f"1 cities from rank 1 × 3 countries on {used} emulator(s)", done.stdout)

    def test_a_pool_emulator_that_never_boots_is_still_shut_down(self) -> None:
        self.app["never_boots"] = ["emulator-5554"]
        done = self.top("--country-top", "uk", "1")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("emulator-5554 didn't finish booting", done.stderr)
        self.assertIn(["-s", "emulator-5554", "emu", "kill"], self.adb())

    def test_a_closed_emulator_is_rebooted_on_its_own_port_and_the_app_reinstalled(self) -> None:
        self.app["dies"] = "emulator-5554"
        self.city("uk", "Leeds", films=9)
        self.city("uk", "York", films=8)
        done = self.top("--country-top", "uk", "2")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("emulator-5554 is gone (closed or killed) — bringing it back", done.output)
        booted = [c for c in self.fakes.calls("emulator") if c != ["-list-avds"]]
        self.assertEqual(2, len(booted))
        self.assertTrue(all(c[c.index("-port") + 1] == "5554" and "-read-only" in c for c in booted), booted)
        apk = self.path("layout/android/app/build/outputs/apk/debug/app-debug.apk")
        self.assertEqual(2, self.adb().count(["-s", "emulator-5554", "install", "-r", apk]))
        self.assertIn("en-GB: 2/2 cities from rank 1", done.stdout)

    def test_a_live_emulator_is_not_rebooted_between_cities(self) -> None:
        self.city("uk", "Leeds", films=9)
        self.city("uk", "York", films=8)
        done = self.top("--country-top", "uk", "2")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(1, len([c for c in self.fakes.calls("emulator") if c != ["-list-avds"]]))
        self.assertEqual(8, len(self.shots(self.candidates("en-GB"))))

    def test_a_failed_city_is_skipped_and_named_and_the_next_one_numbered_on(self) -> None:
        self.city("uk", "CityOne", films=9)
        self.city("uk", "BadCity", films=8, browser=True)
        self.city("uk", "CityThree", films=7)
        done = self.top("--country-top", "uk", "3")
        self.assertEqual(0, done.code, done.output)
        for name in ("CityOne", "BadCity", "CityThree"):
            self.assertIn(f"en-GB · {name}", done.stdout)
        self.assertIn("en-GB: 2/3 cities from rank 1", done.stdout)
        self.assertIn("en-GB: failed on BadCity", done.output)
        self.assertEqual([f"{n:03d}.png" for n in range(1, 9)], self.shots(self.candidates("en-GB")))

    def test_every_city_failing_is_said_out_loud_after_trying_each(self) -> None:
        for films, name in enumerate(("One", "Two", "Three")):
            self.city("uk", name, films=9 - films, browser=True)
        done = self.top("--country-top", "uk", "3")
        self.assertIn("en-GB: NOTHING captured — all 3 cities failed", done.output)
        self.assertEqual(3, done.stdout.count("en-GB · "))

    def test_country_top_narrows_to_one_country_from_the_start_rank(self) -> None:
        for films, name in enumerate(("A", "B", "C", "D", "E")):
            self.city("de", name, films=9 - films)
        self.city("uk", "Leeds")
        done = self.top("--country-top", "de", "2", "3")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("2 cities from rank 3 × 1 countries on 1 emulator(s)", done.stdout)
        self.assertEqual(["c", "d"], self.typed())
        self.assertIn("de-DE: 2/2 cities from rank 3", done.stdout)
        self.assertEqual([], self.shots(self.candidates("en-GB")))

    def test_country_top_refuses_an_unknown_or_missing_country(self) -> None:
        for args in (["--country-top", "fr", "2"], ["--country-top"]):
            done = self.top(*args)
            self.assertEqual(1, done.code, done.output)
            self.assertIn("--country-top wants a country", done.stderr)

    def test_all_top_refuses_a_bad_count_or_start_rank_and_warns_past_eight_shots(self) -> None:
        self.assertIn("--all-top wants a city count", self.top("--all-top", "0").stderr)
        self.assertIn("start rank is 1-based", self.top("--all-top", "2", "0").stderr)
        done = self.top("--all-top", "3", COUNTRIES="pl")
        self.assertIn("3 cities × 4 screens = 12 shots per locale; Play publishes at most 8", done.stderr)

    def test_the_closing_preview_opens_only_this_runs_shots(self) -> None:
        self.city("uk", "Leeds", films=9)
        self.city("uk", "York", films=8)
        os.makedirs(self.candidates("en-GB"))
        for n in range(1, 5):
            open(os.path.join(self.candidates("en-GB"), f"{n:03d}.png"), "w").close()
        done = self.shoot("--country-top", "uk", "2")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([["-a", "Preview"] + [os.path.join(self.candidates("en-GB"), f"{n:03d}.png")
                                               for n in range(5, 13)]], self.fakes.calls("open"))

    def test_the_preview_stays_shut_when_the_run_added_nothing(self) -> None:
        self.city("uk", "Leeds", browser=True)
        self.shoot("--country-top", "uk", "1")
        self.assertEqual([], self.fakes.calls("open"))


class RankingTest(AndroidCase):
    def test_top_prints_the_ranking_by_absolute_rank_from_the_start_rank(self) -> None:
        for n in range(12):
            self.city("uk", f"Town{n:02d}", films=40 - n)
        done = self.shoot("--top", "uk", "5", "11")
        self.assertEqual(0, done.code, done.output)
        lines = done.stdout.splitlines()
        self.assertEqual("5 uk cities from rank 11 by live film count (12 total):", lines[0])
        self.assertEqual(["   11.   30  town10                 Town10", "   12.   29  town11                 Town11"],
                         lines[1:])
        self.assertEqual([], self.adb())

    def test_top_starts_at_rank_one_by_default_and_refuses_rank_zero(self) -> None:
        self.city("uk", "Leeds", films=3)
        self.city("uk", "York", films=7)
        done = self.shoot("--top", "uk", "5")
        self.assertEqual("5 uk cities from rank 1 by live film count (2 total):", done.stdout.splitlines()[0])
        self.assertEqual(["    1.    7  york                   York", "    2.    3  leeds                  Leeds"],
                         done.stdout.splitlines()[1:])
        bad = self.shoot("--top", "uk", "5", "0")
        self.assertEqual(1, bad.code, bad.output)
        self.assertIn("--top's start rank is 1-based", bad.stderr)

    def test_top_refuses_an_unknown_country(self) -> None:
        done = self.shoot("--top", "fr", "5")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("unknown country 'fr'", done.stderr)


class LockTest(AndroidCase):
    LOCK = "kinowo-screenshots-android.lock"

    def hold(self, pid: int) -> None:
        os.makedirs(self.path(self.LOCK))
        with open(self.path(self.LOCK, "pid"), "w") as f:
            f.write(f"{pid}\n")

    def test_every_capture_path_is_refused_while_a_live_run_holds_the_lock(self) -> None:
        self.hold(os.getpid())
        for args in (["en-GB", "Leeds"], ["--all-top", "1"], ["--country-top", "uk", "1"]):
            done = self.shoot(*args)
            self.assertEqual(1, done.code, done.output)
            self.assertIn(f"another store-screenshots run (pid {os.getpid()}) is already driving the android devices",
                          done.stderr)
        self.assertEqual([], self.adb())

    def test_top_does_not_take_the_lock(self) -> None:
        self.hold(os.getpid())
        self.city("uk", "Leeds")
        self.assertEqual(0, self.shoot("--top", "uk", "1").code)

    def test_a_stale_lock_is_cleared_and_said_and_a_run_releases_its_own(self) -> None:
        self.hold(999999)
        self.city("uk", "Leeds")
        done = self.shoot("en-GB", "Leeds", self.path("out"))
        self.assertEqual(0, done.code, done.output)
        self.assertIn("clearing a stale android lock left by pid 999999", done.stderr)
        self.assertFalse(os.path.exists(self.path(self.LOCK)))
        failed = self.shoot("fr-FR", "Paris")
        self.assertEqual(1, failed.code)
        self.assertFalse(os.path.exists(self.path(self.LOCK)))

    def test_a_second_process_is_refused_while_the_first_runs_and_takes_over_once_it_is_killed(self) -> None:
        self.city("uk", "Leeds")
        gate = self.path("launch")
        self.app["block_launch"] = gate
        with open(self.path("app.json"), "w") as f:
            json.dump(self.app, f)
        holder = subprocess.Popen([self.tool, "en-GB", "Leeds", self.path("out")], env=self.env(self.tool_env()),
                                  cwd=self.work, stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL,
                                  stderr=subprocess.DEVNULL, start_new_session=True)
        self.addCleanup(kill_group, holder)
        self.assertTrue(cli_harness.wait_until(lambda: os.path.exists(gate + ".reached")))
        second = self.shoot("en-GB", "Leeds", self.path("out2"))
        self.assertEqual(1, second.code, second.output)
        self.assertIn(f"(pid {holder.pid})", second.stderr)
        self.assertIsNone(holder.poll())
        kill_group(holder)
        open(gate, "w").close()
        third = self.shoot("en-GB", "Leeds", self.path("out3"))
        self.assertEqual(0, third.code, third.output)
        self.assertIn(f"clearing a stale android lock left by pid {holder.pid}", third.stderr)


def slug_of(name: str) -> str:
    """The backend's ASCII slug of a city name."""
    folded = unicodedata.normalize("NFD", name.replace("ł", "l").replace("Ł", "L"))
    return "".join(c for c in folded.lower() if c.isascii() and (c.isalnum() or c == " ")).replace(" ", "-")


def kill_group(proc: "subprocess.Popen[bytes]") -> None:
    if proc.poll() is None:
        os.killpg(proc.pid, signal.SIGKILL)
        proc.wait()


class ListingLayoutTest(unittest.TestCase):
    """What gradle-play-publisher publishes: `/listings/*/graphics/<ImageType.dirName>/*`, one segment deep."""
    GPP_DIRNAMES = ("icon", "feature-graphic", "phone-screenshots", "tablet-screenshots", "large-tablet-screenshots",
                    "tv-banner", "tv-screenshots", "wear-screenshots")

    def test_every_graphics_dir_in_the_repository_is_a_gpp_dir_name(self) -> None:
        root = os.path.join(cli_harness.ROOT, LISTINGS)
        found = {d for locale in os.listdir(root) if os.path.isdir(os.path.join(root, locale, "graphics"))
                 for d in os.listdir(os.path.join(root, locale, "graphics"))
                 if os.path.isdir(os.path.join(root, locale, "graphics", d))}
        self.assertEqual(set(), found - set(self.GPP_DIRNAMES))

    def test_the_gitignore_publishes_the_gpp_dir_and_keeps_candidates_out(self) -> None:
        with open(os.path.join(cli_harness.ROOT, "android/.gitignore")) as f:
            lines = f.read().splitlines()
        self.assertIn("!app/src/main/play/listings/*/graphics/phone-screenshots/", lines)
        self.assertIn("app/src/main/play/listings/*/graphics/phone-screenshots/candidates/", lines)
        self.assertEqual([], [x for x in lines if "phoneScreenshots" in x and not x.lstrip().startswith("#")])


if __name__ == "__main__":
    unittest.main()
