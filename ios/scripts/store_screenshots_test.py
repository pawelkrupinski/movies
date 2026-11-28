#!/usr/bin/env python3
"""ios/scripts/store-screenshots.sh, black-box, over a scratch copy of its layout: a fake xcrun (simctl's device list,
boot, install, launch and screenshot), a fake xcodebuild and `open`, and a backend of the test's own
(scripts/lib/mobile/store_screenshots_fixture.py). Nothing reaches a simulator, a store or the live backend. Ported from the
shell test case for case, with the shared engine's numbering cases (the Android test covers the rest of it); the same
cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import json
import os
import shutil
import sys
import unittest
from typing import Dict, List, Optional

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "..", "scripts", "lib"))
from harness import cli_harness  # noqa: E402
from mobile.store_screenshots_fixture import Backend, City, copy_layout, png  # noqa: E402

TOOL = cli_harness.tool_path("ios/scripts/store_screenshots.py", "ios/scripts/store-screenshots.sh")
PHONE, TABLET, TABLET_11 = "iPhone 17 Pro Max", "iPad Pro 13-inch (M5)", "iPad Pro 11-inch (M5)"
UDIDS = {PHONE: "PHONE-UDID", TABLET: "TABLET-UDID", TABLET_11: "TABLET11-UDID"}
# simctl, as the driver uses it; FAKE_SIMS holds the devices (and their state), FAKE_LAUNCHES each launch's hooks
XCRUN = r'''
sims = json.load(open(env["FAKE_SIMS"]))
devices = [d for runtime in sims["devices"].values() for d in runtime]
def save():
    json.dump(sims, open(env["FAKE_SIMS"], "w"))
def device(udid):
    return next(d for d in devices if d["udid"] == udid)
sub = argv[1] if len(argv) > 1 else ""
if argv[:2] == ["simctl", "list"]:
    out(json.dumps(sims))
elif sub == "boot":
    device(argv[2])["state"] = "Booted"
    save()
elif sub == "shutdown":
    device(argv[2])["state"] = "Shutdown"
    save()
elif sub == "launch":
    hooks = {k[len("SIMCTL_CHILD_"):]: v for k, v in env.items() if k.startswith("SIMCTL_CHILD_")}
    with open(env["FAKE_LAUNCHES"], "a") as f:
        f.write(json.dumps({"udid": argv[2], "bundle": argv[3], "args": argv[4:], "hooks": hooks}) + "\n")
elif sub == "io":
    launches = [json.loads(x) for x in open(env["FAKE_LAUNCHES"])]
    link = launches[-1]["hooks"].get("KINOWO_UITEST_DEEPLINK", "")
    if "badcity" in link:
        err("An error was encountered processing the command (code=60)")
        exit(1)
    with open(argv[4], "wb") as f:
        f.write(open(env["FAKE_FRAME"], "rb").read())
'''
XCODEBUILD = r'''
if env.get("FAKE_XCODEBUILD_FAILS"):
    exit(65)
app = os.path.join(argv[argv.index("-derivedDataPath") + 1], "Build/Products/Debug-iphonesimulator/Kinowo.app")
os.makedirs(app, exist_ok=True)
with open(os.path.join(app, "Kinowo"), "w") as f:
    f.write("binary")
os.chmod(os.path.join(app, "Kinowo"), 0o755)
'''
OLD = 1767225600  # 2026-01-01, before any build a case makes


class IosCase(cli_harness.ToolCase):
    def setUp(self) -> None:
        super().setUp()
        self.layout = self.path("layout")
        copy_layout(self.layout)
        self.tool = os.path.join(self.layout, os.path.relpath(TOOL, cli_harness.ROOT))
        self.backend = Backend()
        self.base = self.backend.start()
        self.addCleanup(self.backend.stop)
        self.sims: List[Dict[str, str]] = [{"name": n, "udid": u, "state": "Shutdown"} for n, u in UDIDS.items()]
        self.fakes.program("xcrun", XCRUN)
        self.fakes.program("xcodebuild", XCODEBUILD)
        self.fakes.script("open", ":")
        with open(self.path("frame.png"), "wb") as f:
            f.write(png(1320, 2868, 1000))
        open(self.path("launches.jsonl"), "w").close()

    def city(self, country: str, slug: str, name: str, films: Optional[List[str]] = None) -> None:
        self.backend.add(country, City(slug, name, films=["Dune"] if films is None else films))

    def shoot(self, *args: str, env: Optional[Dict[str, Optional[str]]] = None) -> cli_harness.Done:
        with open(self.path("sims.json"), "w") as f:
            json.dump({"devices": {"com.apple.CoreSimulator.SimRuntime.iOS-26-0": self.sims}}, f)
        open(self.path("launches.jsonl"), "w").close()
        return self.run_tool(*args, tool=self.tool, env={**self.tool_env(), **(env or {})})

    def tool_env(self) -> Dict[str, Optional[str]]:
        return {"TMPDIR": self.work, "LOCK_ROOT": None, "STORE_API_BASE": self.base, "STORE_NAPS": "0",
                "FAKE_SIMS": self.path("sims.json"), "FAKE_LAUNCHES": self.path("launches.jsonl"),
                "FAKE_FRAME": self.path("frame.png"), "SHOT_CLASS": None, "SHOT_CLASSES": None, "IOS_PHONE": None,
                "IOS_TABLET": None, "IOS_TABLET_11": None, "COUNTRIES": None, "NO_OPEN": "1", "BUILD": None,
                "SETTLE": None}

    def launches(self) -> List[Dict[str, object]]:
        with open(self.path("launches.jsonl")) as f:
            return [json.loads(line) for line in f]

    def links(self) -> List[str]:
        return [str(x["hooks"]["KINOWO_UITEST_DEEPLINK"]) for x in self.launches()]  # type: ignore[index]

    def launched_on(self) -> List[str]:
        """Each run of launches' device, once per run of them."""
        seen: List[str] = []
        for launch in self.launches():
            if not seen or seen[-1] != launch["udid"]:
                seen.append(str(launch["udid"]))
        return seen

    def candidates(self, locale: str, shot_class: str = "phone-screenshots") -> str:
        return os.path.join(self.layout, "ios/store/listings", locale, "graphics", shot_class, "candidates")

    def shots(self, folder: str) -> List[str]:
        return sorted(os.listdir(folder)) if os.path.isdir(folder) else []

    def builds(self) -> int:
        return len(self.fakes.calls("xcodebuild"))

    def app_binary(self) -> str:
        return self.path("kinowo-shots-dd/Build/Products/Debug-iphonesimulator/Kinowo.app/Kinowo")


FIVE = [f"{n:03d}.png" for n in range(1, 6)]


class ClassesTest(IosCase):
    def setUp(self) -> None:
        super().setUp()
        self.city("de", "berlin", "Berlin")

    def test_every_run_shoots_the_phone_then_the_13_inch_ipad_each_in_its_own_dir(self) -> None:
        done = self.shoot("de-DE", "Berlin")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(FIVE, self.shots(self.candidates("de-DE")))
        self.assertEqual(FIVE, self.shots(self.candidates("de-DE", "tablet-screenshots")))
        self.assertEqual([], self.shots(self.candidates("de-DE", "tablet-11-screenshots")))
        self.assertEqual([UDIDS[PHONE], UDIDS[TABLET]], self.launched_on())
        self.assertLess(done.stdout.index(f"▪ phone-screenshots on {PHONE}"),
                        done.stdout.index(f"▪ tablet-screenshots on {TABLET}"))
        self.assertIn("wrote " + self.candidates("de-DE", "tablet-screenshots") + "/{001..005}.png", done.stdout)

    def test_shot_classes_names_the_passes_and_shot_class_pins_one(self) -> None:
        for env, dirs, udids in (({"SHOT_CLASSES": "tablet-11-screenshots"}, ["tablet-11-screenshots"], [TABLET_11]),
                                 ({"SHOT_CLASS": "tablet-screenshots"}, ["tablet-screenshots"], [TABLET]),
                                 ({"SHOT_CLASSES": "phone-screenshots"}, ["phone-screenshots"], [PHONE]),
                                 ({"SHOT_CLASS": "whatever"}, ["whatever"], [PHONE])):
            done = self.shoot("de-DE", "Berlin", env=dict(env))
            self.assertEqual(0, done.code, done.output)
            self.assertEqual([UDIDS[u] for u in udids], self.launched_on(), env)
            for shot_class in dirs:
                self.assertEqual(FIVE, self.shots(self.candidates("de-DE", shot_class)), env)
            shutil.rmtree(os.path.join(self.layout, "ios/store/listings"), ignore_errors=True)

    def test_a_leading_device_flag_narrows_every_command_to_that_device(self) -> None:
        for flag, shot_class, name in (("--iphone", "phone-screenshots", PHONE),
                                       ("--ipad-13", "tablet-screenshots", TABLET),
                                       ("--ipad-11", "tablet-11-screenshots", TABLET_11)):
            for command in (["de-DE", "Berlin"], ["--all-top", "1"], ["--country-top", "de", "1"]):
                done = self.shoot(flag, *command, env={"COUNTRIES": "de"})
                self.assertEqual(0, done.code, done.output)
                self.assertEqual([UDIDS[name]], self.launched_on(), (flag, command))
                self.assertEqual(1, done.stdout.count("▪ "), done.output)
                self.assertIn(f"▪ {shot_class} on {name}", done.stdout)

    def test_every_capture_command_runs_once_per_class(self) -> None:
        for command in (["de-DE", "Berlin"], ["--all-top", "1"], ["--country-top", "de", "1"]):
            done = self.shoot(*command, env={"COUNTRIES": "de"})
            self.assertEqual(0, done.code, done.output)
            self.assertEqual([UDIDS[PHONE], UDIDS[TABLET]], self.launched_on(), command)

    def test_the_device_names_come_from_the_environment(self) -> None:
        self.sims.append({"name": "iPhone SE", "udid": "SE-UDID", "state": "Shutdown"})
        done = self.shoot("--iphone", "de-DE", "Berlin", env={"IOS_PHONE": "iPhone SE"})
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(["SE-UDID"], self.launched_on())

    def test_a_missing_simulator_is_named(self) -> None:
        done = self.shoot("--iphone", "de-DE", "Berlin", env={"IOS_PHONE": "iPhone 1"})
        self.assertEqual(1, done.code, done.output)
        self.assertIn("no simulator named 'iPhone 1'", done.stderr)

    def test_a_booted_simulator_is_preferred_and_only_what_the_run_booted_is_shut_down(self) -> None:
        self.sims.insert(0, {"name": TABLET, "udid": "OTHER-TABLET", "state": "Shutdown"})
        self.sims[2]["state"] = "Booted"
        done = self.shoot("de-DE", "Berlin")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([UDIDS[PHONE], UDIDS[TABLET]], self.launched_on())
        calls = self.fakes.calls("xcrun")
        self.assertIn(["simctl", "boot", UDIDS[PHONE]], calls)
        self.assertNotIn(["simctl", "boot", UDIDS[TABLET]], calls)
        self.assertIn(["simctl", "shutdown", UDIDS[PHONE]], calls)
        self.assertNotIn(["simctl", "shutdown", UDIDS[TABLET]], calls)
        self.assertIn(["simctl", "status_bar", UDIDS[PHONE], "override", "--time", "9:41", "--batteryState",
                       "charged", "--batteryLevel", "100", "--cellularBars", "4", "--wifiBars", "3"], calls)
        self.assertIn(["simctl", "privacy", UDIDS[PHONE], "grant", "location", "dev.kinowo.Kinowo"], calls)

    def test_a_simulator_booted_for_a_ranked_run_is_shut_down_too(self) -> None:
        done = self.shoot("--country-top", "de", "1")
        self.assertEqual(0, done.code, done.output)
        calls = self.fakes.calls("xcrun")
        for udid in (UDIDS[PHONE], UDIDS[TABLET]):
            self.assertEqual(1, calls.count(["simctl", "boot", udid]))
            self.assertEqual(1, calls.count(["simctl", "shutdown", udid]))


class ScreensTest(IosCase):
    def test_five_deep_links_in_store_order_the_filtry_sheet_over_the_last(self) -> None:
        self.city("uk", "london", "London", films=["Toy Story 5", "Dune"])
        done = self.shoot("--iphone", "en-GB", "London")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(["kinowo://london", "kinowo://london?sort=rating", "kinowo://london/film?title=Toy%20Story%205",
                          "kinowo://london?date=tomorrow", "kinowo://london"], self.links())
        self.assertEqual([False, False, False, False, True],
                         ["KINOWO_UITEST_OPEN_FILTERS" in x["hooks"] for x in self.launches()])  # type: ignore[operator]
        self.assertEqual(["-areaPickerSeenCities", '("london")', "-selectedCountryCode", "uk", "-AppleLanguages",
                          "(en)", "-AppleLocale", "en"], self.launches()[0]["args"])
        self.assertEqual({"dev.kinowo.Kinowo"}, {x["bundle"] for x in self.launches()})

    def test_every_launch_is_pinned_to_the_countrys_bundle_language(self) -> None:
        for country, locale, slug, language in (("de", "de-DE", "koeln", "de"), ("pl", "pl-PL", "poznan", "pl"),
                                                ("us", "en-US", "chicago", "en"), ("es", "es-ES", "madrid", "es")):
            self.city(country, slug, slug.title())
            done = self.shoot("--iphone", locale, slug.title(), self.path("out", country))
            self.assertEqual(0, done.code, done.output)
            self.assertEqual({f"-selectedCountryCode {country} -AppleLanguages ({language}) -AppleLocale {language}"},
                             {" ".join(x["args"][2:]) for x in self.launches()})  # type: ignore[arg-type]

    def test_a_missing_film_still_shoots_five_screens_repeating_the_listing(self) -> None:
        self.city("uk", "london", "London", films=[])
        done = self.shoot("--iphone", "en-GB", "London", self.path("out"))
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("kinowo://london", self.links()[2])
        self.assertEqual(FIVE, self.shots(self.path("out")))
        self.assertIn("London: no film from the API — repeating the listing for shot 3", done.stderr)

    def test_a_city_is_found_by_its_folded_name_and_an_unknown_one_refused(self) -> None:
        self.city("pl", "wroclaw", "Wrocław")
        done = self.shoot("--iphone", "pl-PL", "wroclaw", self.path("out"))
        self.assertEqual(0, done.code, done.output)
        self.assertIn("pl-PL · wroclaw (wroclaw)", done.stdout)
        missing = self.shoot("--iphone", "pl-PL", "Atlantis", self.path("out"))
        self.assertEqual(1, missing.code, missing.output)
        self.assertIn("'Atlantis' isn't in pl's catalog — check the spelling.", missing.stderr)
        self.assertEqual(1, self.shoot("--iphone", "fr-FR", "Paris").code)

    def test_a_screen_that_never_rendered_fails_the_city(self) -> None:
        self.city("uk", "badcity", "BadCity")
        done = self.shoot("--iphone", "en-GB", "BadCity", self.path("out"))
        self.assertEqual(1, done.code, done.output)
        self.assertIn("BadCity: screen 1 never rendered", done.stderr)

    def test_a_single_capture_opens_its_five_shots_unless_no_open(self) -> None:
        self.city("uk", "leeds", "Leeds")
        out = self.path("out")
        done = self.shoot("--iphone", "en-GB", "Leeds", out, env={"NO_OPEN": None})
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([["-a", "Preview"] + [os.path.join(out, f) for f in FIVE]], self.fakes.calls("open"))


class NumberingTest(IosCase):
    def setUp(self) -> None:
        super().setUp()
        self.city("uk", "leeds", "Leeds")
        self.out = self.path("out")

    def after(self, *existing: str) -> List[str]:
        """The files one capture adds to an outdir already holding `existing`."""
        os.makedirs(self.out, exist_ok=True)
        for name in existing:
            open(os.path.join(self.out, name), "w").close()
        before = set(self.shots(self.out))
        done = self.shoot("--iphone", "en-GB", "Leeds", self.out)
        self.assertEqual(0, done.code, done.output)
        return sorted(set(self.shots(self.out)) - before)

    def test_an_empty_or_missing_dir_starts_at_one(self) -> None:
        done = self.shoot("--iphone", "en-GB", "Leeds", os.path.join(self.out, "nope"))
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(FIVE, self.shots(os.path.join(self.out, "nope")))
        self.assertEqual(FIVE, self.after())

    def test_a_block_of_five_continues_at_six_zero_padded(self) -> None:
        self.assertEqual(FIVE, self.after())
        self.assertEqual([f"{n:03d}.png" for n in range(6, 11)], self.after())

    def test_leading_zeros_are_base_ten_and_ten_counts_above_nine(self) -> None:
        self.assertEqual([f"{n:03d}.png" for n in range(10, 15)], self.after("008.png", "009.png"))
        self.clear()
        self.assertEqual([f"{n:03d}.png" for n in range(11, 16)], self.after("009.png", "010.png"))

    def test_legacy_unpadded_names_count_and_strays_do_not(self) -> None:
        self.assertEqual([f"{n:03d}.png" for n in range(13, 18)], self.after("12.png", "keep.png", "2b.png"))

    def clear(self) -> None:
        for name in self.shots(self.out):
            os.remove(os.path.join(self.out, name))


class BuildTest(IosCase):
    def setUp(self) -> None:
        super().setUp()
        self.city("uk", "leeds", "Leeds")

    def age_sources(self) -> None:
        for folder, _, names in os.walk(os.path.join(self.layout, "ios")):
            for name in names:
                os.utime(os.path.join(folder, name), (OLD, OLD))

    def built_app(self) -> None:
        os.makedirs(os.path.dirname(self.app_binary()))
        with open(self.app_binary(), "w") as f:
            f.write("binary")
        os.chmod(self.app_binary(), 0o755)
        os.utime(self.app_binary(), (OLD + 100, OLD + 100))
        self.age_sources()

    def test_a_missing_app_is_built_once_for_every_pass_and_city(self) -> None:
        done = self.shoot("en-GB", "Leeds")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(1, self.builds())
        args = self.fakes.calls("xcodebuild")[0]
        self.assertEqual(["-project", os.path.join(self.layout, "ios/Kinowo.xcodeproj"), "-scheme", "Kinowo",
                          "-configuration", "Debug", "-sdk", "iphonesimulator", "-derivedDataPath",
                          self.path("kinowo-shots-dd"), "CODE_SIGNING_ALLOWED=NO", "build"], args)

    def test_an_app_newer_than_every_source_is_reused(self) -> None:
        self.built_app()
        self.assertEqual(0, self.shoot("--iphone", "en-GB", "Leeds").code)
        self.assertEqual(0, self.builds())

    def test_a_source_edited_since_the_build_makes_it_stale(self) -> None:
        self.built_app()
        self.write("layout/ios/Kinowo/Edited.swift", "// edited\n")
        self.assertEqual(0, self.shoot("--iphone", "en-GB", "Leeds").code)
        self.assertEqual(1, self.builds())

    def test_screenshots_and_build_outputs_are_no_sources(self) -> None:
        self.built_app()
        for relative in ("layout/ios/store/listings/001.png", "layout/ios/build/artifact.o",
                         "layout/ios/.build/x.o", "layout/ios/DerivedData/x", "layout/ios/target/x"):
            self.write(relative, "new\n")
        self.assertEqual(0, self.shoot("--iphone", "en-GB", "Leeds").code)
        self.assertEqual(0, self.builds())

    def test_build_1_forces_a_rebuild(self) -> None:
        self.built_app()
        self.assertEqual(0, self.shoot("--iphone", "en-GB", "Leeds", env={"BUILD": "1"}).code)
        self.assertEqual(1, self.builds())

    def test_a_failed_build_fails_the_capture(self) -> None:
        done = self.shoot("--iphone", "en-GB", "Leeds", env={"FAKE_XCODEBUILD_FAILS": "1"})
        self.assertEqual(1, done.code, done.output)
        self.assertIn("xcodebuild failed", done.stderr)


class ManyCitiesTest(IosCase):
    def test_country_top_narrows_to_one_country_from_the_start_rank(self) -> None:
        for films, name in enumerate(("A", "B", "C", "D", "E")):
            self.city("de", name.lower(), name, films=[f"F{n}" for n in range(9 - films)])
        self.city("uk", "leeds", "Leeds")
        done = self.shoot("--iphone", "--country-top", "de", "2", "3")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("2 cities from rank 3 × 1 countries", done.stdout)
        self.assertEqual(["kinowo://c", "kinowo://d"], [x for x in self.links() if "?" not in x and "/film" not in x
                                                        ][::2])
        self.assertIn("de-DE: 2/2 cities from rank 3", done.stdout)
        self.assertEqual([], self.shots(self.candidates("en-GB")))

    def test_country_top_refuses_an_unknown_country(self) -> None:
        done = self.shoot("--country-top", "fr", "2")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("--country-top wants a country", done.stderr)

    def test_a_failed_city_is_skipped_named_and_numbered_past(self) -> None:
        for films, name in enumerate(("CityOne", "BadCity", "CityThree")):
            self.city("uk", name.lower(), name, films=[f"F{n}" for n in range(9 - films)])
        done = self.shoot("--iphone", "--country-top", "uk", "3")
        self.assertEqual(0, done.code, done.output)
        for name in ("CityOne", "BadCity", "CityThree"):
            self.assertIn(f"en-GB · {name}", done.stdout)
        self.assertIn("en-GB: 2/3 cities from rank 1", done.stdout)
        self.assertIn("en-GB: failed on BadCity", done.stderr)
        self.assertEqual([f"{n:03d}.png" for n in range(1, 11)], self.shots(self.candidates("en-GB")))

    def test_every_city_failing_is_said_out_loud_after_trying_each(self) -> None:
        for n in range(3):
            self.city("uk", f"badcity{n}", f"Bad{n}", films=[f"F{x}" for x in range(5 - n)])
        done = self.shoot("--iphone", "--country-top", "uk", "3")
        self.assertIn("en-GB: NOTHING captured — all 3 cities failed", done.stderr)
        self.assertEqual(3, done.stdout.count("en-GB · "))

    def test_all_top_refuses_a_bad_count_or_start_rank(self) -> None:
        self.assertIn("--all-top wants a city count", self.shoot("--all-top", "0").stderr)
        self.assertIn("start rank is 1-based", self.shoot("--all-top", "2", "0").stderr)

    def test_the_closing_preview_opens_only_this_runs_shots_per_class(self) -> None:
        self.city("uk", "leeds", "Leeds")
        for shot_class in ("phone-screenshots", "tablet-screenshots"):
            os.makedirs(self.candidates("en-GB", shot_class))
            for name in FIVE:
                open(os.path.join(self.candidates("en-GB", shot_class), name), "w").close()
        done = self.shoot("--country-top", "uk", "1", env={"NO_OPEN": None})
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([["-a", "Preview"] + [os.path.join(self.candidates("en-GB", c), f"{n:03d}.png")
                                               for n in range(6, 11)] for c in ("phone-screenshots",
                                                                                 "tablet-screenshots")],
                         self.fakes.calls("open"))

    def test_the_preview_stays_shut_when_the_run_added_nothing(self) -> None:
        self.city("uk", "badcity", "BadCity")
        self.shoot("--country-top", "uk", "1", env={"NO_OPEN": None})
        self.assertEqual([], self.fakes.calls("open"))


class CommandLineTest(IosCase):
    LOCK = "kinowo-screenshots-ios.lock"

    def test_the_help_is_the_whole_header_and_stops_at_the_code(self) -> None:
        for args in (["--help"], ["-h"], []):
            done = self.shoot(*args)
            self.assertEqual(0, done.code, done.output)
            for text in ("--country-top", "--all-top 2 4", "candidates/", "SHOT_CLASSES", "--iphone", "--ipad-13",
                         "--ipad-11", "NO_OPEN"):
                self.assertIn(text, done.stdout)
            self.assertIn("IPAD", done.stdout.upper())
            self.assertNotIn("set -euo", done.stdout)
            self.assertEqual([], self.fakes.calls("xcrun"))

    def test_top_prints_the_ranking_once_takes_no_lock_and_no_device(self) -> None:
        os.makedirs(self.path(self.LOCK))
        with open(self.path(self.LOCK, "pid"), "w") as f:
            f.write(f"{os.getpid()}\n")
        self.city("uk", "leeds", "Leeds", films=["A", "B"])
        done = self.shoot("--top", "uk", "3")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(["3 uk cities from rank 1 by live film count (1 total):",
                          "    1.    2  leeds                  Leeds"], done.stdout.splitlines())
        self.assertEqual([], self.fakes.calls("xcrun"))

    def test_every_capture_path_is_refused_while_a_live_run_holds_the_lock(self) -> None:
        os.makedirs(self.path(self.LOCK))
        with open(self.path(self.LOCK, "pid"), "w") as f:
            f.write(f"{os.getpid()}\n")
        for args in (["en-GB", "Leeds"], ["--all-top", "1"], ["--country-top", "uk", "1"], ["--ipad-13", "--all-top"]):
            done = self.shoot(*args)
            self.assertEqual(1, done.code, done.output)
            self.assertIn(f"another store-screenshots run (pid {os.getpid()}) is already driving the ios devices",
                          done.stderr)
        self.assertEqual([], self.fakes.calls("xcrun"))

    def test_a_run_releases_its_lock(self) -> None:
        self.city("uk", "leeds", "Leeds")
        self.assertEqual(0, self.shoot("--iphone", "en-GB", "Leeds").code)
        self.assertFalse(os.path.exists(self.path(self.LOCK)))


if __name__ == "__main__":
    unittest.main()
