#!/usr/bin/env python3
"""Generate App Store screenshots for the Kinowo / Showtimes iOS app. Just run it —
it builds once, boots the simulators, drives the five store screens per city and
writes them out per locale, for the PHONE and the IPAD.

  ios/scripts/store-screenshots.sh en-GB "Birmingham"      # → candidates dir
  ios/scripts/store-screenshots.sh pl-PL "Poznan" /tmp/x   # → a scratch dir (both passes append)
  ios/scripts/store-screenshots.sh --top uk 10             # biggest cities by film count
  ios/scripts/store-screenshots.sh --top uk 5 11           # ranks 11-15 instead of 1-5
  ios/scripts/store-screenshots.sh --all-top 2             # top 2 cities of EVERY country
  ios/scripts/store-screenshots.sh --all-top 2 4           # 2 cities from rank 4, every country
  ios/scripts/store-screenshots.sh --country-top uk 10     # top 10 cities of ONE country
  ios/scripts/store-screenshots.sh --country-top de 5 6    # de ranks 6-10 only

A leading DEVICE FLAG narrows the run to one class; it goes in front of any of
the commands above, and on its own means "just this device":

  ios/scripts/store-screenshots.sh --iphone --all-top 2          # 6.9" phone only
  ios/scripts/store-screenshots.sh --ipad-13 --all-top 2         # 13" iPad only
  ios/scripts/store-screenshots.sh --ipad-11 --country-top uk 5  # 11" iPad only
  ios/scripts/store-screenshots.sh --ipad-13 pl-PL "Poznan"      # one city, one device

--top only PRINTS the ranking; --all-top and --country-top capture. Use
--country-top to top up a single locale without re-shooting the other two.

All three take an optional 1-based START RANK after the count, so
"--all-top 2 4" means "two cities beginning at the 4th best" — that is how you
shoot the runners-up without re-shooting the cities you already have.

A city that fails is SKIPPED, not fatal: the rest of the country still gets shot
and the summary says which ones were lost.

Every run shoots the two classes the store REQUIRES, a full pass each: the
6.9" phone and the 13" iPad. App Store Connect requires an iPad set from any
app whose TARGETED_DEVICE_FAMILY includes iPad, and ours does; shooting phones
alone is how the first submission ended up with three stale, empty iPad sets
nobody noticed.

The 11" iPad is available but OPT-IN, since Apple derives it from the 13":
ask for it with --ipad-11. Worth doing when the derived image would mislead —
the listing lays out four columns at 11" and five at 13", so a squeezed 13"
shows the wrong grid.

Shots land in a candidates/ scratchpad INSIDE the published dir, one per class
(ios/store/listings/<locale>/graphics/<class>/candidates/, gitignored) and every
run APPENDS, zero-padded to three digits (city 1 → 001-005.png, city 2 →
006-010.png, …). Pick the keepers and move them up one level into
phone-screenshots/, tablet-screenshots/ or tablet-11-screenshots/ — that is what
goes to App Store Connect. The layout mirrors the Android side exactly, so one
promote-by-hand habit works for both stores.

Screens are reached by DEEP LINK (`kinowo://<slug>`, `…/film?title=…`), never by
tapping coordinates: the same script then works on a 4" phone and a 13" iPad
with no per-device tap map. The Filtry sheet is the one exception — it is app
state rather than a URL, so it rides a launch hook instead, still no taps.
That also makes SPLIT cities (London) a non-issue —
rather than dismissing the area sheet the way the Android driver must, the
launch pre-seeds `areaPickerSeenCities` so it never opens.

One build covers all three locales: the city's country drives both the backend
and the UI language, so pl-PL shoots Polish, en-GB English, de-DE German.

Only ONE capture run at a time: a second is refused with the holder's pid rather
than letting two runs fight over the same simulators. --top is exempt.

Env: IOS_PHONE / IOS_TABLET / IOS_TABLET_11 pick the devices · SHOT_CLASSES
names the passes ("phone-screenshots tablet-screenshots" by default; add
tablet-11-screenshots for the 11" iPad; SHOT_CLASS=<one> still pins a single
pass) · SETTLE=<s> per-screen wait (posters come off the network) · BUILD=1
force a rebuild · NO_OPEN=1 skip the Preview."""
from __future__ import annotations

import json
import os
import subprocess
import sys
import urllib.parse
from typing import Dict, List, Mapping, Optional, Sequence

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "..", "scripts", "lib"))
import machine  # noqa: E402
from store_screenshots_common import (COUNTRY, Lock, Output, Store, all_top_counts,  # noqa: E402
                                      capture_args, city_slug, cmd_top, country_base, countries_of, country_top,
                                      exit_code, get, naps, noise_file, open_preview, preview_all,
                                      run_worker, usage_of)

REPO_ROOT = os.path.dirname(os.path.dirname(HERE))
BUNDLE = "dev.kinowo.Kinowo"
# The class a leading device flag narrows the run to: named by DEVICE, since at the prompt you know which screen you
# are reshooting, not which directory it lands in. Every class has one.
FLAG_CLASS = {"--iphone": "phone-screenshots", "--ipad-13": "tablet-screenshots", "--ipad-11": "tablet-11-screenshots"}
# Directories pruned from the staleness check: the driver writes its screenshots under `store` mid-run, and build
# dirs are outputs, not sources.
NOT_SOURCES = {"build", ".build", "target", "store", "DerivedData"}


def capture_urls(slug: str, film: str) -> List[str]:
    """The five deep links one capture walks, in store order: listing, by rating, a film's detail (the listing again
    when there is no film), another day, and the plain listing the Filtry sheet opens over."""
    base = f"kinowo://{slug}"
    return [base, f"{base}?sort=rating", f"{base}/film?title={film}" if film else base, f"{base}?date=tomorrow", base]


def language_args(country: str) -> List[str]:
    """The launch arguments pinning the UI language to the country's at PROCESS START (UserDefaults' argument
    domain): iOS fixes the bundle's localization then, so a country arriving with the deep link is too late for
    `String(localized:)` -- shot 001 once came out with Polish pills over a German listing."""
    if country not in COUNTRY:
        return []
    language = COUNTRY[country].language
    return ["-selectedCountryCode", country, "-AppleLanguages", f"({language})", "-AppleLocale", language]


class Simulators:
    """simctl, and the devices this run booted (shut down on exit; one already open is reused and left open)."""

    def __init__(self, env: Mapping[str, str], output: Output, noise: str) -> None:
        self.env = env
        self.output = output
        self.noise = noise
        self.booted: List[str] = []
        self.derived = os.path.join(env.get("TMPDIR") or "/tmp", "kinowo-shots-dd")
        self.app = os.path.join(self.derived, "Build/Products/Debug-iphonesimulator/Kinowo.app")

    def run(self, argv: Sequence[str], env: Optional[Mapping[str, str]] = None) -> int:
        with open(self.noise, "ab") as noise:
            return subprocess.call(list(argv), stdout=noise, stderr=noise, stdin=subprocess.DEVNULL,
                                   env=dict(env or self.env))

    def simctl(self, *args: str) -> int:
        return self.run(["xcrun", "simctl", *args])

    def devices(self, *which: str) -> List[Dict[str, str]]:
        done = subprocess.run(["xcrun", "simctl", "list", "devices", *which, "-j"], capture_output=True, text=True,
                              env=dict(self.env))
        try:
            return [d for runtime in json.loads(done.stdout)["devices"].values() for d in runtime]
        except (ValueError, KeyError, AttributeError):
            return []

    def ensure_device(self, name: str) -> str:
        """The udid of an available simulator named `name`, a booted one preferred; booted if it is not."""
        hits = [d for d in self.devices("available") if d.get("name") == name]
        booted = [d for d in hits if d.get("state") == "Booted"]
        if not hits:
            raise self.output.die(f"no simulator named '{name}' — install it in Xcode, or set IOS_PHONE / IOS_TABLET.")
        udid = (booted or hits)[0]["udid"]
        state = next((d["state"] for d in self.devices() if d.get("udid") == udid), "")
        if state != "Booted":
            self.simctl("boot", udid)
            self.booted.append(udid)
            self.simctl("bootstatus", udid, "-b")
        return udid

    def shutdown(self) -> None:
        for udid in self.booted:
            self.simctl("shutdown", udid)

    def stale(self) -> bool:
        """Whether the cached app must be rebuilt: missing, BUILD=1, or a file under ios/ newer than its binary (a
        derived-data dir from before a change must not keep shooting the old app)."""
        binary = os.path.join(self.app, "Kinowo")
        if self.env.get("BUILD") or not os.access(binary, os.X_OK):
            return True
        built = os.path.getmtime(binary)
        for folder, dirs, files in os.walk(os.path.join(REPO_ROOT, "ios")):
            dirs[:] = [d for d in dirs if d not in NOT_SOURCES]
            if any(os.path.getmtime(os.path.join(folder, f)) > built for f in files if f not in NOT_SOURCES):
                return True
        return False

    def build(self) -> None:
        """Debug, not Release: the KINOWO_UITEST_DEEPLINK hook driving these screens is `#if DEBUG`."""
        if not self.stale():
            return
        self.output.step("building Kinowo (Debug, simulator)")
        if self.run(["xcodebuild", "-project", os.path.join(REPO_ROOT, "ios/Kinowo.xcodeproj"), "-scheme", "Kinowo",
                     "-configuration", "Debug", "-sdk", "iphonesimulator", "-derivedDataPath", self.derived,
                     "CODE_SIGNING_ALLOWED=NO", "build"]) != 0:
            raise self.output.die("xcodebuild failed")
        self.output.done()
        if not os.path.isdir(self.app):
            raise self.output.die(f"no app at {self.app} after a successful build")

    def install(self, udid: str) -> None:
        """The app installed, location pre-granted (no alert mid-shot) and the status bar at Apple's 9:41."""
        if self.simctl("install", udid, self.app) != 0:
            raise self.output.die(f"install failed on {udid}")
        self.simctl("privacy", udid, "grant", "location", BUNDLE)
        self.simctl("status_bar", udid, "override", "--time", "9:41", "--batteryState", "charged", "--batteryLevel",
                    "100", "--cellularBars", "4", "--wifiBars", "3")

    def shoot(self, udid: str, slug: str, url: str, out: str, country: str, filters: bool) -> bool:
        """One screen: the app launched on `url` through its launch-env hook (NOT `simctl openurl`, whose "Open in
        Showtimes?" dialog the shot would catch), the split-city area sheet marked seen, the Filtry sheet opened
        when asked (the app tests for the key's PRESENCE, so it is set only then)."""
        self.simctl("terminate", udid, BUNDLE)
        hooks = {"SIMCTL_CHILD_KINOWO_UITEST_DEEPLINK": url}
        if filters:
            hooks["SIMCTL_CHILD_KINOWO_UITEST_OPEN_FILTERS"] = "1"
        launched = self.run(["xcrun", "simctl", "launch", udid, BUNDLE, "-areaPickerSeenCities", f'("{slug}")',
                             *language_args(country)], env={**self.env, **hooks})
        if launched != 0:
            return False
        naps(self.env, float(self.env.get("SETTLE") or "6"))
        if self.simctl("io", udid, "screenshot", out) != 0:
            return False
        return os.path.isfile(out) and os.path.getsize(out) > 0


def first_film(env: Mapping[str, str], country: str, slug: str) -> str:
    """The url-encoded title the live listing puts first (the film the user sees on top), or ""."""
    try:
        films = get(env, f"{country_base(env, country)}/{slug}/api/repertoire", timeout=20)
        return urllib.parse.quote(films[0]["title"])  # type: ignore[index]
    except Exception:
        return ""


class Driver:
    def __init__(self, store: Store, simulators: Simulators) -> None:
        self.store = store
        self.sims = simulators
        self.out = store.output

    def device(self) -> str:
        env = self.store.env
        names = {"tablet-screenshots": env.get("IOS_TABLET") or "iPad Pro 13-inch (M5)",
                 "tablet-11-screenshots": env.get("IOS_TABLET_11") or "iPad Pro 11-inch (M5)"}
        return names.get(self.store.shot_class, env.get("IOS_PHONE") or "iPhone 17 Pro Max")

    def capture(self, locale: str, term: str, outdir: str = "", first: int = 0) -> None:
        """One city's five screens, by deep link, appended to `outdir` (its candidates dir by default)."""
        country, dest, first = self.store.destination(locale, outdir, first)
        slug = city_slug(self.store.env, country, term)
        if not slug:
            raise self.out.die(f"'{term}' isn't in {country}'s catalog — check the spelling.")
        self.out.say(f"{locale} · {term} ({slug})")
        udid = self.sims.ensure_device(self.device())
        self.sims.build()
        self.sims.install(udid)
        film = first_film(self.store.env, country, slug)
        if not film:
            self.out.warn(f"{term}: no film from the API — repeating the listing for shot 3")
        shots = self.store.shot_paths(dest, first)
        self.out.step(f"capturing {self.store.shots_per_city} screens")
        for i, (url, out) in enumerate(zip(capture_urls(slug, film), shots)):
            if not self.sims.shoot(udid, slug, url, out, country, filters=i == len(shots) - 1):
                raise self.out.die(f"{term}: screen {i + 1} never rendered — the simulator may be starved.")
        self.out.done()
        self.sims.simctl("terminate", udid, BUNDLE)
        self.out.ok(f"wrote {dest}/{{{first:03d}..{first + len(shots) - 1:03d}}}.png")
        if not self.store.env.get("NO_OPEN"):
            self.open_preview(shots)

    def open_preview(self, shots: List[str]) -> None:
        open_preview(self.store.env, self.sims.noise, shots)

    def all_top(self, args: Sequence[str], want_preview: bool) -> None:
        """N cities of every country from a start rank: one worker, since a device type boots only once."""
        n, offset = all_top_counts(self.store, args)
        before = self.store.baselines()
        self.out.say(f"{n} cities from rank {offset} × {len(self.store.countries)} countries")
        self.sims.build()
        quiet = Driver(Store({**self.store.env, "NO_OPEN": "1"}, self.out, self.store.listings, self.store.shot_class,
                             self.store.shots_per_city, self.store.countries), self.sims)
        run_worker(quiet.store, 0, 1, n, offset, quiet.capture)
        if want_preview:
            preview_all(self.store, before, self.open_preview)


def passes(driver: Driver, classes: List[str], args: List[str]) -> None:
    """The capture command once per class: a full ordinary single-class pass each, the build shared."""
    command = args[0]
    want_preview = not driver.store.env.get("NO_OPEN")
    countries = list(driver.store.countries)
    for shot_class in classes:
        driver.store.shot_class = shot_class
        driver.store.countries = list(countries)
        driver.out.say(f"▪ {shot_class} on {driver.device()}")
        if command == "--all-top":
            driver.all_top(args[1:], want_preview)
        elif command == "--country-top":
            country_top(driver.store, args[1:], lambda rest: driver.all_top(rest, want_preview))
        else:
            driver.capture(*capture_args(args))


def dispatch(driver: Driver, lock: Lock, args: List[str]) -> None:
    env = driver.store.env
    classes = (env.get("SHOT_CLASSES") or env.get("SHOT_CLASS") or "phone-screenshots tablet-screenshots").split()
    if args and args[0] in FLAG_CLASS:
        classes = [FLAG_CLASS[args[0]]]
        args = args[1:]
    command = args[0] if args else ""
    if command == "--top":
        cmd_top(driver.store, args[1:])
    elif command in ("-h", "--help", ""):
        usage_of(__doc__)
    else:
        lock.acquire()
        passes(driver, classes, args)


def main(argv: List[str], env: Mapping[str, str]) -> int:
    noise = noise_file(env)
    output = Output(noise)
    store = Store(env, output, os.path.join(REPO_ROOT, "ios/store/listings"), shots_per_city=5,
                  countries=countries_of(env))
    simulators = Simulators(env, output, noise)
    lock = Lock(env, output, "ios")
    return exit_code(lambda: dispatch(Driver(store, simulators), lock, argv), noise, simulators.shutdown,
                     lock.release)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], machine.environment()))
