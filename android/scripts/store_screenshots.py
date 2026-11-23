#!/usr/bin/env python3
"""Generate Google Play phone screenshots for the Kinowo / Showtimes app.
Just run it — it boots the emulator, installs the app, drives the four store
screens per city and pads them to a Play-safe ratio. One build makes pl/en/de
shots: picking a country in the gate forces that locale + backend.

  android/scripts/store-screenshots.sh en-GB "Birmingham"      # → candidates dir
  android/scripts/store-screenshots.sh pl-PL "Poznan" /tmp/x   # → a scratch dir
  android/scripts/store-screenshots.sh --top uk 10             # biggest cities by film count
  android/scripts/store-screenshots.sh --top uk 5 11           # ranks 11-15 instead of 1-5
  android/scripts/store-screenshots.sh --all-top 2             # top 2 cities of EVERY country
  android/scripts/store-screenshots.sh --all-top 2 4           # 2 cities from rank 4, every country
  android/scripts/store-screenshots.sh --country-top uk 10     # top 10 cities of ONE country
  android/scripts/store-screenshots.sh --country-top de 5 6    # de ranks 6-10 only

--top only PRINTS the ranking; --all-top and --country-top capture. Use
--country-top to top up a single locale without re-shooting the other two.

All three take an optional 1-based START RANK after the count, so
"--all-top 2 4" means "two cities beginning at the 4th best" — that is how you
shoot the runners-up without re-shooting the cities you already have.

A city that fails (blank list, a search the emulator mangled, a stray tap) is
SKIPPED, not fatal: the rest of the country still gets shot and the summary says
which ones were lost, so "--all-top 10" no longer risks losing ten cities to one.

SPLIT cities (London: Central/North/East/South/West) open a first-visit AREA
PICKER over the listing. The script asks the backend which cities are split
(/<slug>/api/cinemas → areas) and confirms the dialog — all areas arrive
pre-ticked, so that keeps the flat default. Flat cities skip the wait entirely.

Shots land in a candidates/ scratchpad INSIDE the published dir
(…/graphics/phone-screenshots/candidates/, gitignored), and every run APPENDS:
the next block starts one past the highest number already there, so a second
run never overwrites the first. Pick the keepers and move them up one level
into phone-screenshots/ — that is what gradle-play-publisher publishes, and
Play shows at most 8 phone shots.

--all-top writes one four-shot block per city, numbered end to end and ZERO-PADDED
to three digits (city 1 → 001-004.png, city 2 → 005-008.png, …) so they sort the
same lexically and numerically. It drives one read-only emulator per country in
parallel (~countries× faster); EMULATORS=<k> caps that (EMULATORS=1 = serial)
for hosts that can't feed three emulators at once. The city RANKING itself is
fetched concurrently too, so --top returns in about one request's time.

Only ONE capture run at a time: a second one is refused with the holder's pid
rather than allowed to fight over the AVD, ports and adb server (it used to kill
the first run's emulators and then drive the same instance). --top is exempt.

If an emulator disappears mid-run — you closed the window, or the system killed
it — the worker re-boots it on the same port and re-installs the app, then carries
on with the next city instead of losing the rest of that country.

When done it shuts down every emulator IT booted (a reused, already-running one
is left alone) and opens the shots in Preview (macOS). Env: EMULATORS=<k>
parallel emulator count · CLEAN_FILM="…" for a clean German detail (showtimes-de
lags) · BUILD=1 force rebuild+install · AVD=<name> · NO_OPEN=1 skip the Preview."""
from __future__ import annotations

import os
import queue
import shutil
import sys
import tempfile
import threading
import traceback
from typing import List, Mapping, Optional, Sequence

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "..", "scripts", "lib"))
from store_screenshots_common import (COUNTRY, Lock, Output, Store, all_top_counts,  # noqa: E402
                                      capture_args, cmd_top, countries_of, country_top, effective_k, exit_code, naps,
                                      noise_file, open_preview, preview_all, run_worker, usage_of,
                                      worker_slice)
from store_screenshots_device import ACT, PKG, Device, Rig, boot_pool, build_apk, pool_serial  # noqa: E402
from store_screenshots_ui import Screen, search_term  # noqa: E402

REPO_ROOT = os.path.dirname(os.path.dirname(HERE))
# kinowo_xl (Pixel 9 Pro XL, 1344×2992) tap coordinates
ALL_DATES = (1140, 164)        # list: "All / Wszystkie / Alle" pill
FILTERS = (1260, 164)          # list: filters icon
FILM = (250, 1275)             # list: first film → detail
CINEMA = (180, 682)            # Filtry: cinema section header
SEARCH_FILM = (694, 2800)      # list: "Search films" field


class Driver:
    """One run: the store layout, the devices' rig, and (in a pool worker) the emulator this worker owns."""

    def __init__(self, store: Store, rig: Rig, serial: Optional[str] = None) -> None:
        self.store = store
        self.rig = rig
        self.device = Device(rig, serial)
        self.screen = Screen(self.device)
        self.out = store.output

    def nap(self, seconds: float) -> None:
        naps(self.rig.env, seconds)

    def open_preview(self, shots: List[str]) -> None:
        open_preview(self.rig.env, self.rig.noise, shots)

    def capture(self, locale: str, term: str, outdir: str = "", first: int = 0) -> None:
        """One city's four store screens, padded Play-safe, appended to `outdir` (its candidates dir by default)."""
        country, dest, first = self.store.destination(locale, outdir, first)
        wait = 40 if country == "de" else 26          # the de backend is slow
        self.out.say(f"{locale} · {term}")
        if self.rig.pool:
            self.device.revive()
        else:
            self.device.ensure_emulator()
            self.device.ensure_app()
        self.open_app()
        self.pick_country(country)
        self.load_city(country, term, wait)
        stage = tempfile.mkdtemp(dir=self.rig.env.get("TMPDIR"))
        try:
            self.shoot_screens(country, term, wait, stage)
            shots = self.store.shot_paths(dest, first)
            for n, out in enumerate(shots, start=1):
                self.device.pad_playsafe(os.path.join(stage, f"{n}.png"), out)
        finally:
            shutil.rmtree(stage, ignore_errors=True)
        self.out.ok(f"wrote {dest}/{{{first:03d}..{first + 3:03d}}}.png")
        if not self.rig.env.get("NO_OPEN"):
            self.open_preview(shots)

    def open_app(self) -> None:
        self.out.step("opening app")
        self.device.must("shell", "pm", "clear", PKG)
        self.device.adb("shell", "pm", "grant", PKG, "android.permission.ACCESS_COARSE_LOCATION")
        self.device.must("shell", "am", "start", "-n", f"{PKG}/{ACT}")
        # a country PILL, not a big frame: right after `am start` the launcher is still on screen
        if not self.screen.wait_gate(150):
            raise self.out.die("the city gate never appeared — cold launch may have failed (see BUILD=1).")
        self.out.done()

    def pick_country(self, country: str) -> None:
        """The target pill always tapped (a country persisted by a previous run would leak in), and the localized
        header coming back proves the switch (the app recreates itself between the two)."""
        name = COUNTRY[country].name
        self.out.step(f"country → {name}")
        pill = self.screen.scroll_to_country(name)
        if not pill:
            raise self.out.die(f"'{name}' is not in the country row, even after scrolling it —\n   the app's catalog "
                               "may not carry that country yet.")
        self.device.tap(pill)
        if not self.screen.wait_text(COUNTRY[country].header, 150):
            raise self.out.die(f"country never switched to {name} — the pill tap didn't land.")
        self.nap(2)
        self.out.done()

    def load_city(self, country: str, term: str, wait: int) -> None:
        """The city searched for and its row (NAMED `term`, never "whatever is first") tapped, any area picker
        confirmed, and the gate proven gone before anything taps by coordinate."""
        self.out.step(f"loading {term}")
        self.screen.tap_field()
        self.nap(1)
        typed = search_term(term)
        if not self.screen.type_field_verified(typed):
            raise self.out.die(f"the city search kept mangling '{typed}' — the emulator is starved (try a lower "
                               "EMULATORS).")
        self.screen.tap_text(term, 60)
        self.screen.wait_frame(50, 60000)
        self.screen.dismiss_area_picker(country, term, wait)
        if not self.device.in_app():
            raise self.out.die("app fell out of foreground — a stray tap opened a browser.")
        if self.screen.showing(COUNTRY[country].header):
            raise self.out.die(f"still on the city gate after picking {term} — nothing was captured.")
        self.out.done()

    def shoot_screens(self, country: str, term: str, wait: int, stage: str) -> None:
        """Listing, detail, the Filtry sheet and its cinema section; a poster-bearing grid is waited for."""
        clean_film = self.rig.env.get("CLEAN_FILM", "")
        self.out.step("capturing 4 screens")
        self.device.tap(ALL_DATES)
        if not self.screen.wait_frame(wait + 25, 900000):
            self.out.warn(f"{term} list looks empty/blank — re-run")
        self.nap(3)
        self.device.snap(os.path.join(stage, "1.png"))
        if clean_film:
            self.device.tap(SEARCH_FILM)
            self.nap(1)
            self.device.type_(clean_film)
            self.nap(3)
        self.device.tap(FILM)
        self.nap(5)
        self.device.snap(os.path.join(stage, "2.png"))
        # the detail's SHARE icon sits where the listing's FILTERS icon does: wait for the listing to come back
        self.device.back()
        self.nap(2)
        if clean_film:
            self.device.back()
            self.nap(1)
        if not self.screen.wait_text(COUNTRY[country].listing_marker, 20):
            self.out.warn(f"{term}: the listing did not come back after the detail — screen 3 may be wrong")
        self.device.tap(FILTERS)
        self.nap(4)
        self.device.snap(os.path.join(stage, "3.png"))
        self.device.tap(CINEMA)
        self.nap(3)
        self.device.snap(os.path.join(stage, "4.png"))
        self.device.back()
        self.nap(1)
        self.out.done()

    def all_top(self, args: Sequence[str]) -> None:
        """N cities of every country, from a start rank, one read-only emulator per country in parallel."""
        n, offset = all_top_counts(self.store, args)
        if n * 4 > 8:
            self.out.warn(f"{n} cities × 4 screens = {n * 4} shots per locale; Play publishes at most 8 of them.")
        countries = self.store.countries
        k = effective_k(self.rig.env.get("EMULATORS") or str(len(countries)), len(countries))
        want_preview = not self.rig.env.get("NO_OPEN")
        before = self.store.baselines()
        self.out.say(f"{n} cities from rank {offset} × {len(countries)} countries on {k} emulator(s)")
        boot_pool(self.rig, k)
        self.rig.pool_apk = build_apk(self.rig)
        for w in range(k):
            self.out.step(f"installing app on {pool_serial(w)}")
            if Device(self.rig, pool_serial(w)).adb("install", "-r", self.rig.pool_apk) != 0:
                raise self.out.die(f"install failed on {pool_serial(w)}")
            self.out.done()
        self.rig.pool = True
        Pool(self, k, n, offset).run()
        if want_preview:
            preview_all(self.store, before, self.open_preview)


class Pool:
    """K workers, each on its own emulator and to its own log, each log flushed the moment its worker ends."""

    def __init__(self, driver: Driver, k: int, n: int, offset: int) -> None:
        self.driver = driver
        self.k = k
        self.n = n
        self.offset = offset
        self.ended: "queue.Queue[tuple[int, int]]" = queue.Queue()
        self.logs = [tempfile.mkstemp(dir=driver.rig.env.get("TMPDIR"))[1] for _ in range(k)]

    def work(self, w: int) -> None:
        main = self.driver
        code = 0
        with open(self.logs[w], "w") as log, tempfile.NamedTemporaryFile(dir=main.rig.env.get("TMPDIR")) as noise:
            output = Output(noise.name, out=log, err=log)
            env = {**main.rig.env, "NO_OPEN": "1"}
            rig = main.rig.to(output, noise.name)
            rig.env = env
            store = Store(env, output, main.store.listings, countries=main.store.countries)
            worker = Driver(store, rig, pool_serial(w))
            try:
                run_worker(store, w, self.k, self.n, self.offset, worker.capture)
            except Exception:
                log.write(traceback.format_exc())
                code = 1
        self.ended.put((w, code))

    def run(self) -> None:
        out = self.driver.out
        for w in range(self.k):
            threading.Thread(target=self.work, args=(w,), daemon=True).start()
            slice_ = " ".join(worker_slice(self.driver.store.countries, self.k, w))
            out.say(f"worker {w} → {pool_serial(w)} → {slice_} ")
        for _ in range(self.k):
            w, code = self.ended.get()
            with open(self.logs[w]) as log:
                print(f"\n\033[36m▸ worker {w} ({pool_serial(w)})\033[0m\n" + log.read(), end="", flush=True)
            os.remove(self.logs[w])
            if code:
                out.warn(f"worker {w} exited {code}")


def dispatch(driver: Driver, lock: Lock, args: List[str]) -> None:
    """--top prints a ranking and takes no lock; every capture path takes it."""
    command = args[0] if args else ""
    if command == "--top":
        cmd_top(driver.store, args[1:])
    elif command in ("-h", "--help", ""):
        usage_of(__doc__)
    else:
        lock.acquire()
        if command == "--all-top":
            driver.all_top(args[1:])
        elif command == "--country-top":
            country_top(driver.store, args[1:], driver.all_top)
        else:
            driver.capture(*capture_args(args))


def main(argv: List[str], env: Mapping[str, str]) -> int:
    sdk = env.get("ANDROID_SDK_ROOT") or env.get("ANDROID_HOME") or "/opt/homebrew/share/android-commandlinetools"
    env = {**env, "ANDROID_HOME": sdk}
    noise = noise_file(env)
    output = Output(noise)
    store = Store(env, output, os.path.join(REPO_ROOT, "android/app/src/main/play/listings"),
                  countries=countries_of(env))
    rig = Rig(env, output, REPO_ROOT, noise, sdk=sdk, avd=env.get("AVD") or "kinowo_xl")
    lock = Lock(env, output, "android")
    return exit_code(lambda: dispatch(Driver(store, rig), lock, argv), noise, rig.stop_emulators, lock.release)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], os.environ))
