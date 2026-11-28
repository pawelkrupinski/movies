"""The store-screenshot drivers' SHARED ENGINE -- android/scripts/store_screenshots.py and ios/scripts/
store_screenshots.py both import it. Everything here is platform-agnostic: which countries exist, how cities are
ranked, whether a city is split, where shots land and what they are numbered, the one-run-at-a-time lock and the --top
table. The drivers keep only what genuinely differs -- an emulator pool driven by taps, or simulators driven by deep
links.

Env (the drivers' tests): STORE_API_BASE puts every country's backend at <it>/<country>; STORE_NAPS=0 skips every
pause (a wait loop counts its nominal seconds, so it stays bounded without them)."""
from __future__ import annotations

import concurrent.futures
import json
import math
import os
import re
import shutil
import signal
import subprocess
import sys
import tempfile
import time
import unicodedata
import urllib.request
from dataclasses import dataclass, field
from types import FrameType
from typing import Callable, Dict, List, Mapping, Optional, Sequence, TextIO, Tuple

# ── countries ─────────────────────────────────────────────────────────────────
# Every country the apps carry: the one place a new country is added. A country is one deployment + one store locale
# + one UI language. ALL_COUNTRIES is never narrowed (anything asking "which pills could be on screen" reads it); a
# run walks COUNTRIES, which --country-top narrows.
ALL_COUNTRIES = ["pl", "uk", "de", "us", "es"]


@dataclass(frozen=True)
class Country:
    locale: str
    # the UI language a launch pins, mirroring `Country.languageCode` in the apps: the bundle's (en), not the listing's
    language: str
    # the catalog's ENDONYM, identical in every locale, so it doubles as a tap target whatever the app's language
    name: str
    base: str
    # the gate's localized "Country" header: seeing it proves the country switch landed
    header: str
    # the listing's "All dates" pill (R.string.all): proof the LISTING is on screen rather than a film's detail
    listing_marker: str
    # the area picker's confirm button, shown only by SPLIT cities
    showlist_label: str


COUNTRY = {
    "pl": Country("pl-PL", "pl", "Polska", "https://kinowo.net", "Kraj", "Wszystkie", "Pokaż repertuar"),
    "uk": Country("en-GB", "en", "United Kingdom", "https://showtimes.cc/uk", "Country", "All", "Show listings"),
    "de": Country("de-DE", "de", "Deutschland", "https://showtimes.cc/de", "Land", "Alle", "Programm anzeigen"),
    "us": Country("en-US", "en", "United States", "https://showtimes.cc/us", "Country", "All", "Show listings"),
    "es": Country("es-ES", "es", "España", "https://showtimes.cc/es", "País", "Todo", "Ver la cartelera"),
}
LOCALE_COUNTRY = {c.locale: code for code, c in COUNTRY.items()}


def country_base(env: Mapping[str, str], country: str) -> str:
    if country not in COUNTRY:
        return ""
    if env.get("STORE_API_BASE"):
        return f"{env['STORE_API_BASE']}/{country}"
    return COUNTRY[country].base


def fold(text: str) -> str:
    """Case and diacritics folded (ł too, which NFD keeps), as the apps' own city search matches."""
    text = text.replace("ł", "l").replace("Ł", "L")
    return "".join(c for c in unicodedata.normalize("NFD", text) if not unicodedata.combining(c)).casefold()


# ── output ────────────────────────────────────────────────────────────────────
class Died(Exception):
    """A die(): its message already printed; the run (or, in a worker, the city) ends."""


class Output:
    """say / ok / warn / die / step / done_ (scripts/log.sh's), to `out` and `err`; die tails `noise`."""

    def __init__(self, noise: str, out: TextIO = sys.stdout, err: TextIO = sys.stderr) -> None:
        self.noise = noise
        self.out = out
        self.err = err

    def _write(self, stream: TextIO, text: str) -> None:
        stream.write(text)
        stream.flush()

    def say(self, text: str) -> None:
        self._write(self.out, f"\033[36m▸\033[0m {text}\n")

    def ok(self, text: str) -> None:
        self._write(self.out, f"\033[32m✓\033[0m {text}\n")

    def warn(self, text: str) -> None:
        self._write(self.err, f"\033[33m!\033[0m {text}\n")

    def step(self, text: str, to_err: bool = False) -> None:
        self._write(self.err if to_err else self.out, f"  {text}… ")

    def done(self, to_err: bool = False) -> None:
        self._write(self.err if to_err else self.out, "\033[32m✓\033[0m\n")

    def die(self, text: str) -> Died:
        self._write(self.err, f"\033[31m✗\033[0m {text}\n")
        if self.noise and os.path.exists(self.noise) and os.path.getsize(self.noise) > 0:
            with open(self.noise, errors="replace") as f:
                self._write(self.err, "".join(f.readlines()[-5:]))
        return Died(text)


def naps(env: Mapping[str, str], seconds: float) -> None:
    if env.get("STORE_NAPS", "1") != "0":
        time.sleep(seconds)


def positive(text: str) -> Optional[int]:
    """`text` as a whole number of at least 1, else None (the shell's `[ "$n" -ge 1 ] 2>/dev/null`)."""
    return int(text) if re.fullmatch(r"\s*[+-]?[0-9]+\s*", text) and int(text) >= 1 else None


# ── one run at a time ─────────────────────────────────────────────────────────
# The drivers are hostile to a second concurrent run of themselves: the Android pool opens with `adb kill-server`, a
# pkill of the AVD's qemu and deleting its locks, and both allocate device slots from the same end. A directory is the
# lock (mkdir is atomic, macOS has no flock(1)); the holder's pid inside tells a killed run's lock from a live one.
class Lock:
    def __init__(self, env: Mapping[str, str], output: Output, resource: str) -> None:
        root = env.get("LOCK_ROOT") or env.get("TMPDIR") or "/tmp"
        self.dir = os.path.join(root, f"kinowo-screenshots-{resource}.lock")
        self.output = output
        self.resource = resource
        self.held = False

    def acquire(self) -> None:
        if not self._made():
            holder = self._holder()
            if holder and _alive(holder):
                raise self.output.die(f"another store-screenshots run (pid {holder}) is already driving the "
                                      f"{self.resource} devices.\n   Two runs sabotage each other — wait for it, or "
                                      f"stop it first:  kill {holder}")
            self.output.warn(f"clearing a stale {self.resource} lock left by pid {holder or 'unknown'}")
            shutil.rmtree(self.dir, ignore_errors=True)
            if not self._made():
                raise self.output.die(f"could not take the {self.resource} lock at {self.dir}")
        self.held = True
        with open(os.path.join(self.dir, "pid"), "w") as f:
            f.write(f"{os.getpid()}\n")

    def _made(self) -> bool:
        try:
            os.mkdir(self.dir)
        except OSError:
            return False
        return True

    def _holder(self) -> str:
        try:
            with open(os.path.join(self.dir, "pid")) as f:
                return f.read().strip()
        except OSError:
            return ""

    def release(self) -> None:
        if self.held:
            shutil.rmtree(self.dir, ignore_errors=True)
            self.held = False


def _alive(pid: str) -> bool:
    try:
        os.kill(int(pid), 0)
    except (OSError, ValueError):
        return False
    return True


# ── where shots land ──────────────────────────────────────────────────────────
@dataclass
class Store:
    """One driver's run: its listings root, the class being shot (gradle-play-publisher's ImageType.dirName, which
    the iOS side mirrors), the files a city writes, and the countries the run walks."""
    env: Mapping[str, str]
    output: Output
    listings: str
    shot_class: str = "phone-screenshots"
    shots_per_city: int = 4
    countries: List[str] = field(default_factory=lambda: list(ALL_COUNTRIES))

    def candidates_dir(self, locale: str) -> str:
        """A candidates/ scratchpad INSIDE the published dir: captures are raw material, promoted by a deliberate
        `mv` up one level. GPP includes `/listings/*/graphics/<dirName>/*`, one segment, so nothing here publishes."""
        return f"{self.listings}/{locale}/graphics/{self.shot_class}/candidates"

    def shot_paths(self, directory: str, first: int) -> List[str]:
        """The files one capture writes, zero-padded to three digits so lexical order is numeric order."""
        return [f"{directory}/{first + n:03d}.png" for n in range(self.shots_per_city)]

    def destination(self, locale: str, outdir: str, first: int) -> Tuple[str, str, int]:
        """A capture's country, the dir it appends to (`outdir`, or the locale's candidates dir; made) and its first
        file number (`first`, or one past what the dir holds)."""
        country = LOCALE_COUNTRY.get(locale)
        if not country:
            raise self.output.die(f"unknown locale '{locale}' (use en-GB | pl-PL | de-DE)")
        dest = outdir or self.candidates_dir(locale)
        os.makedirs(dest, exist_ok=True)
        return country, dest, first or next_shot_number(dest)

    def baselines(self) -> Dict[str, int]:
        """Where each locale's candidates dir ENDS before a run, so the closing Preview shows only this run's."""
        return {COUNTRY[c].locale: next_shot_number(self.candidates_dir(COUNTRY[c].locale)) for c in self.countries}


def next_shot_number(directory: str) -> int:
    """One past the highest N.png in `directory` (numerically: 10 after 9, 008 as eight), or 1: runs APPEND."""
    try:
        names = os.listdir(directory)
    except OSError:
        return 1
    numbers = [int(n[:-4]) for n in names if n.endswith(".png") and re.fullmatch(r"[0-9]+", n[:-4])]
    return max(numbers, default=0) + 1


# ── work splitting ────────────────────────────────────────────────────────────
def worker_slice(countries: Sequence[str], k: int, w: int) -> List[str]:
    """Worker W of K takes indices W, W+K, W+2K… of the country list."""
    return [c for i, c in enumerate(countries) if i % k == w]


def effective_k(requested: str, most: int) -> int:
    """A requested worker count clamped to [1, `most`]; anything not a number is 1."""
    asked = positive(requested) or 1
    return min(asked, most)


# ── the backend ───────────────────────────────────────────────────────────────
def get(env: Mapping[str, str], url: str, timeout: float = 30, tries: int = 1) -> object:
    for attempt in range(tries):
        try:
            with urllib.request.urlopen(url, timeout=timeout) as response:
                return json.loads(response.read())
        except Exception:
            if attempt == tries - 1:
                raise
            naps(env, 1.5)
    raise AssertionError("unreachable")


def catalog(env: Mapping[str, str], base: str, tries: int = 1) -> List[Dict[str, str]]:
    answer = get(env, f"{base}/api/catalog", tries=tries)
    assert isinstance(answer, dict)
    return list(answer["cities"])  # type: ignore[arg-type]


def rank_cities(store: Store, country: str, n: int, offset: int = 1) -> Tuple[int, List[Tuple[int, str, str]]]:
    """A country's city count, and its cities ranked by live film count (films, slug, name), best first, from the
    1-based rank `offset`: every city is counted (concurrently) before the ranking is sliced."""
    base = country_base(store.env, country)
    if not base:
        raise store.output.die(f"unknown country '{country}' (use pl | uk | de | us | es)")
    cities = [c for c in catalog(store.env, base, tries=3) if c.get("country") == country]

    def count(city: Dict[str, str]) -> Tuple[int, str, str]:
        try:
            films = get(store.env, f"{base}/{city['slug']}/api/repertoire", tries=3)
            total = len(films) if isinstance(films, list) else len(films.get("films", []))  # type: ignore[union-attr]
            return (total, city["slug"], city["name"])
        except Exception:
            return (-1, city["slug"], city["name"])

    with concurrent.futures.ThreadPoolExecutor(max_workers=8) as pool:
        rows = sorted(pool.map(count, cities), reverse=True)
    return len(cities), rows[offset - 1:offset - 1 + n]


def city_slug(env: Mapping[str, str], country: str, city: str) -> str:
    """The slug of whatever the caller typed (name or slug, diacritics or not), or "" when it is not in the catalog
    or the catalog is unreachable."""
    want = fold(city)
    try:
        cities = catalog(env, country_base(env, country))
    except Exception:
        return ""
    return next((c["slug"] for c in cities if want in (fold(c["name"]), fold(c["slug"]))), "")


def city_area_count(env: Mapping[str, str], country: str, city: str) -> int:
    """How many AREAS the app offers for this city (non-zero: a SPLIT city, which opens a first-visit area picker),
    from `/<slug>/api/cinemas`; -1 when the lookup fails, which callers read as "might be split"."""
    slug = city_slug(env, country, city)
    try:
        answer = get(env, f"{country_base(env, country)}/{slug}/api/cinemas") if slug else None
    except Exception:
        return -1
    if not isinstance(answer, dict):
        return -1
    return len(answer.get("areas") or [])  # type: ignore[arg-type]


def cmd_top(store: Store, args: Sequence[str]) -> None:
    """--top <country> [N] [start rank]: the ranking as a readable table, rows numbered by ABSOLUTE rank."""
    country = args[0] if args else ""
    n = int(args[1]) if len(args) > 1 else 10
    offset = positive(args[2]) if len(args) > 2 else 1
    if offset is None:
        raise store.output.die("--top's start rank is 1-based, e.g. --top uk 5 11")
    total, rows = rank_cities(store, country, n, offset)
    print(f"{n} {country} cities from rank {offset} by live film count ({total} total):")
    for rank, (films, slug, name) in enumerate(rows, start=offset):
        print(f"  {rank:>3}. {films:>4}  {slug:<22} {name}")
    sys.stdout.flush()


def usage_of(docstring: Optional[str]) -> None:
    print("\n" + (docstring or "").rstrip("\n") + "\n")


def png_size(path: str) -> Optional[Tuple[int, int]]:
    """A PNG's (width, height) out of its IHDR, the first chunk: big-endian at bytes 16 and 20."""
    with open(path, "rb") as f:
        head = f.read(24)
    if len(head) < 24:
        return None
    return int.from_bytes(head[16:20], "big"), int.from_bytes(head[20:24], "big")


def play_safe_width(height: int) -> int:
    """The width that keeps long:short at 1.98:1 (Play caps at 2:1)."""
    return int(math.ceil(height / 1.98))


# ── driver-shared commands ────────────────────────────────────────────────────
def preview_all(store: Store, before: Mapping[str, int], open_preview: Callable[[List[str]], None]) -> None:
    """Every shot this run captured (each locale's since its baseline, in numeric order), opened at once."""
    shots: List[str] = []
    for country in store.countries:
        locale = COUNTRY[country].locale
        dest = store.candidates_dir(locale)
        for number in range(before.get(locale, 1), next_shot_number(dest)):
            if os.path.isfile(f"{dest}/{number:03d}.png"):
                shots.append(f"{dest}/{number:03d}.png")
    if shots:
        open_preview(shots)


def country_top(store: Store, args: Sequence[str], all_top: Callable[[Sequence[str]], None]) -> None:
    """--country-top <country> [N] [start rank]: --all-top narrowed to one country."""
    country = args[0] if args else ""
    if country not in COUNTRY:
        raise store.output.die("--country-top wants a country: --country-top uk 10 (or --country-top uk 10 4)")
    store.countries = [country]
    all_top([args[1] if len(args) > 1 else "2", args[2] if len(args) > 2 else "1"])


def all_top_counts(store: Store, args: Sequence[str]) -> Tuple[int, int]:
    """--all-top's N and start rank, each at least 1."""
    n = positive(args[0] if args else "2")
    if n is None:
        raise store.output.die("--all-top wants a city count, e.g. --all-top 2")
    offset = positive(args[1] if len(args) > 1 else "1")
    if offset is None:
        raise store.output.die("--all-top's start rank is 1-based, e.g. --all-top 2 4 (2 cities from the 4th)")
    return n, offset


def run_worker(store: Store, w: int, k: int, n: int, offset: int, capture: Callable[[str, str], None]) -> None:
    """One worker: its slice of the countries, city by city, each landing the moment it works. A failed city (a
    die() inside `capture`) costs only that city; the tally says which were lost."""
    for country in worker_slice(store.countries, k, w):
        locale = COUNTRY[country].locale
        dest = store.candidates_dir(locale)
        names = [name for _, _, name in ranked(store, country, n, offset) if name]
        os.makedirs(dest, exist_ok=True)
        failed: List[str] = []
        for name in names:
            try:
                capture(locale, name)
            except Died:
                failed.append(name)
                store.output.warn(f"{locale}: {name} failed — skipping it")
        tally(store.output, locale, names, failed, f"cities from rank {offset} → {dest}")


def tally(output: Output, locale: str, names: Sequence[str], failed: Sequence[str], what: str) -> None:
    if failed:
        output.warn(f"{locale}: failed on " + ", ".join(failed))
    if len(failed) == len(names):
        output.warn(f"{locale}: NOTHING captured — all {len(names)} cities failed")
    else:
        output.ok(f"{locale}: {len(names) - len(failed)}/{len(names)} {what}")


def ranked(store: Store, country: str, n: int, offset: int) -> List[Tuple[int, str, str]]:
    """rank_cities' rows, or none when the backend could not rank them (the run goes on to the next country)."""
    try:
        return rank_cities(store, country, n, offset)[1]
    except Exception as failure:
        store.output.warn(f"{country}: could not rank its cities: {failure}")
        return []


def countries_of(env: Mapping[str, str]) -> List[str]:
    return env.get("COUNTRIES", "").split() or list(ALL_COUNTRIES)


def capture_args(args: Sequence[str]) -> Tuple[str, str, str, int]:
    """A capture's <locale> <city> [outdir] [first file number] (0: one past what the dir holds)."""
    locale, term, outdir, first = (list(args) + [""] * 4)[:4]
    return locale, term, outdir, int(first) if first.isdigit() else 0


def open_preview(env: Mapping[str, str], noise: str, shots: Sequence[str]) -> None:
    """The shots opened in Preview (macOS), when there is an `open` to do it."""
    if shots and shutil.which("open", path=env.get("PATH")):
        with open(noise, "ab") as out:
            subprocess.call(["open", "-a", "Preview", *shots], stdout=out, stderr=out, stdin=subprocess.DEVNULL,
                            env=dict(env))


def _ended_by(signum: int, _frame: Optional[FrameType]) -> None:
    raise SystemExit(128 + signum)


def noise_file(env: Mapping[str, str]) -> str:
    """Where the devices' and builds' chatter goes (die() tails it); INT, TERM and HUP end the run through its
    cleanup from here on, as the shell drivers' EXIT trap did."""
    for signum in (signal.SIGINT, signal.SIGTERM, signal.SIGHUP):
        signal.signal(signum, _ended_by)
    return tempfile.mkstemp(dir=env.get("TMPDIR"))[1]


def exit_code(run: Callable[[], None], noise: str, *cleanups: Callable[[], None]) -> int:
    """`run`'s exit status (1 after a die()), every cleanup run and the noise file gone however it ended."""
    try:
        run()
        return 0
    except Died:
        return 1
    finally:
        for cleanup in cleanups:
            cleanup()
        os.remove(noise)
