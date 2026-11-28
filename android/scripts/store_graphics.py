#!/usr/bin/env python3
"""Generate the Play listing's per-language feature graphic (1024×500), and fan the one shared app icon out.

Every locale gets its FEATURE GRAPHIC, and the one APP ICON copied in.

  android/scripts/store-graphics.sh              # every locale
  android/scripts/store-graphics.sh de-DE        # just one

Play keys both assets per LANGUAGE, but only one of them actually differs by
language. The icon is identical everywhere, so it lives ONCE at
android/store/icon/1.png and is copied into each
listings/<locale>/graphics/icon/ at generation time — the copies are gitignored
derivatives, the master is the tracked original. Keeping three byte-identical
icons under version control was the alternative, and they drift.

The feature graphic DOES differ: it carries the brand name and the tagline, both
of which are per-language, plus that locale's own first screenshot — so the
German card shows German UI. The brand comes from the listing itself (title.txt
before the colon), so the card can never disagree with the listing it sits above.

Rendered with headless Chrome rather than an image library: the layout is CSS,
which is far easier to adjust than hand-placed rectangles, and Chrome is already
a dependency of the page tests. CHROME=<path> names another Chrome.

Env (its test): STORE_NAPS=0 skips the render wait's pauses (the wait stays bounded by its count)."""
from __future__ import annotations

import base64
import os
import shutil
import subprocess
import sys
import tempfile
import time
from string import Template
from typing import List, Mapping, Optional, Sequence

HERE = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = os.path.dirname(os.path.dirname(HERE))
sys.path.insert(0, os.path.join(REPO_ROOT, "scripts", "lib"))
from processes import machine  # noqa: E402
from mobile.store_screenshots_common import COUNTRY, Died, Output, png_size  # noqa: E402

LISTINGS = os.path.join(REPO_ROOT, "android/app/src/main/play/listings")
# Deliberately OUTSIDE app/src/main/play: gradle-play-publisher validates that
# whole tree and fails the build on any path it does not recognise ("Unknown Play
# resource file"), so the shared master cannot live inside it. android/store/
# mirrors ios/store/ instead.
SHARED_ICON = os.path.join(REPO_ROOT, "android/store/icon/1.png")
DEFAULT_CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
# Play's exact slot size. Anything else is rejected at upload, so it is checked
# after rendering rather than trusted.
WIDTH, HEIGHT = 1024, 500
# How long Chrome gets to write the card, in one-second pauses.
RENDER_WAIT = 90

# The one-line tagline under the wordmark. NOT short-description.txt: that is the
# store blurb, which in English and German spends its 80 characters listing rating
# sources and reads as a paragraph on a card. These are the same strings the web
# share-cards use — OgCardGenerator.homeTagline — so the card, the OG image and the
# site all say the same thing in each language (store_graphics_test.py holds them to it).
TAGLINES = {
    "pl-PL": "Repertuar kin w Twoim mieście",
    "de-DE": "Kinoprogramm in deiner Stadt",
    "es-ES": "Cartelera de cine en tu ciudad",
}
ENGLISH_TAGLINE = "Cinema listings in your city"

# App palette, straight from ui/theme/Theme.kt so the card matches the product;
# the rating-badge colours too, so the pills are the app's, not an approximation of
# them. RtFresh really is the red one (>=60) — that is the app's mapping.
PALETTE = {
    "BG": "#0B0B12", "CARD": "#1E1E2E", "ELEVATED": "#2A2A3E",
    "IMDB": "#F5C518", "META_GOOD": "#66CC66", "RT_FRESH": "#FA320A",
    "FW_ORANGE": "#FF6C00", "FW_LIGHT": "#FF9C4A",
}

CARD = Template("""<!doctype html><meta charset="utf-8">
<style>
  * { margin:0; padding:0; box-sizing:border-box; }
  html, body { width:${WIDTH}px; height:${HEIGHT}px; overflow:hidden; }
  body {
    background:
      radial-gradient(900px 420px at 78% 50%, rgba(107,171,222,.20), transparent 70%),
      radial-gradient(600px 400px at 8% 20%, rgba(170,212,255,.10), transparent 65%),
      $BG;
    font-family: "Helvetica Neue", Helvetica, Arial, sans-serif;
    color:#fff; display:flex; align-items:center; gap:44px; padding:0 56px;
  }
  .copy { flex:1 1 auto; min-width:0; }
  .row { display:flex; align-items:center; gap:18px; margin-bottom:22px; }
  .icon { width:92px; height:92px; border-radius:21px; box-shadow:0 10px 30px rgba(0,0,0,.55); }
  h1 { font-size:62px; font-weight:700; letter-spacing:-1.5px; line-height:1; }
  /* One line, always. The copy column is ~650px wide and the longest tagline
     ("Repertuar kin w Twoim mieście", 29 chars) sets ~435px at this size, so
     nowrap cannot clip — but keep that headroom in mind before lengthening one. */
  p  { font-size:30px; line-height:1.3; color:rgba(255,255,255,.82); white-space:nowrap; }
  /* The app's LabelValuePill, at card scale: a bold label chip on the source's
     colour, then the value on CardElevated in that same colour. Same 3dp-ish
     corner, same label-Black / value-SemiBold weights. */
  .pills { display:flex; gap:9px; margin-top:30px; align-items:center; }
  .pill { display:flex; border-radius:5px; overflow:hidden; font-size:20px; line-height:1; }
  .pill .l { font-weight:900; padding:8px 9px; }
  .pill .v { font-weight:600; padding:8px 9px; background:$ELEVATED; }
  /* Metacritic is the label-less solid one — just the score on its band colour. */
  .pill.solid { font-weight:900; padding:8px 10px; color:#0B0B12; background:$META_GOOD; }
  .imdb .l { background:$IMDB; color:#000; }   .imdb .v { color:$IMDB; }
  .rt   .l { background:$RT_FRESH; color:#fff; } .rt   .v { color:$RT_FRESH; }
  .fw   .l { background:$FW_ORANGE; color:#fff; } .fw .v { color:$FW_LIGHT; }
  /* The locale's OWN first screenshot: the German card must show German UI. */
  .phone {
    flex:0 0 auto; width:216px; height:396px; border-radius:28px; overflow:hidden;
    border:5px solid $CARD; box-shadow:0 26px 60px rgba(0,0,0,.6);
    transform: rotate(3deg);
  }
  .phone img { width:100%; height:100%; object-fit:cover; object-position:top center; display:block; }
</style>
<div class="copy">
  <div class="row">
    <img class="icon" src="$icon">
    <h1>$brand</h1>
  </div>
  <p>$tagline</p>
  <!-- Real ratings rather than bare source names: this is how a film actually
       reads in the app. Filmweb is a POLISH site and the app only carries it for
       Poland (Country.filmwebEnabled), so the FW pill is Poland's alone — every
       other card would be advertising a rating source that listing never shows. -->
  <div class="pills">
    <span class="pill imdb"><span class="l">IMDb</span><span class="v">7.9</span></span>
    <span class="pill solid">81</span>
    <span class="pill rt"><span class="l">RT</span><span class="v">91%</span></span>
$filmweb
  </div>
</div>
$phone
""")
FILMWEB_PILL = '    <span class="pill fw"><span class="l">FW</span><span class="v">7.4</span></span>'


def brand_of(locale: str) -> str:
    """Whatever the listing title says before the colon — "Kinowo" in Poland, "Showtimes" everywhere else (see
    Country.brandName). Deriving it beats a second copy of that rule here."""
    with open(os.path.join(LISTINGS, locale, "title.txt"), encoding="utf-8") as f:
        return f.read().split(":", 1)[0].replace("\n", "")


def data_uri(png: str) -> str:
    """A base64 data URI for an image, so the HTML is self-contained and Chrome needs no file-access flags."""
    with open(png, "rb") as f:
        return "data:image/png;base64," + base64.b64encode(f.read()).decode("ascii")


def feature_html(locale: str, icon: str, shot: str) -> str:
    """The card for `locale`: `icon` and `shot` are data URIs, `shot` empty for a card without a phone."""
    return CARD.substitute(PALETTE, WIDTH=WIDTH, HEIGHT=HEIGHT, icon=icon, brand=brand_of(locale),
                           tagline=TAGLINES.get(locale, ENGLISH_TAGLINE),
                           filmweb=FILMWEB_PILL if locale == "pl-PL" else "",
                           phone=f'<div class="phone"><img src="{shot}"></div>' if shot else "")


class Generator:
    def __init__(self, env: Mapping[str, str], output: Output) -> None:
        self.env = env
        self.output = output
        self.chrome = env.get("CHROME") or DEFAULT_CHROME

    def render(self, html: str, out: str) -> None:
        """Chrome's new headless mode does not reliably EXIT after --screenshot: it writes the file and then sits at 0%
        CPU forever. So it runs in the background until the PNG has appeared, and is then ended — the artefact is the
        completion signal, not the process."""
        profile = tempfile.mkdtemp()
        if os.path.exists(out):
            os.remove(out)
        chrome = subprocess.Popen(
            [self.chrome, "--headless", "--disable-gpu", "--hide-scrollbars", "--no-first-run",
             "--no-default-browser-check", "--disable-extensions", "--force-device-scale-factor=1",
             f"--window-size={WIDTH},{HEIGHT}", f"--user-data-dir={profile}", f"--screenshot={out}", f"file://{html}"],
            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        waited = self.wait_for_png(out, chrome)
        chrome.kill()
        chrome.wait()
        shutil.rmtree(profile, ignore_errors=True)
        if not nonempty(out):
            raise self.output.die(f"Chrome produced no image for {out} after {waited}s")

    def wait_for_png(self, out: str, chrome: "subprocess.Popen[bytes]") -> int:
        """Seconds waited for `out` to be a PNG that parses AND has stopped growing: Chrome writes it progressively,
        and a size read off a header not yet landed failed a card that was in fact perfect. A Chrome that has ended
        writes nothing more, so its last state is the answer."""
        last: Optional[int] = None
        for waited in range(RENDER_WAIT):
            ended = chrome.poll() is not None
            size = os.path.getsize(out) if nonempty(out) else None
            if size is not None and (size == last or ended) and png_size(out):
                return waited
            if ended:
                return waited
            last = size
            pause(self.env)
        return RENDER_WAIT

    def phone_shot(self, locale: str, graphics: str) -> str:
        """This locale's own first published screenshot as a data URI, downscaled first (the card shows it 236px wide,
        and a 3MB PNG in the HTML made Chrome crawl); "" with a warning when there is none yet."""
        source = os.path.join(graphics, "phone-screenshots", "001.png")
        if not os.path.isfile(source):
            self.output.warn(f"{locale}: no 001.png yet — rendering the card without a phone")
            return ""
        handle, small = tempfile.mkstemp(prefix="shot-", suffix=".png")
        os.close(handle)
        try:
            subprocess.run(["sips", "-Z", "560", source, "--out", small], stdout=subprocess.DEVNULL,
                           stderr=subprocess.DEVNULL, check=True)
            return data_uri(small)
        except subprocess.CalledProcessError:
            raise self.output.die(f"{locale}: sips could not downscale {source}") from None
        finally:
            os.remove(small)

    def generate(self, locale: str) -> None:
        if not os.path.isdir(os.path.join(LISTINGS, locale)):
            raise self.output.die(f"no listing dir for '{locale}'")
        graphics = os.path.join(LISTINGS, locale, "graphics")
        # The icon: one master, copied out. Derived, gitignored.
        os.makedirs(os.path.join(graphics, "icon"), exist_ok=True)
        shutil.copyfile(SHARED_ICON, os.path.join(graphics, "icon", "1.png"))
        html = feature_html(locale, data_uri(SHARED_ICON), self.phone_shot(locale, graphics))
        handle, page = tempfile.mkstemp(prefix="feature-", suffix=".html")
        with os.fdopen(handle, "w", encoding="utf-8") as f:
            f.write(html)
        out = os.path.join(graphics, "feature-graphic", "1.png")
        os.makedirs(os.path.dirname(out), exist_ok=True)
        self.output.step(f"{locale} feature graphic")
        try:
            self.render(page, out)
        finally:
            os.remove(page)
        width, height = png_size(out) or ("", "")
        if (width, height) != (WIDTH, HEIGHT):
            raise self.output.die(f"{locale} card is {width}×{height}; Play requires exactly {WIDTH}×{HEIGHT}")
        self.output.done()
        self.output.ok(f"{locale} → {out} ({width}×{height}, {os.path.getsize(out) // 1024}KB)")


def pause(env: Mapping[str, str]) -> None:
    """One second of the render wait; under STORE_NAPS=0 (its test) a twentieth, as Chrome still needs real time."""
    time.sleep(1 if env.get("STORE_NAPS", "1") != "0" else 0.05)


def nonempty(path: str) -> bool:
    return os.path.isfile(path) and os.path.getsize(path) > 0


def run(generator: Generator, output: Output, locales: Sequence[str]) -> None:
    if not os.path.isfile(SHARED_ICON):
        raise output.die(f"no shared icon at {SHARED_ICON}")
    if not (os.path.isfile(generator.chrome) and os.access(generator.chrome, os.X_OK)):
        raise output.die(f"no Chrome at {generator.chrome} — set CHROME=<path>")
    if not locales:
        output.say("feature graphics + icon for every locale")
    # Every locale the apps carry, in the store screenshots' order (store_screenshots_common.COUNTRY).
    for locale in locales or [country.locale for country in COUNTRY.values()]:
        generator.generate(locale)


def main(argv: List[str], env: Mapping[str, str]) -> int:
    output = Output("")
    try:
        run(Generator(env, output), output, argv)
    except Died:
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], machine.environment()))
