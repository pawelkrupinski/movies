#!/usr/bin/env python3
"""android/scripts/store-graphics.sh, black-box, over a scratch copy of its layout (scripts/lib/store_screenshots_fixture
.copy_layout): a fake Chrome that keeps the card's HTML it was handed and writes the screenshot, a fake sips. Nothing
renders and no listing of this checkout is written. What the shell test checked through the sourced helpers -- where
the card's copy comes from, what the card is built of, one icon fanned out -- is read here off the card each run
hands Chrome; the drift guards (Theme.kt's colours, OgCardGenerator's taglines, the committed cards and .gitignore)
read this checkout. The same cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import re
import sys
import unittest
from typing import Dict, Sequence

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "..", "scripts", "lib"))
import cli_harness  # noqa: E402
from store_screenshots_fixture import copy_layout, png  # noqa: E402
from store_screenshots_common import png_size  # noqa: E402

TOOL = cli_harness.tool_path("android/scripts/store_graphics.py", "android/scripts/store-graphics.sh")
LISTINGS = "android/app/src/main/play/listings"
ICON = "android/store/icon/1.png"
LOCALES = ["pl-PL", "en-GB", "de-DE", "en-US", "es-ES"]
ENGLISH = "Cinema listings in your city"
# The fake Chrome: the card's HTML kept as <FAKE_DIR>/<locale>.html, the screenshot written as FAKE_CHROME says --
# "ok" a 1024x500 card, "small" a 1000x500 one, "none" nothing, "hang" the card and then no exit (Chrome's new headless
# mode, which writes the file and sits there).
CHROME = r'''
import shutil, signal
page = next(a for a in argv if a.startswith("file://"))[len("file://"):]
shot = next(a for a in argv if a.startswith("--screenshot="))[len("--screenshot="):]
locale = shot.split("/listings/")[1].split("/")[0]
shutil.copyfile(page, os.path.join(env["FAKE_DIR"], locale + ".html"))
with open(os.path.join(env["FAKE_DIR"], "chrome.pids"), "a") as f:
    f.write(str(os.getpid()) + "\n")
mode = env.get("FAKE_CHROME", "ok")
if mode != "none":
    with open(shot, "wb") as f:
        f.write(open(env["FAKE_PNG_SMALL" if mode == "small" else "FAKE_PNG"], "rb").read())
if mode == "hang":
    signal.pause()
'''
SIPS = 'shutil = __import__("shutil")\nshutil.copyfile(argv[argv.index("--out") - 1], argv[argv.index("--out") + 1])'


class StoreGraphicsCase(cli_harness.ToolCase):
    def setUp(self) -> None:
        super().setUp()
        self.layout = self.path("layout")
        copy_layout(self.layout)
        self.tool = os.path.join(self.layout, os.path.relpath(TOOL, cli_harness.ROOT))
        self.cards = self.path("cards")
        os.makedirs(self.cards)
        self.chrome = self.fakes.program("chrome", CHROME)
        self.fakes.program("sips", SIPS)
        self.fakes.script("sleep", ":")
        with open(self.path("card.png"), "wb") as f:
            f.write(png(1024, 500))
        with open(self.path("small.png"), "wb") as f:
            f.write(png(1000, 500))
        self.icon = png(512, 512, 2048)
        self.write_bytes(ICON, self.icon)
        for locale in LOCALES:
            self.listing(locale)

    def write_bytes(self, relative: str, data: bytes) -> None:
        full = os.path.join(self.layout, relative)
        os.makedirs(os.path.dirname(full), exist_ok=True)
        with open(full, "wb") as f:
            f.write(data)

    def listing(self, locale: str, shot: bool = True) -> None:
        brand = "Kinowo" if locale == "pl-PL" else "Showtimes"
        self.write_bytes(f"{LISTINGS}/{locale}/title.txt", f"{brand}: {locale} cinema listings\n".encode())
        self.write_bytes(f"{LISTINGS}/{locale}/short-description.txt", f"The {locale} store blurb\n".encode())
        if shot:
            self.write_bytes(f"{LISTINGS}/{locale}/graphics/phone-screenshots/001.png", png(1344, 2992))

    def generate(self, *locales: str, **env: str) -> cli_harness.Done:
        extra: Dict[str, str] = {"CHROME": self.chrome, "FAKE_DIR": self.cards, "FAKE_PNG": self.path("card.png"),
                                 "FAKE_PNG_SMALL": self.path("small.png"), "STORE_NAPS": "0"}
        extra.update(env)
        return self.run_tool(*locales, tool=self.tool, env=extra, bound=120)

    def card(self, locale: str) -> str:
        with open(os.path.join(self.cards, locale + ".html")) as f:
            return f.read()

    def out(self, locale: str, *parts: str) -> str:
        return os.path.join(self.layout, LISTINGS, locale, "graphics", *parts)


class EveryLocaleTest(StoreGraphicsCase):
    def test_every_locale_gets_its_card_and_the_one_icon(self) -> None:
        done = self.generate()
        self.assertEqual(0, done.code, done.output)
        self.assertIn("feature graphics + icon for every locale", done.stdout)
        self.assertEqual(sorted(LOCALES), sorted(f[:-5] for f in os.listdir(self.cards) if f.endswith(".html")))
        for locale in LOCALES:
            self.assertEqual((1024, 500), png_size(self.out(locale, "feature-graphic", "1.png")), locale)
            with open(self.out(locale, "icon", "1.png"), "rb") as f:
                self.assertEqual(self.icon, f.read(), locale)
            self.assertIn(f"  {locale} feature graphic… \033[32m✓\033[0m\n", done.stdout)
            self.assertRegex(done.stdout, re.escape(f"\033[32m✓\033[0m {locale} → {self.out(locale, 'feature-graphic', '1.png')}")
                             + r" \(1024×500, \d+KB\)\n")

    def test_the_locales_go_in_the_apps_order(self) -> None:
        done = self.generate()
        self.assertEqual(0, done.code, done.output)
        order = [m.group(1) for m in re.finditer(r"✓\x1b\[0m (\S+) → ", done.stdout)]
        self.assertEqual(LOCALES, order)

    def test_a_missing_listing_ends_the_run_naming_it(self) -> None:
        done = self.generate("de-DE", "xx-XX")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("\033[31m✗\033[0m no listing dir for 'xx-XX'\n", done.stderr)
        self.assertEqual((1024, 500), png_size(self.out("de-DE", "feature-graphic", "1.png")))

    def test_only_the_named_locales_are_generated(self) -> None:
        done = self.generate("es-ES")
        self.assertEqual(0, done.code, done.output)
        self.assertNotIn("every locale", done.stdout)
        self.assertEqual(["es-ES.html"], [f for f in os.listdir(self.cards) if f.endswith(".html")])
        self.assertFalse(os.path.exists(self.out("de-DE", "icon", "1.png")))


class CopyTest(StoreGraphicsCase):
    """The copy comes from the listing (the brand) and the one tagline table (never short-description.txt)."""

    def test_the_brand_is_the_listing_title_before_its_colon(self) -> None:
        self.assertEqual(0, self.generate("pl-PL", "en-GB", "de-DE").code)
        self.assertIn("<h1>Kinowo</h1>", self.card("pl-PL"))
        self.assertIn("<h1>Showtimes</h1>", self.card("en-GB"))
        self.assertIn("<h1>Showtimes</h1>", self.card("de-DE"))

    def test_each_language_has_its_own_tagline(self) -> None:
        self.assertEqual(0, self.generate().code)
        taglines = {locale: self.tagline(locale) for locale in LOCALES}
        self.assertEqual({"pl-PL": "Repertuar kin w Twoim mieście", "en-GB": ENGLISH,
                          "de-DE": "Kinoprogramm in deiner Stadt", "en-US": ENGLISH,
                          "es-ES": "Cartelera de cine en tu ciudad"}, taglines)
        self.assertNotIn(ENGLISH, self.card("de-DE"))

    def test_the_tagline_is_not_the_store_blurb(self) -> None:
        self.assertEqual(0, self.generate("de-DE").code)
        self.assertNotIn("store blurb", self.card("de-DE"))

    def tagline(self, locale: str) -> str:
        found = re.search(r"<p>(.*)</p>", self.card(locale))
        assert found, self.card(locale)
        return found.group(1)


class CardTest(StoreGraphicsCase):
    def test_the_canvas_is_plays_slot_and_the_tagline_one_line(self) -> None:
        self.assertEqual(0, self.generate("de-DE").code)
        card = self.card("de-DE")
        self.assertIn("width:1024px; height:500px", card)
        self.assertIn("white-space:nowrap", card)
        self.assertNotRegex(card, r"p  \{.*max-width")

    def test_filmweb_is_polands_pill_alone(self) -> None:
        self.assertEqual(0, self.generate().code)
        self.assertIn('class="l">FW</span><span class="v">7.4', self.card("pl-PL"))
        for locale in ("en-GB", "de-DE", "en-US", "es-ES"):
            self.assertNotIn("pill fw", self.card(locale), locale)

    def test_every_card_shows_real_ratings(self) -> None:
        self.assertEqual(0, self.generate().code)
        for locale in LOCALES:
            for want in ('class="l">IMDb</span><span class="v">7.9', 'class="pill solid">81',
                         'class="l">RT</span><span class="v">91%'):
                self.assertIn(want, self.card(locale), locale)

    def test_the_phone_is_the_locales_own_first_screenshot_downscaled(self) -> None:
        self.assertEqual(0, self.generate("de-DE").code)
        self.assertIn('<div class="phone"><img src="data:image/png;base64,', self.card("de-DE"))
        sips = self.fakes.calls("sips")
        self.assertEqual(1, len(sips))
        self.assertEqual(["-Z", "560", self.out("de-DE", "phone-screenshots", "001.png"), "--out"], sips[0][:4])

    def test_no_screenshot_yet_still_renders_a_card_without_a_phone(self) -> None:
        self.listing("pt-PT", shot=False)
        done = self.generate("pt-PT")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("\033[33m!\033[0m pt-PT: no 001.png yet — rendering the card without a phone\n", done.stderr)
        self.assertNotIn('class="phone"', self.card("pt-PT"))
        self.assertIn("<h1>Showtimes</h1>", self.card("pt-PT"))
        self.assertEqual([], self.fakes.calls("sips"))

    def test_the_card_carries_the_shared_icon_inline(self) -> None:
        self.assertEqual(0, self.generate("en-GB").code)
        self.assertIn('<img class="icon" src="data:image/png;base64,', self.card("en-GB"))


class RenderTest(StoreGraphicsCase):
    def test_a_chrome_that_never_exits_is_ended_once_the_card_is_written(self) -> None:
        done = self.generate("en-GB", "de-DE", FAKE_CHROME="hang")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual((1024, 500), png_size(self.out("de-DE", "feature-graphic", "1.png")))
        self.assertEqual(0, cli_harness.still_running(os.path.join(self.cards, "chrome.pids")))

    def test_a_card_of_another_size_is_refused(self) -> None:
        done = self.generate("en-GB", FAKE_CHROME="small")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("\033[31m✗\033[0m en-GB card is 1000×500; Play requires exactly 1024×500\n", done.stderr)

    def test_no_image_from_chrome_is_refused(self) -> None:
        done = self.generate("en-GB", FAKE_CHROME="none")
        self.assertEqual(1, done.code, done.output)
        self.assertRegex(done.stderr, re.escape(f"\033[31m✗\033[0m Chrome produced no image for "
                                                f"{self.out('en-GB', 'feature-graphic', '1.png')} after ") + r"\d+s\n")

    def test_a_stale_card_is_not_taken_for_a_new_one(self) -> None:
        self.write_bytes(f"{LISTINGS}/en-GB/graphics/feature-graphic/1.png", png(1024, 500))
        done = self.generate("en-GB", FAKE_CHROME="none")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("Chrome produced no image", done.stderr)

    def test_no_chrome_is_refused_naming_the_variable(self) -> None:
        done = self.generate(CHROME=self.path("no-chrome"))
        self.assertEqual(1, done.code, done.output)
        self.assertIn(f"\033[31m✗\033[0m no Chrome at {self.path('no-chrome')} — set CHROME=<path>\n", done.stderr)
        self.assertEqual([], os.listdir(self.cards))

    def test_no_shared_icon_is_refused(self) -> None:
        os.remove(os.path.join(self.layout, ICON))
        done = self.generate()
        self.assertEqual(1, done.code, done.output)
        self.assertIn(f"\033[31m✗\033[0m no shared icon at {os.path.join(self.layout, ICON)}\n", done.stderr)


class RepositoryTest(unittest.TestCase):
    """Drift guards over this checkout: what the tool's copies must still agree with."""

    def cards(self, locales: Sequence[str] = ("pl-PL", "de-DE", "en-GB")) -> Dict[str, str]:
        """The card each of `locales` is handed to Chrome as, by a run over a scratch layout listing them."""
        case = StoreGraphicsCase("run")
        case.setUp()
        try:
            for locale in locales:
                case.listing(locale)
            done = case.generate(*locales)
            self.assertEqual(0, done.code, done.output)
            return {locale: case.card(locale) for locale in locales}
        finally:
            case.tearDown()

    def card(self) -> str:
        return "".join(self.cards().values())

    def test_the_pills_are_theme_kts_colours(self) -> None:
        theme = read("android/app/src/main/java/pl/kinowo/ui/theme/Theme.kt")
        colour = {name: "#" + hex_ for name, hex_ in re.findall(r"val (\w+) = Color\(0xFF([0-9A-F]{6})\)", theme)}
        card = self.card()
        for name, css in (("ImdbYellow", ".imdb .l { background:{}"), ("MetaGood", "color:#0B0B12; background:{}"),
                          ("RtFresh", ".rt   .l { background:{}"), ("FwOrange", ".fw   .l { background:{}"),
                          ("FwOrangeLight", ".fw .v { color:{}"), ("CardElevated", "background:{}; }"),
                          ("CardSurface", "border:5px solid {}"), ("Background", "{};\n    font-family")):
            self.assertIn(css.replace("{}", colour[name]), card, name)

    def test_the_taglines_are_the_web_share_cards(self) -> None:
        og = read("web/src/page/scala/tools/OgCardGenerator.scala")
        for tagline in re.findall(r"<p>(.*)</p>", self.card()):
            self.assertIn(f'"{tagline}"', og)

    def test_every_listing_the_app_ships_has_its_own_tagline(self) -> None:
        # English is the FALLBACK, not a translation: es-ES shipped an English card until it had its own line
        shipped = sorted(os.listdir(os.path.join(cli_harness.ROOT, LISTINGS)))
        for locale, card in self.cards([locale for locale in shipped if locale != "en-GB"]).items():
            self.assertNotIn(f"<p>{ENGLISH}</p>", card, locale)

    def test_the_shared_icon_lives_once_outside_the_publishers_tree(self) -> None:
        self.assertTrue(os.path.isfile(os.path.join(cli_harness.ROOT, ICON)))
        self.assertNotIn("/listings/", ICON)
        self.assertNotIn("src/main/play/", ICON)

    def test_gitignore_commits_the_cards_and_keeps_the_icon_copies_out(self) -> None:
        lines = read("android/.gitignore").splitlines()
        self.assertIn("!app/src/main/play/listings/*/graphics/feature-graphic/", lines)
        self.assertEqual([], [line for line in lines if "graphics/icon/" in line and not line.lstrip().startswith("#")])

    def test_the_committed_cards_are_plays_size(self) -> None:
        for locale in ("pl-PL", "en-GB", "de-DE"):
            card = os.path.join(cli_harness.ROOT, LISTINGS, locale, "graphics/feature-graphic/1.png")
            self.assertEqual((1024, 500), png_size(card), locale)


def read(relative: str) -> str:
    with open(os.path.join(cli_harness.ROOT, relative), encoding="utf-8") as f:
        return f.read()


if __name__ == "__main__":
    unittest.main()
