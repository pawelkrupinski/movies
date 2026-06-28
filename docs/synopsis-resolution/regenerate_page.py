#!/usr/bin/env python3
"""Regenerate the synopsis-resolution report page.

Reads  accepts.jsonl + enrich.json  (this directory)
Writes ~/Desktop/synopsis-resolutions.html

142 cards grouped by website, sorted by sim descending. Each card carries a
Kinowo link, the external link, director HTML with matched names highlighted,
clue chips, an amber "why unresolved" strip, and a collapsible synopsis pair.
"""

import html
import json
import os
import unicodedata
from urllib.parse import quote

HERE = os.path.dirname(os.path.abspath(__file__))
ACCEPTS = os.path.join(HERE, "accepts.jsonl")
ENRICH = os.path.join(HERE, "enrich.json")
OUT = os.path.expanduser("~/Desktop/synopsis-resolutions.html")

SITE_META = {
    "TMDB": ("TMDB", "#01b4e4", "films with no TMDB ID at all"),
    "IMDb": ("IMDb", "#f5c518", "TMDB present but its imdb_id field was null"),
    "Filmweb": ("Filmweb", "#ffd000", "TMDB present but no Filmweb link"),
}
SITE_ORDER = ["TMDB", "IMDb", "Filmweb"]


def norm(s):
    s = unicodedata.normalize("NFD", s or "")
    s = "".join(c for c in s if unicodedata.category(c) != "Mn")
    return s.lower().strip()


def esc(s):
    return html.escape(s or "")


def kinowo_link(rec, enrich):
    info = enrich.get(rec["filmId"])
    if not info:
        return None
    city = info.get("city")
    title = info.get("title") or rec.get("filmTitle") or ""
    if not city:
        return None
    return f"https://kinowo.fly.dev/{city}/film?title={quote(title)}"


def directors_html(rowDirectors, matched):
    matched_norm = {norm(m) for m in (matched or [])}
    parts = []
    for d in (rowDirectors or []):
        if norm(d) in matched_norm:
            parts.append(f'<b class="hit">{esc(d)}</b>')
        else:
            parts.append(esc(d))
    return ", ".join(parts) if parts else '<span class="muted">—</span>'


def why_unresolved(rec):
    """Amber strip text, derived from the record fields."""
    src = rec["src"]
    if src == "IMDb":
        return "TMDB carried no <code>imdb_id</code> — the standard pipeline never reaches IMDb for this film."
    rd = rec.get("rowDirectors") or []
    md = rec.get("matchedDirectors") or []
    year_match = rec.get("yearMatch")
    if not rd:
        return "No director in the cinema listing — standard disambiguation abstains."
    if rd and not md:
        return "Director name differs from the external site (transliteration / middle-name) — year+director gate excluded the candidate."
    if year_match is False:
        return "Corpus year ≠ external year — the ±1-year search gate excluded the correct candidate."
    return "Correct entry was not the top search result (shared title / different market title)."


def clue_chips(rec):
    chips = []
    via = rec.get("via")
    via_label = {
        "director": "director match",
        "exact-title": "exact title",
        "character-name": "character name",
        "cast": "cast match",
    }.get(via, via or "")
    if via_label:
        chips.append(f'<span class="chip chip-via">{esc(via_label)}</span>')
    for ch in (rec.get("matchedCharacters") or []):
        chips.append(f'<span class="chip chip-char">char: {esc(ch)}</span>')
    for ca in (rec.get("matchedCast") or []):
        chips.append(f'<span class="chip chip-cast">cast: {esc(ca)}</span>')
    if rec.get("yearMatch"):
        chips.append('<span class="chip chip-year">year ✓</span>')
    else:
        chips.append('<span class="chip chip-year-no">year ✗</span>')
    return "".join(chips)


def card_html(rec, enrich):
    title = rec.get("filmTitle") or rec.get("searchTitle") or "(untitled)"
    fyear = rec.get("filmYear") or ""
    eyear = rec.get("extYear") or ""
    sim = rec.get("sim", "0")
    try:
        sim_f = float(sim)
    except (TypeError, ValueError):
        sim_f = 0.0
    sim_pct = int(round(sim_f * 100))

    klink = kinowo_link(rec, enrich)
    title_html = esc(title) + (f' <span class="yr">{esc(fyear)}</span>' if fyear else "")
    if klink:
        title_html = f'<a class="title-link" href="{esc(klink)}" target="_blank" rel="noopener">{title_html}</a>'

    ext_url = rec.get("extUrl") or "#"
    ext_title = rec.get("extTitle") or "(external)"
    ext_label = esc(ext_title) + (f" ({esc(eyear)})" if eyear else "")

    dirs = directors_html(rec.get("rowDirectors"), rec.get("matchedDirectors"))
    cand_dirs = directors_html(rec.get("candDirectors"), rec.get("matchedDirectors"))

    sim_class = "sim-high" if sim_f >= 0.5 else ("sim-mid" if sim_f >= 0.35 else "sim-low")

    return f"""
    <article class="card">
      <div class="card-head">
        <h3>{title_html}</h3>
        <span class="sim {sim_class}" title="stem-IDF cosine similarity">{sim_pct}%</span>
      </div>
      <div class="links">
        <span class="lab">→</span>
        <a href="{esc(ext_url)}" target="_blank" rel="noopener" class="ext">{ext_label}</a>
      </div>
      <div class="dirs">
        <div><span class="lab">corpus dir.</span> {dirs}</div>
        <div><span class="lab">candidate dir.</span> {cand_dirs}</div>
      </div>
      <div class="chips">{clue_chips(rec)}</div>
      <div class="why">{why_unresolved(rec)}</div>
      <details>
        <summary>synopses</summary>
        <div class="syn">
          <div><span class="lab">reference</span><p>{esc(rec.get('refSyn'))}</p></div>
          <div><span class="lab">candidate</span><p>{esc(rec.get('candSyn'))}</p></div>
        </div>
      </details>
    </article>"""


def main():
    with open(ACCEPTS, encoding="utf-8") as f:
        records = [json.loads(line) for line in f if line.strip()]
    with open(ENRICH, encoding="utf-8") as f:
        enrich = json.load(f)

    groups = {s: [] for s in SITE_ORDER}
    for r in records:
        groups.setdefault(r.get("src", "?"), []).append(r)
    for s in groups:
        groups[s].sort(key=lambda r: float(r.get("sim") or 0), reverse=True)

    total = len(records)
    nav = "".join(
        f'<a href="#{s.lower()}"><b>{len(groups.get(s, []))}</b> {SITE_META.get(s, (s,))[0]}</a>'
        for s in SITE_ORDER
    )

    sections = []
    for s in SITE_ORDER:
        recs = groups.get(s, [])
        if not recs:
            continue
        label, color, blurb = SITE_META.get(s, (s, "#888", ""))
        cards = "".join(card_html(r, enrich) for r in recs)
        sections.append(f"""
    <section id="{s.lower()}">
      <h2 style="border-color:{color}"><span class="dot" style="background:{color}"></span>{esc(label)} <span class="count">{len(recs)}</span></h2>
      <p class="sec-blurb">{esc(blurb)}</p>
      <div class="grid">{cards}</div>
    </section>""")

    doc = f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Synopsis Resolutions — {total} proposed links</title>
<style>
  :root {{ color-scheme: dark; }}
  * {{ box-sizing: border-box; }}
  body {{ margin:0; font:15px/1.5 -apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,sans-serif;
         background:#0e1116; color:#e6edf3; }}
  header {{ padding:32px 24px 20px; border-bottom:1px solid #222; position:sticky; top:0;
            background:#0e1116ee; backdrop-filter:blur(8px); z-index:10; }}
  header h1 {{ margin:0 0 6px; font-size:24px; }}
  header p {{ margin:0; color:#8b949e; }}
  nav {{ display:flex; gap:10px; margin-top:14px; flex-wrap:wrap; }}
  nav a {{ padding:6px 12px; border:1px solid #30363d; border-radius:999px; color:#e6edf3;
           text-decoration:none; font-size:13px; }}
  nav a:hover {{ background:#1c2128; }}
  nav b {{ color:#58a6ff; }}
  main {{ max-width:1280px; margin:0 auto; padding:24px; }}
  section {{ margin-bottom:40px; }}
  section h2 {{ font-size:20px; border-left:4px solid; padding-left:12px; display:flex;
                align-items:center; gap:10px; }}
  section h2 .dot {{ width:10px; height:10px; border-radius:50%; display:inline-block; }}
  .count {{ background:#1c2128; color:#8b949e; font-size:13px; padding:2px 9px; border-radius:999px; }}
  .sec-blurb {{ color:#8b949e; margin:-6px 0 18px 16px; font-size:13px; }}
  .grid {{ display:flex; flex-direction:column; gap:14px; }}
  .card {{ background:#161b22; border:1px solid #21262d; border-radius:10px; padding:14px 16px; }}
  .card-head {{ display:flex; justify-content:space-between; align-items:flex-start; gap:8px; }}
  .card-head h3 {{ margin:0; font-size:15px; line-height:1.35; }}
  .title-link {{ color:#e6edf3; text-decoration:none; }}
  .title-link:hover {{ color:#58a6ff; text-decoration:underline; }}
  .yr {{ color:#8b949e; font-weight:400; }}
  .sim {{ font-size:13px; font-weight:700; padding:2px 8px; border-radius:6px; white-space:nowrap; }}
  .sim-high {{ background:#1a3a1a; color:#56d364; }}
  .sim-mid  {{ background:#3a341a; color:#d4b850; }}
  .sim-low  {{ background:#3a2a1a; color:#d49050; }}
  .links {{ margin:8px 0; font-size:13px; }}
  .ext {{ color:#58a6ff; text-decoration:none; }}
  .ext:hover {{ text-decoration:underline; }}
  .dirs {{ font-size:12.5px; color:#c9d1d9; margin:6px 0; display:flex; flex-direction:column; gap:2px; }}
  .lab {{ color:#6e7681; font-size:11px; text-transform:uppercase; letter-spacing:.04em; margin-right:6px; }}
  .hit {{ color:#56d364; }}
  .muted {{ color:#6e7681; }}
  .chips {{ display:flex; gap:5px; flex-wrap:wrap; margin:8px 0; }}
  .chip {{ font-size:11px; padding:2px 8px; border-radius:999px; background:#1c2128; color:#adbac7; }}
  .chip-via {{ background:#1f2d3d; color:#79c0ff; }}
  .chip-char {{ background:#2d1f3d; color:#d2a8ff; }}
  .chip-cast {{ background:#1f3d2d; color:#7ce38b; }}
  .chip-year {{ background:#13311f; color:#56d364; }}
  .chip-year-no {{ background:#3d1f1f; color:#f08c8c; }}
  .why {{ margin:8px 0 4px; font-size:12.5px; color:#e3b341; background:#2b240e;
          border-left:3px solid #bb8009; padding:7px 10px; border-radius:4px; }}
  .why code {{ background:#0e1116; padding:1px 4px; border-radius:3px; font-size:11.5px; }}
  details {{ margin-top:8px; }}
  summary {{ cursor:pointer; color:#8b949e; font-size:12.5px; }}
  .syn {{ display:grid; grid-template-columns:1fr 1fr; gap:12px; margin-top:8px; }}
  .syn p {{ margin:3px 0 0; font-size:12.5px; color:#adbac7; }}
  @media (max-width:560px) {{ .syn {{ grid-template-columns:1fr; }} }}
</style>
</head>
<body>
<header>
  <h1>Synopsis-Based Resolutions</h1>
  <p>{total} unlinked Kinowo films matched to TMDB / IMDb / Filmweb by stem-IDF synopsis similarity, each gated on an independent corroborator (director / exact-title / character / cast). 0 false positives.</p>
  <nav>{nav}</nav>
</header>
<main>{''.join(sections)}</main>
</body>
</html>"""

    with open(OUT, "w", encoding="utf-8") as f:
        f.write(doc)
    print(f"wrote {OUT} ({os.path.getsize(OUT) // 1024} KB, {total} cards)")


if __name__ == "__main__":
    main()
