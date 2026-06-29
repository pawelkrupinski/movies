#!/usr/bin/env python3
"""Generate the missing-ratings report — one card per movie we tried to find a
rating for, with the corpus-vs-candidate data compared and the reason it does or
doesn't match.

Inputs (this directory):
  ratings-eval.jsonl   — every match attempt (accepted + abstained) per (film, source)
  rating-enrich.json   — filmId -> {title, year, city}

Writes ~/Desktop/missing-ratings-synopsis.html

Grouped by source (IMDb / Filmweb), sorted by similarity desc within each — same
shape as the resolutions page. Each card states whether the rating was found, or
the concrete reason the best candidate was rejected.
"""

import html
import json
import os
import unicodedata
from urllib.parse import quote

HERE = os.path.dirname(os.path.abspath(__file__))
EVALS = f"{HERE}/ratings-eval.jsonl"
ENRICH = f"{HERE}/rating-enrich.json"
OUT = os.path.expanduser("~/Desktop/missing-ratings-synopsis.html")

SOURCE_META = {"IMDb": ("#f5c518", "no IMDb rating"), "Filmweb": ("#ffd000", "no Filmweb rating")}
SOURCE_ORDER = ["IMDb", "Filmweb"]


def norm(s):
    s = unicodedata.normalize("NFD", s or "")
    return "".join(c for c in s if unicodedata.category(c) != "Mn").lower().strip()


def esc(s):
    return html.escape(str(s) if s is not None else "")


def klink(fid, enrich, fallback_title):
    info = enrich.get(fid)
    if not info or not info.get("city"):
        return None
    return f"https://kinowo.fly.dev/{info['city']}/film?title={quote(info.get('title') or fallback_title)}"


def directors_html(directors, matched):
    mn = {norm(m) for m in (matched or [])}
    if not directors:
        return '<span class="muted">—</span>'
    return ", ".join(f'<b class="hit">{esc(d)}</b>' if norm(d) in mn else esc(d) for d in directors)


def reason(r):
    """(category, text) — why we did or didn't find the rating, from the data."""
    rd, cd, md = r.get("rowDirectors") or [], r.get("candDirectors") or [], r.get("matchedDirectors") or []
    sim_pct = int(round(float(r["sim"]) * 100))
    if r["outcome"] == "accepted":
        via = r["via"]
        if via == "director":
            return ("found", f"Found — same director ({esc(', '.join(md))}), rating recoverable from this entry")
        return ("found", f"Found — {esc(via)} match, rating recoverable from this entry")
    # abstained — pick the most specific reason
    if not r.get("extTitle"):
        return ("none", "No candidate returned by the site’s search")
    if rd and cd and not md:
        return ("dirmis", f"Director mismatch — we have {esc(', '.join(rd))}, the candidate is {esc(', '.join(cd))}")
    if not rd:
        return ("nodir", "No director in our data, so the candidate can’t be corroborated")
    if not cd:
        return ("nocd", "Candidate lists no director, so it can’t be corroborated")
    if not r.get("yearMatch"):
        return ("year", f"Year differs ({esc(r.get('filmYear') or '—')} vs {esc(r.get('extYear') or '—')}) with no exact-title or director overlap")
    return ("weak", f"Too weak — synopsis only {sim_pct}% similar and no director/exact-title corroboration")


def card_html(r, enrich):
    fid = r["filmId"]
    info = enrich.get(fid, {})
    title = info.get("title") or r.get("filmTitle") or fid
    fyear = r.get("filmYear") or info.get("year") or ""
    title_html = esc(title) + (f' <span class="yr">{esc(fyear)}</span>' if fyear else "")
    lk = klink(fid, enrich, title)
    if lk:
        title_html = f'<a class="title-link" href="{lk}" target="_blank" rel="noopener">{title_html}</a>'

    sim_pct = int(round(float(r["sim"]) * 100))
    cat, why = reason(r)
    found = cat == "found"
    status = '<span class="sim sim-ok">found</span>' if found else f'<span class="sim sim-no">{sim_pct}%</span>'

    ext_label = esc(r.get("extTitle") or "(nothing found)")
    if r.get("extYear"):
        ext_label += f" ({esc(r['extYear'])})"
    ext_url = r.get("extUrl")
    ext_html = f'<a href="{esc(ext_url)}" target="_blank" rel="noopener" class="ext">{ext_label}</a>' if ext_url else ext_label

    has_syn = (r.get("refSyn") or r.get("candSyn"))
    syn = ""
    if has_syn:
        syn = f"""<details><summary>synopses compared</summary><div class="syn">
          <div><span class="lab">ours (reference)</span><p>{esc(r.get('refSyn'))}</p></div>
          <div><span class="lab">candidate</span><p>{esc(r.get('candSyn'))}</p></div></div></details>"""

    return f"""
    <article class="card">
      <div class="card-head"><h3>{title_html}</h3>{status}</div>
      <div class="links"><span class="lab">candidate</span> {ext_html}</div>
      <div class="dirs">
        <div><span class="lab">our dir.</span> {directors_html(r.get('rowDirectors'), r.get('matchedDirectors'))}</div>
        <div><span class="lab">candidate dir.</span> {directors_html(r.get('candDirectors'), r.get('matchedDirectors'))}</div>
      </div>
      <div class="why why-{cat}">{why}</div>
      {syn}
    </article>"""


def main():
    recs = [json.loads(l) for l in open(EVALS) if l.strip()]
    enrich = json.load(open(ENRICH))
    groups = {s: [] for s in SOURCE_ORDER}
    for r in recs:
        groups.setdefault(r["src"], []).append(r)
    for s in groups:
        # found first, then by similarity desc
        groups[s].sort(key=lambda r: (r["outcome"] != "accepted", -float(r["sim"])))

    total = len(recs)
    found = sum(1 for r in recs if r["outcome"] == "accepted")
    nav = "".join(
        f'<a href="#{s.lower()}"><b>{len(groups.get(s, []))}</b> {s}</a>' for s in SOURCE_ORDER if groups.get(s))

    sections = []
    for s in SOURCE_ORDER:
        rs = groups.get(s, [])
        if not rs:
            continue
        color, _ = SOURCE_META.get(s, ("#888", ""))
        f_n = sum(1 for r in rs if r["outcome"] == "accepted")
        cards = "".join(card_html(r, enrich) for r in rs)
        sections.append(f"""
    <section id="{s.lower()}">
      <h2 style="border-color:{color}"><span class="dot" style="background:{color}"></span>{esc(s)}
        <span class="count">{len(rs)} films · {f_n} found</span></h2>
      <div class="grid">{cards}</div>
    </section>""")

    doc = f"""<!DOCTYPE html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Missing ratings — match attempts</title>
<style>
  :root {{ color-scheme: dark; }}
  * {{ box-sizing: border-box; }}
  body {{ margin:0; font:15px/1.5 -apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,sans-serif; background:#0e1116; color:#e6edf3; }}
  header {{ padding:32px 24px 20px; border-bottom:1px solid #222; position:sticky; top:0; background:#0e1116ee; backdrop-filter:blur(8px); z-index:10; }}
  header h1 {{ margin:0 0 6px; font-size:24px; }}
  header p {{ margin:0; color:#8b949e; max-width:860px; }}
  nav {{ display:flex; gap:10px; margin-top:14px; flex-wrap:wrap; }}
  nav a {{ padding:6px 12px; border:1px solid #30363d; border-radius:999px; color:#e6edf3; text-decoration:none; font-size:13px; }}
  nav a:hover {{ background:#1c2128; }} nav b {{ color:#58a6ff; }}
  main {{ max-width:1100px; margin:0 auto; padding:24px; }}
  section {{ margin-bottom:40px; }}
  h2 {{ font-size:20px; border-left:4px solid; padding-left:12px; display:flex; align-items:center; gap:10px; }}
  h2 .dot {{ width:10px; height:10px; border-radius:50%; display:inline-block; }}
  .count {{ background:#1c2128; color:#8b949e; font-size:13px; padding:2px 9px; border-radius:999px; }}
  .grid {{ display:flex; flex-direction:column; gap:12px; }}
  .card {{ background:#161b22; border:1px solid #21262d; border-radius:10px; padding:13px 16px; }}
  .card-head {{ display:flex; justify-content:space-between; align-items:flex-start; gap:8px; }}
  .card-head h3 {{ margin:0; font-size:15px; line-height:1.35; }}
  .title-link {{ color:#e6edf3; text-decoration:none; }} .title-link:hover {{ color:#58a6ff; text-decoration:underline; }}
  .yr {{ color:#8b949e; font-weight:400; }}
  .sim {{ font-size:13px; font-weight:700; padding:2px 8px; border-radius:6px; white-space:nowrap; }}
  .sim-ok {{ background:#1a3a1a; color:#56d364; }}
  .sim-no {{ background:#3a2a1a; color:#d49050; }}
  .links {{ margin:8px 0; font-size:13px; }}
  .ext {{ color:#58a6ff; text-decoration:none; }} .ext:hover {{ text-decoration:underline; }}
  .dirs {{ font-size:12.5px; color:#c9d1d9; margin:6px 0; display:flex; flex-direction:column; gap:2px; }}
  .lab {{ color:#6e7681; font-size:11px; text-transform:uppercase; letter-spacing:.04em; margin-right:6px; }}
  .hit {{ color:#56d364; }} .muted {{ color:#6e7681; }}
  .why {{ margin:8px 0 4px; font-size:12.5px; padding:7px 10px; border-radius:4px; border-left:3px solid; }}
  .why-found {{ background:#13311f; border-color:#2ea043; color:#7ce38b; }}
  .why-dirmis {{ background:#3a1d1d; border-color:#d04545; color:#f0a0a0; }}
  .why-year {{ background:#2b240e; border-color:#bb8009; color:#e3b341; }}
  .why-nodir, .why-nocd, .why-weak {{ background:#21262d; border-color:#6e7681; color:#adbac7; }}
  .why-none {{ background:#21262d; border-color:#484f58; color:#8b949e; }}
  details {{ margin-top:8px; }}
  summary {{ cursor:pointer; color:#8b949e; font-size:12.5px; }}
  .syn {{ display:grid; grid-template-columns:1fr 1fr; gap:12px; margin-top:8px; }}
  .syn p {{ margin:3px 0 0; font-size:12.5px; color:#adbac7; }}
  @media (max-width:560px) {{ .syn {{ grid-template-columns:1fr; }} }}
</style></head><body>
<header>
  <h1>Missing ratings — match attempts</h1>
  <p>{total} films missing an IMDb or Filmweb rating that we could search for a match (a TMDB-linked entry exists to search from). Each card compares our data to the best candidate and gives the reason the rating was or wasn’t found. {found} found, {total - found} rejected — sorted by closeness within each source.</p>
  <nav>{nav}</nav>
</header>
<main>{''.join(sections)}</main>
</body></html>"""

    with open(OUT, "w", encoding="utf-8") as f:
        f.write(doc)
    print(f"wrote {OUT} ({os.path.getsize(OUT)//1024} KB) — {total} cards, {found} found")


if __name__ == "__main__":
    main()
