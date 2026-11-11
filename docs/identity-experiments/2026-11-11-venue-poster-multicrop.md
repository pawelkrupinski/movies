# 2026-11-11 — a venue poster hashed over seven crops (dropped)

**Aim.** Let the pHash vote (`PosterEvidence.vote`, <= 4 bits) match more venue posters whose art is the film's but
framed otherwise: a venue's banner trims, squashes or zooms TMDB's poster, and one 64-bit pHash of the whole image
then lands too far from every TMDB poster. The venue poster would be hashed seven ways -- base, trim (borders cut),
squash, left half, right half, zoom 0.9, zoom 0.8 -- and its distance to a film be the minimum over the seven.

**Research sample.** Over the poster research's venue-film pairs (the right film always among the candidates) it gave
+7 pts judged recall. That sample cannot show what a wider net costs where the right film is missing or has a
near-twin.

**Measured on prod's REAL candidate sets** (`~/.cache/movies-posters-research/prodval2-eval.py`, results
`prodval2-results.txt`; the population of the CLIP entry, `2026-11-11-clip-poster-margin.md`: 2,621 clusters, 371
judged, 1,782 unjudged matched ones whose prod take is the weak label). "A" is the multicrop, "B" a film's 16 TMDB
posters instead of 8:

| vote <= 4 bits | judged TP | judged FP | weak agree / disagree |
|---|---|---|---|
| baseline (8 posters, one crop) | 142 | 13 | 1066 / 6 |
| A: seven venue crops | 145 | 13 | 1099 / 7 |
| B: 16 film posters | 155 | 13 | 1149 / 5 |
| A+B | 158 | 13 | 1188 / 5 |

The veto on the taken film is unchanged on judged clusters (3 right takes wrongly vetoed, 1 wrong rightly vetoed under
every variant); weak prod takes vetoed 8 -> 10 under A.

A adds one new weak disagreement: PL "Chocolat" at Żuławski Ośrodek Kultury (biletyna.pl) votes TMDB 369778 where prod
took 392 (B and A+B vote the same).

**Verdict.** Dropped. +3 judged TP (142 -> 145) for seven times the venue hashing (each venue poster downloaded once
but shrunk and hashed seven times, its seven hashes filed), one new weak disagreement and two more weak vetoes. B
(16 posters a film) bought +13 TP offline yet moved no take on prod's data, and was dropped too
(`2026-11-11-film-posters-16.md`); A on top of B adds +3 TP more, still not worth seven venue hashes. Nothing was built in the worker; the crops live only in the research scripts.

**Angle worth trying next.** Crop only when the base hash misses every candidate by a margin (a venue banner's aspect
ratio far from a poster's 2:3), so the cost falls on the few banner-shaped posters rather than every venue poster.
