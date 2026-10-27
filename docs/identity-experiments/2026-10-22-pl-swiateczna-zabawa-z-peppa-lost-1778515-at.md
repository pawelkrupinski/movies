# 2026-10-22 — PL "Świąteczna zabawa z Peppą" lost 1778515 at 96fb23c6c: a feed's re-titling, not a commit (prod mirror, recordings 573 / 528)

- **Reported:** 123 PL listings (Cinema City ×35 "Świąteczna zabawa z Peppą", Helios ×52 "… – seanse z konkursami HDD",
  Multikino ×36 "… - Kinowe przeżycie") went tmdb 1778515 → none (NoCandidate, "joined by original-title") at
  10-21 11:30 under deploy 96fb23c6c; ab24cdcb1 / ed7a36648 were suspected.
- **Found:** `identity-slice pl --recording 573 --bisect f664df1ca..96fb23c6c` — no commit moves it; both recordings
  (10-20) bill Cinema City as "Festive Fun with Peppa Cinema Experience" (page `…/festive-fun-with-peppa-cinema-experience/8605d2r`),
  which f664df1's "Cinema Experience" tail strip let take 1778515 in prod (10-20 23:02). The take-change log
  (VictoriaLogs) shows the rest: 10-21 10:10 Helios and Multikino joined the Cinema City family (one CC venue already
  re-titled to the Polish name, its title equal to Helios' originalTitle) and took 1778515 via the 35 still-English CC
  billings; by 11:30 Cinema City had re-titled all 36 (same feed id `cc:8605d2r`, new page `…/swiateczna-zabawa-z-peppa/8605d2r`)
  and the family held no title TMDB knows: TMDB has no Polish title or alternative title for 1778515 (search pl-PL for
  the Polish title: 0 results), IMDb suggests nothing, Filmweb's only hit is an unrelated film. Every ref from
  f664df1 on would take none on the 11:30 data; the deploy only coincided.
- **Why no resolver fix:** the remaining routes are another country's take (UK/US/DE take 1778515) — the 10-19
  cross-country entry above found it unsafe and per-country by design — or remembering a feed id's earlier title across
  scrapes, which makes a take depend on scrape history no recording or slice replays. Not built.
- **Done instead:** PL `*` rows judging the four billings right for 1778515 (agent judged, flagged for the user), so
  a later change that takes it again counts as gained right. **Next angle:** a feed-id title history filed as a
  family answer (recordable, so replayable) would let a re-titled feed listing keep searching its earlier title.
