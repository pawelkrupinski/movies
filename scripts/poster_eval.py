#!/usr/bin/env python3
"""How strong a venue poster is as identity evidence, on prod's REAL candidate sets: the logic of scripts/poster-eval.

    scripts/poster-eval [--posters N] [--vote B] [--veto-match B] [--veto B] [--countries pl,uk] [--refresh]
    scripts/poster-eval coverage [--countries pl,uk] [--refresh]

THE DATASET (built once from the LOCAL prod mirror, kept in <cache>/dataset.npz, rebuilt when the mirror has synced more
than RefreshHours past it, or with --refresh): every model decision (`identity_model_families.decisions`) whose member
listings show a venue poster that counts (`PosterEvidence.shows`' source rule: no Webedia/Cineamo id, no kinoprogramm /
kinoheld page -- a feed's poster is its entry's; the stage-work and double-bill shapes are not reproduced), with its
REAL candidate set (every member's trace candidates not DENIED, plus the decision's film), prod's take, and the poster
answers prod filed (`identity_family_answers` poster|venue|<url>, poster|film|<id>). One row per listing for `coverage`.

A VARIANT is the poster count per film, the vote bits, the veto's match bits and the taken film's veto bits; prod's
(PosterEvidence: 8 / 4 / 8 / 10) is the baseline. A venue poster's hash is the one prod filed, else its image's; a film's
posters are the ones prod filed (its first N), else `PosterHashing.chosen` over TMDB's /images; above 8, the filed 8 plus
chosen N. A hash is production's bit for bit (poster_hash.py), cached by the image's sha1; an image or TMDB list not yet
cached is fetched (TMDB_API_KEY for the lists) -- 8 workers, a 429/503 halves them.

LABELS: identity-judgements.tsv (judged: the vote is TP when it names a film judged right, FP when it names a film
judged wrong or another than the right one), and prod's take for the rest (weak: agree / disagree). Never mix them:
the 2026-11-11 investigation's research sample (where the right film was always a candidate) gave ~99.4% precision; prod's
real sets, which often lack the right film or hold a near-twin, give ~92% (docs/design/poster-evidence-evaluation.md).
"""

from __future__ import annotations

import argparse
import os
import time
from typing import Dict, List, Optional, Sequence, Tuple

from poster_eval_common import Arrays, Baseline, Countries, Meta, Root, Variant, log
from poster_eval_dataset import build, dataset, load, save  # noqa: F401  (build, load, save: the names the tests import)
from poster_eval_images import Images, TmdbLists, chosen  # noqa: F401  (chosen: ditto)
from poster_eval_report import coverage, labels, report
from poster_eval_vote import Result, run_variant
import machine  # noqa: E402
from tool_files import Files  # noqa: E402
from tool_process import SubprocessRunner  # noqa: E402
from identity_judgements import COLUMNS as Columns, read as read_judgements  # noqa: E402,F401  (the names the tests import)

__all__ = ["Baseline", "Columns", "Images", "TmdbLists", "build", "chosen", "coverage", "labels", "load", "main", "read_judgements",
           "report", "run_variant", "save"]


def _parse(argv: Optional[Sequence[str]]) -> Tuple[argparse.Namespace, List[str]]:
    parser = argparse.ArgumentParser(prog="poster-eval", add_help=True)
    parser.add_argument("command", nargs="?", default="eval", choices=("eval", "coverage"))
    parser.add_argument("--posters", type=int, default=Baseline["posters"])
    parser.add_argument("--vote", type=int, default=Baseline["vote"])
    parser.add_argument("--veto-match", type=int, default=Baseline["veto_match"])
    parser.add_argument("--veto", type=int, default=Baseline["veto"])
    parser.add_argument("--countries", default=",".join(Countries))
    parser.add_argument("--refresh", action="store_true", help="re-extract the dataset from the mirror now")
    parser.add_argument("--cache", default=os.environ.get("POSTER_EVAL_CACHE", os.path.expanduser("~/.cache/movies-posters")))
    parser.add_argument("--judgements", default=os.path.join(Root, "test/resources/fixtures/identity-judgements.tsv"))
    args = parser.parse_args(argv)
    countries = [c.strip() for c in args.countries.split(",") if c.strip()]
    if unknown := set(countries) - set(Countries):
        parser.error(f"unknown countries {sorted(unknown)}")
    return args, countries


def _evaluate(args: argparse.Namespace, countries: List[str], meta: Meta, arrays: Arrays, rows: Sequence[object],
              images: Images, tmdb: TmdbLists) -> None:
    variant: Variant = {"posters": args.posters, "vote": args.vote, "veto_match": args.veto_match, "veto": args.veto}
    results: Dict[str, Result] = {"baseline": run_variant(meta, arrays, images, tmdb, Baseline)}
    if variant != Baseline:
        results["variant"] = run_variant(meta, arrays, images, tmdb, variant)
    keep = [i for i, c in enumerate(meta["clusters"]) if c["cc"] in countries]
    labs = labels(meta, rows)
    report(dict(meta, clusters=[meta["clusters"][i] for i in keep]), [labs[i] for i in keep],
           {k: (v[0][keep], v[1][keep], v[2][keep]) for k, v in results.items()}, variant)


def main(argv: Optional[Sequence[str]] = None) -> None:
    args, countries = _parse(argv)
    os.makedirs(args.cache, exist_ok=True)
    started = time.time()
    meta, arrays = dataset(args.cache, countries, args.refresh)
    rows = read_judgements(Files.here(), args.judgements)
    if args.command == "coverage":
        coverage(meta, rows)
    else:
        _evaluate(args, countries, meta, arrays, rows,
                  Images(args.cache, SubprocessRunner(), Files.here(), machine.environment()), TmdbLists(args.cache))
    log(f"poster-eval: {time.time() - started:.1f}s")


if __name__ == "__main__":
    main()
