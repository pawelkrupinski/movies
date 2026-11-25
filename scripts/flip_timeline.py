#!/usr/bin/env python3
"""ONE LISTING'S FLIP TIMELINE (scripts/flip-timeline).

    flip_timeline.py <cc> --venue S --title S [--since 7d] [--dump FILE | --from FILE]
    flip_timeline.py <cc> --all [--since 7d] [--summary] [--tsv [FILE]]

One timeline, in time order, of what moved a listing's take — and, at the end, why each reversal (a take that went back
to the film it had left) happened:

  PROJ  every projection that wrote a take of it: its trigger (scrape / answer / film-change / boot / periodic / retry),
        start and end, its agreement phase, pod and build (VictoriaLogs `identity projection:` lines);
  TAKE  each take change (identity_history `take`): before -> after, stage, basis, decided_by and the trace's rules,
        the members of its decision (who joined or left), the cause prod logged, and `awaits`/`unanswered` when kept;
  ASK   every family ask filed for its title (identity_history `answer:identity_question_markers`, the agreement
        stage's question memory) and the record asks filed during a projection of it: family, filed or failed, inline
        (filed while a projection of it ran, from its agreement phase on: sent ~ the phase's start) or queued;
  LATE  an ask filed after the projection that needed it gave up waiting (its agreement phase's start +
        AgreementStage.AskNowWithin, 5 s) — the two-step write: that projection wrote without it, the next re-read it;
  LIST  a member listing (re-)published in the window (identity_history `listing`);
  HIST  the inline-ask histogram (kinowo_worker_identity_agreement_inline_ask_seconds) around a dropping projection,
        where it has samples.

REVERSALS classifies each X -> Y -> X of one listing, first match wins:
  DEPLOY/RESTART  a boot (`deploy`) projection on either side, or the build or the pod changed between them;
  LATE-ANSWER     the restoring projection was answer-triggered and an ask of the cluster was filed after the dropping
                  projection gave up waiting, before the restoring one began (inferred, and said so, for a poster
                  vote: poster answers are not kept in identity_history);
  SIBLING/POPULATION  the decision's members moved (a listing joined or left) or a member was re-published between them;
  RULE            none of the above: the same inputs, decided otherwise;
  UNCLASSIFIED    (never silent: it says why) no projection line for a take, or an answer-triggered restore with no ask
                  naming the listing's title.
--all reads every take change of the country (paged), keeps the reversals restored within 24 h and classifies each.

Reads the LOCAL prod mirror (:28017, read-only, after scripts/local-mirror/freshness), VictoriaLogs and Prometheus over
`ssh monitoring-1` (listing_history's helpers). `--dump FILE` keeps what was read, `--from FILE` prints from it (the
self-test's fixtures are such dumps). Tests: flip_timeline_test.py beside it. The modules: flip_timeline_model (what a take, a projection and an ask are),
flip_timeline_reversals (the classes), flip_timeline_gather and flip_timeline_queries (the reads), flip_timeline_render.
"""
from __future__ import annotations

import argparse
import datetime
import io
import json
import sys
from typing import TextIO

import flip_timeline_outside as outside
from flip_timeline_gather import Run, gather, gather_all
from flip_timeline_model import Json, ask_needle, iso as _iso, merge_logged, parse_projection, title_pattern
from flip_timeline_render import TSV_COLUMNS, render, render_all
from flip_timeline_reversals import CLASSES, analyse_all

__all__ = ["CLASSES", "TSV_COLUMNS", "_iso", "analyse_all", "ask_needle", "main", "merge_logged", "parse_args", "parse_projection",
           "render", "render_all", "title_pattern"]


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=(__doc__ or "").split("\n")[0])
    parser.add_argument("cc")
    parser.add_argument("--venue", default="")
    parser.add_argument("--title", default="")
    parser.add_argument("--all", action="store_true", help="every reversal of the country, classified (no --venue / --title)")
    parser.add_argument("--summary", action="store_true", help="with --all: the per-class count table only")
    parser.add_argument("--tsv", nargs="?", const="-", metavar="FILE", help="with --all: a row per reversal, to FILE (stdout when bare)")
    parser.add_argument("--since", default="7d")
    parser.add_argument("--dump", help="keep what was read in FILE (JSON)")
    parser.add_argument("--from", dest="source", help="print from a --dump FILE, reading nothing")
    args = parser.parse_args(argv)
    args.cc = args.cc.lower()
    if args.all and (args.venue or args.title):
        parser.error("--all reads the whole country: no --venue / --title")
    if not args.all and not (args.title or args.venue):
        parser.error("--venue and/or --title is required (or --all for the whole country)")
    return args


def _read_raw(args: argparse.Namespace, run: Run, now: float | None) -> Json:
    if args.source:
        with open(args.source, encoding="utf-8") as f:
            return json.load(f)
    gatherer = gather_all if args.all else gather
    raw = gatherer(args, run, now or datetime.datetime.now(datetime.timezone.utc).timestamp())
    if args.dump:
        with open(args.dump, "w", encoding="utf-8") as f:
            json.dump(raw, f, ensure_ascii=False, indent=1)
    return raw


def _print_all(raw: Json, args: argparse.Namespace) -> None:
    tsv: TextIO | None = None
    if args.tsv == "-":
        tsv = io.StringIO()
    elif args.tsv:
        tsv = open(args.tsv, "w", encoding="utf-8")
    sys.stdout.write(render_all(raw, summary=args.summary, tsv=tsv))
    if isinstance(tsv, io.StringIO):
        sys.stdout.write("\n" + tsv.getvalue())
    elif tsv:
        tsv.close()


def main(argv: list[str], run: Run = outside.run_command, now: float | None = None) -> int:
    args = parse_args(argv)
    raw = _read_raw(args, run, now)
    if raw.get("all"):
        _print_all(raw, args)
    else:
        sys.stdout.write(render(raw))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
