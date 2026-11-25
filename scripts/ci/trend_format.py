"""The text the trend tools print (scripts/ci/ci_trends.py): minutes and shares, the `report` aggregate and its per-run
rows, the `trend` table with its regressions, the `compare` rows, the per-leg markdown tables of the in-run summary, and
the JSON / CSV / text emitter every command ends in.

    from trend_format import mins, pct, format_report, format_trend, format_compare, leg_tables, emit
"""
from __future__ import annotations

import csv
import json
from typing import Any, Dict, List, Optional, Sequence, TextIO, Union

from trend_measures import (SETUP_CATEGORIES, SLOTS, bound_by, categorize, gc_summary, job_categories, job_resources)
from trend_records import CALLER_PREFIX, Record, as_record, iso, job_group, job_leg, listing, section
from trend_series import change
from trend_summary import metric_rows, summarize


def mins(seconds: Optional[float]) -> str:
    if seconds is None:
        return "-"
    return "%d:%02d" % (seconds // 60, round(seconds % 60)) if seconds >= 60 else "%ds" % round(seconds)


def pct(value: Optional[float]) -> str:
    return "-" if value is None else "%.0f%%" % (value * 100)


def format_value(name: str, value: Optional[float]) -> str:
    if value is None:
        return "-"
    if name.startswith(("cache:", "res:", "gc:")) or "share" in name:
        return pct(value)
    if "runner-min" in name or "peak" in name or "busy mean" in name:
        return "%.1f" % value
    return mins(value)


def _p50(summary: Record, category: str) -> float:
    return section(summary["categories"], category).get("p50") or 0


def format_runs(summary: Record) -> List[str]:
    """One row per run: its wall, what the critical path spent queued and executing, every job's queue summed."""
    lines = ["  %-12s %-7s %-8s %-11s %6s %6s %6s %6s %6s %6s  %s" % (
        "run", "sha", "change", "started", "wall", "c→end", "path q", "exec", "all q", "r-min", "critical path")]
    for row in summary["per_run"]:
        lines.append("  %-12s %-7s %-8s %-11s %6s %6s %6s %6s %6s %6.0f  %s" % (
            row["id"], (row["sha"] or "")[:7], row["change"], iso(row["start"])[5:16].replace("T", " "),
            mins(row["wall"]), mins(row["elapsed"]), mins(row["path_queue"]), mins(row["path_work"]),
            mins(row["queue_total"]), row["runner_min"] or 0, row["path"]))
    return lines


def _report_head(summary: Record, workflow: Optional[str], slots: int) -> List[str]:
    lines = ["%s: %d runs (%s)" % (workflow or "all workflows", summary["runs"],
                                   ", ".join(str(i) for i in summary["run_ids"][-12:])),
             "  wall p50 %s p90 %s max %s · runner-min p50 %.0f · peak jobs p50 %s" % (
                 mins(summary["wall"]["p50"]), mins(summary["wall"]["p90"]), mins(summary["wall"]["max"]),
                 summary["runner_min"]["p50"] or 0, summary["peak"]["p50"]),
             "  critical path p50: queue %s + work %s (p90: queue %s + work %s) · every job's queue summed p50 %s p90 %s"
             % (mins(summary["path_queue"]["p50"]), mins(summary["path_work"]["p50"]),
                mins(summary["path_queue"]["p90"]), mins(summary["path_work"]["p90"]),
                mins(summary["queue_total"]["p50"]), mins(summary["queue_total"]["p90"])),
             "  created to last job's end p50 %s p90 %s (wall plus the first job's queue)" % (
                 mins(summary["elapsed"]["p50"]), mins(summary["elapsed"]["p90"]))]
    if summary["sat_mean"]["n"]:
        lines.append("  account slots during the run: mean %.1f of %d busy, all %d busy %s of the time (p50)" % (
            summary["sat_mean"]["p50"], slots, slots, pct(summary["sat_full"]["p50"])))
    if summary["skewed_jobs"]:
        lines.append("  clock skew: %d jobs' GitHub timestamps were off; corrected from their steps" % summary["skewed_jobs"])
    if summary["cache"]:
        lines.append("  cache hit rates: " + ", ".join("%s %s" % (k, pct(v)) for k, v in summary["cache"].items()))
    return lines


def _category_table(summary: Record) -> List[str]:
    setup = sum(_p50(summary, c) for c in SETUP_CATEGORIES)
    work = sum(_p50(summary, c) for c in summary["categories"] if c not in SETUP_CATEGORIES)
    lines = ["", "  setup vs work (runner-min per run, p50): setup %.1f, work %.1f" % (setup / 60, work / 60),
             "  %-18s %8s %8s" % ("category", "p50", "p90")]
    for category, figures in summary["categories"].items():
        lines.append("  %-18s %8s %8s" % (category, mins(figures["p50"]), mins(figures["p90"])))
    return lines


def _bound_text(entry: Record) -> str:
    bound = entry["bound"] or ""
    if entry["notes"]:
        bound += ": " + ", ".join(entry["notes"])
    strong = ["r(dur,%s)=%.2f" % (k, v) for k, v in entry["corr"].items() if abs(v) >= 0.6]
    return bound + " · " + ", ".join(strong) if strong else bound


def _job_lines(name: str, entry: Record, steps: bool) -> List[str]:
    setup_s = sum(section(entry["categories"], c).get("p50") or 0 for c in SETUP_CATEGORIES)
    red = " (%d red)" % entry["red"] if entry["red"] else ""
    lines = ["  %-46s %3d %6s %6s %6s %6s %6s %6s  %s%s" % (
        name[:46], entry["dur"]["n"], mins(entry["dur"]["p50"]), mins(entry["dur"]["p90"]),
        mins(entry["dur"]["max"]), mins(entry["queue"]["p50"]), mins(entry["queue"]["p90"]), mins(setup_s),
        _bound_text(entry), red)]
    if steps:
        for step, figures in sorted(entry["steps"].items(), key=lambda item: -(item[1]["p50"] or 0)):
            if (figures["p50"] or 0) >= 15:
                lines.append("      %-62s %6s %6s  [%s]" % (step[:62], mins(figures["p50"]), mins(figures["p90"]),
                                                           categorize(step)))
    return lines


def _job_table(summary: Record, steps: bool) -> List[str]:
    lines = ["", "  %-46s %3s %6s %6s %6s %6s %6s %6s  %s" % ("job", "n", "p50", "p90", "max", "queue", "q p90",
                                                             "setup", "bound")]
    for name, entry in sorted(summary["jobs"].items(), key=lambda item: -(item[1]["dur"]["p50"] or 0)):
        lines += _job_lines(name, entry, steps)
    return lines


def format_report(summary: Record, workflow: Optional[str], critical: bool = False, steps: bool = True,
                  runs: bool = False, slots: int = SLOTS) -> str:
    lines = _report_head(summary, workflow, slots) + _category_table(summary) + _job_table(summary, steps)
    if runs:
        lines += ["", "  per run (wall: first step to last; c→end: created to last; path q/exec: the critical path's "
                      "queue and execution; all q: every job's queue summed):"] + format_runs(summary)
    if critical:
        lines += ["", "  critical paths (runs · queue p50 · work p50):"]
        for chain, count, queue, work in summary["paths"][:5]:
            lines.append("    %2d × %s + %s  %s" % (count, mins(queue), mins(work), chain))
    return "\n".join(lines)


def _signed_percent(value: Optional[float]) -> str:
    return "-" if value is None else "%+.0f%%" % (value * 100)


def _window_text(between: Record) -> str:
    before, after = between["last_before"], between["first_after"]
    return " -- started between run %s (%s, %s) and run %s (%s, %s)" % (
        before["run"], (before["sha"] or "")[:9], before["at"], after["run"], (after["sha"] or "")[:9], after["at"])


def _regression_line(flag: Record) -> str:
    where = _window_text(flag["between"]) if flag["between"] else ""
    if flag.get("machine_shift"):
        where += " -- lines up with a machine change: " + "; ".join(flag["machine_shift"])
    return "  ! %s: %s → %s (%+.0f%%)%s" % (flag["row"], format_value(flag["row"], flag["previous"]),
                                          format_value(flag["row"], flag["current"]), flag["change"] * 100, where)


def _trend_line(row: Record) -> str:
    present = [v for v in row["values"] if v is not None]
    return "%-60s %s %9s %9s %8s" % (
        row["row"][:60], row["spark"], format_value(row["row"], present[0] if present else None),
        format_value(row["row"], present[-1] if present else None), _signed_percent(row["change"]))


def format_trend(result: Record, limit: Optional[int] = None) -> str:
    lines = ["buckets (%s): %s" % (result["by"], " ".join("%s(%d)" % (b, n) for b, n in zip(result["buckets"], result["runs"]))),
             "%-60s %-*s %9s %9s %8s" % ("row", len(result["buckets"]), "trend", "first", "last", "change")]
    lines += [_trend_line(row) for row in (result["rows"][:limit] if limit else result["rows"])]
    if result["regressions"]:
        lines += ["", "REGRESSIONS against the previous range:"] + [_regression_line(f) for f in result["regressions"]]
    elif result.get("compared"):
        lines += ["", "No regression beyond the threshold against the previous range."]
    return "\n".join(lines)


def compare_rows(before: Sequence[Optional[Record]], after: Sequence[Optional[Record]]) -> List[Record]:
    before_rows = metric_rows(summarize(before))
    left = dict(before_rows)
    right = dict(metric_rows(summarize(after)))
    names = [name for name, _ in before_rows] + [name for name in right if name not in left]
    return [{"row": name, "before": left.get(name), "after": right.get(name),
             "change": change(left.get(name), right.get(name))} for name in names]


def format_compare(rows: Sequence[Record], before_n: int, after_n: int, everything: bool = False) -> str:
    lines = ["%-64s %9s %9s %8s" % ("row (before %d runs, after %d)" % (before_n, after_n), "before", "after", "change")]
    for row in rows:
        if not everything and row["row"].startswith("step:"):
            continue
        lines.append("%-64s %9s %9s %8s" % (row["row"][:64], format_value(row["row"], row["before"]),
                                            format_value(row["row"], row["after"]), _signed_percent(row["change"])))
    return "\n".join(lines)


def _machine_note(machine: Record, notes: Sequence[str], bound: Optional[str]) -> str:
    """The machine, then what pressed on the job, then what it was bound by."""
    return " · ".join(x for x in [
        "%s %sc" % (machine.get("cpu_model") or "?", machine.get("cores") or "?") if machine else "",
        ", ".join(notes), bound or ""] if x)


def _setup_by_category(setup: Dict[str, float]) -> str:
    if not setup:
        return ""
    return "(" + ", ".join("%s %s" % (k, mins(v)) for k, v in sorted(setup.items(), key=lambda kv: -kv[1])) + ")"


def _leg_row(job: Record) -> str:
    categories = job_categories(job)
    setup = {k: v for k, v in categories.items() if k in SETUP_CATEGORIES and v >= 1}
    work = sum(v for k, v in categories.items() if k not in SETUP_CATEGORIES)
    resources, _ = job_resources(job)
    bound, notes = bound_by(resources, gc_summary(job))
    note = _machine_note(section(job, "machine"), notes, bound)
    return "| %s | %s | %s %s | %s | %s | %s |" % (
        CALLER_PREFIX.sub("", job["n"]), mins(max(0, job["s"] - job["r"])), mins(sum(setup.values())),
        _setup_by_category(setup), mins(work), mins(job["e"] - job["s"]), note or "-")


def _leg_table(key: str, jobs: Sequence[Record]) -> List[str]:
    critical = max(jobs, key=lambda job: job["e"])
    wall = max(job["e"] for job in jobs) - min(job["s"] for job in jobs)
    return (["", "#### %s — wall %s, critical row: %s" % (key.replace("group: ", ""), mins(wall), critical["n"]),
             "", "| row | queue | setup (by category) | work | total | machine · GC · pressure |",
             "|---|---|---|---|---|---|"]
            + [_leg_row(job) for job in sorted(jobs, key=lambda job: -(job["e"] - job["s"]))])


def leg_tables(record: Record) -> str:
    """Markdown: one table per leg (convergence's countries; a workflow without legs gets one per job group of two or
    more rows): each row's queue, setup by category, work, total, machine, GC and pressure; the leg's wall-clock from
    its first row's start to its last row's end, and its critical (last-ending) row."""
    groups: Dict[str, List[Record]] = {}
    for job in record["jobs"]:
        leg = job_leg(job["n"], job.get("label"))
        groups.setdefault(leg or ("group: " + job_group(job["n"])), []).append(job)
    has_legs = any(not key.startswith("group: ") for key in groups)
    lines: List[str] = []
    for key, jobs in sorted(groups.items()):
        skipped = key.startswith("group: ") if has_legs else len(jobs) < 2
        if not skipped:
            lines += _leg_table(key, jobs)
    return "\n".join(lines) + ("\n" if lines else "")


def _csv_rows(data: Union[List[Record], Record], out: TextIO) -> None:
    writer = csv.writer(out)
    table: Record = {} if isinstance(data, list) else data
    rows: List[Any] = data if isinstance(data, list) else listing(table, "rows")
    first = as_record(rows[0]) if rows else None
    if first is None:
        return
    keys = list(first)
    if "values" in keys:
        writer.writerow(["row", *table["buckets"], "slope", "change"])
        for row in rows:
            writer.writerow([row["row"]] + row["values"] + [row["slope"], row["change"]])
    else:
        writer.writerow(keys)
        for row in rows:
            writer.writerow([row[key] for key in keys])


def emit(data: Union[List[Record], Record], fmt: str, text: str, out: TextIO) -> None:
    if fmt == "json":
        json.dump(data, out, indent=1, default=str)
        out.write("\n")
    elif fmt == "csv":
        _csv_rows(data, out)
    else:
        out.write(text + "\n")
