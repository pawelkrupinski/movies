#!/usr/bin/env python3
"""RULE ABLATION: which identity and title rules move takes when switched off, over recorded corpora.

Each round resolves a plan of configurations (integration.IdentityRuleAblation, one sbt run per recording and chunk),
each configuration one BATCH of rules switched off together; a batch whose takes moved is split in two for the next
round, until each mover is one rule. A batch that moved nothing clears every rule in it at once. The moves are judged
by identity_measure.classify against the judgements, so every rule ends with: cure (a wrong take fixed), right-lost,
right->wrong, new-wrong, gained-right, and the unjudged rest, summed over every recording and country.

Rules (--kinds): every title rule in code (`title:<id>`, from the run's title-rules.tsv; each resolved only in the
countries it applies to), and, once the resolver can switch them off, every identity rule the baseline's trace names (`accept:`, `veto:`, `join:`, `apart:`), but not the
learned ones (`learned-*`: the refit's, not code's) nor `refused:`/`pooled:`/`format:` (reasons, a pooled rule is its
`accept:` rule, the format peel is no rule). A veto whose id carries a film or a feed (`...-namesake-1382513-is`) is
ablated as its family (`veto:...-namesake-*`).

Rounds are kept under <out>/round-N/<recording>/ and a finished configuration is never resolved again, so a stopped
run resumes where it stopped. Each sbt run takes the heavy-build lock (--lock) and runs niced.
"""

import argparse
import json
import os
import re
import subprocess
import sys
from collections import defaultdict

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
sys.path.insert(0, os.path.join(HERE, "ci"))
import identity_measure as measure  # noqa: E402
from identity_judgements import read as read_judgements  # noqa: E402

IDENTITY_KINDS = ("accept:", "veto:", "join:", "apart:")
HARMFUL = ("right-lost", "right->wrong", "new-wrong")


# ── which rules ──────────────────────────────────────────────────────────────────────────────────────────────────────

def family(rule):
    """The name a rule is ablated by: a veto naming a film or a feed by id is ablated as its whole family."""
    if not rule.startswith("veto:"):
        return rule
    m = re.match(r"^(veto:.*?)-(?:\d|p-https|cineamo-|tmdb-)", rule)
    return m.group(1) + "-*" if m else rule


def identity_rules(fired):
    """The identity rules to ablate, from the baseline's fired counts {rule: listings}: {name: listings}."""
    out = defaultdict(int)
    for rule, n in fired.items():
        if rule.startswith("pooled:"):  # a pooled rule is switched off as the acceptance rule it is
            rule = "accept:" + rule[len("pooled:"):]
        if not rule.startswith(IDENTITY_KINDS) or "learned-" in rule:
            continue
        out[family(rule)] += n
    return dict(out)


def read_title_rules(path):
    """{"title:<id>": set of country codes, or None for every country} from title-rules.tsv."""
    out = {}
    with open(path, encoding="utf-8") as f:
        for line in f:
            if line.strip():
                rid, _scope, countries = line.rstrip("\n").split("\t")
                out["title:" + rid] = None if countries == "*" else set(countries.split(","))
    return out


def read_fired(path):
    out = {}
    if os.path.exists(path):
        with open(path, encoding="utf-8") as f:
            for line in f:
                if line.strip():
                    rule, n = line.rstrip("\n").rsplit("\t", 1)
                    out[rule] = int(n)
    return out


# ── batches and bisection ────────────────────────────────────────────────────────────────────────────────────────────

def batches(rules, countries_of, fired, hot_size, cold_size, prefix="b"):
    """The first round's configurations: rules grouped by the countries they reach and whether any baseline listing
    fires them (a rule no listing fires rarely moves anything, so those go in larger batches), each group cut into
    batches. [{"id", "rules", "countries"}], countries None meaning every one."""
    groups = defaultdict(list)
    for rule in sorted(rules):
        cs = countries_of(rule)
        key = (rule.split(":", 1)[0], ",".join(sorted(cs)) if cs else "*", bool(fired.get(rule)))
        groups[key].append(rule)
    out = []
    for (kind, cs, hot), rs in sorted(groups.items()):
        size = hot_size if hot else cold_size
        for i in range(0, len(rs), size):
            out.append({"id": f"{prefix}{len(out) + 1:03d}", "rules": rs[i:i + size],
                        "countries": None if cs == "*" else cs.split(",")})
    return out


def halves(config):
    rs = config["rules"]
    mid = len(rs) // 2
    return [dict(config, id=config["id"] + "a", rules=rs[:mid]), dict(config, id=config["id"] + "b", rules=rs[mid:])]


def next_round(results, configs):
    """(the next round's configurations, the rules settled this round {rule: outcome}, interactions [(rules, outcome)]).
    A batch that moved nothing settles each of its rules as zero; a single rule that moved settles as its outcome; a
    larger batch that moved splits in two. Two halves that both moved nothing while their parent moved are an
    INTERACTION (rules standing in for one another): settled as such, never split further."""
    nxt, settled, interactions = [], {}, []
    by_id = {c["id"]: c for c in configs}
    for c in configs:
        out = results[c["id"]]
        if total(out) == 0:
            for r in c["rules"]:
                settled[r] = out
        elif len(c["rules"]) == 1:
            settled[c["rules"][0]] = out
        else:
            nxt += halves(c)
    # a parent split last round whose two halves both moved nothing
    for c in configs:
        if c["id"].endswith("a") and c["id"][:-1] + "b" in by_id:
            sib = by_id[c["id"][:-1] + "b"]
            if total(results[c["id"]]) == 0 and total(results[sib["id"]]) == 0 and c.get("parentMoved"):
                interactions.append((c["rules"] + sib["rules"], c["parentMoved"]))
    for n in nxt:
        n["parentMoved"] = results[n["id"][:-1]]
    return nxt, settled, interactions


# ── outcomes ─────────────────────────────────────────────────────────────────────────────────────────────────────────

def empty_outcome():
    return {b: 0 for b in measure.BUCKETS} | {"moved": 0, "where": {}, "examples": []}


def total(outcome):
    return outcome["moved"]


def add(outcome, cc_label, buckets):
    n = sum(len(ms) for ms in buckets.values())
    outcome["moved"] += n
    if n:
        outcome["where"][cc_label] = outcome["where"].get(cc_label, 0) + n
    for b, ms in buckets.items():
        outcome[b] += len(ms)
        for m in ms:
            if b != "unjudged" and len(outcome["examples"]) < 6:
                outcome["examples"].append(f"{cc_label} {b}: {m['venue']} | {m['rawTitle']}: {m['before']} -> {m['after']}")


def judge_moves(path, cc, rows):
    """identity_measure's buckets for one moves-<cc>.jsonl: each move's baseline and head take, judged."""
    base, head = [], []
    with open(path, encoding="utf-8") as f:
        for line in f:
            if not line.strip():
                continue
            m = json.loads(line)
            for side, out in (("base", base), ("head", head)):
                out.append({"key": m["key"], "venue": m["venue"], "rawTitle": m["rawTitle"], "film": m[side]["film"],
                            "filmTitle": m[side]["filmTitle"], "basis": m[side]["basis"]})
    return measure.classify(cc, base, head, rows)


def harm_only(outcome):
    """Does switching the rule off fix wrong takes and cost no right one: the rule only ever does harm?"""
    return outcome["cure"] > 0 and all(outcome[b] == 0 for b in HARMFUL) and outcome["right->right"] == 0


# ── running ──────────────────────────────────────────────────────────────────────────────────────────────────────────

def recording_countries(pair):
    return sorted(d[len("enrichment-"):] for d in os.listdir(pair) if d.startswith("enrichment-"))


def run_plan(repo, recording, pair, configs, out, lock, chunk, log):
    """Resolve `configs` over one recording, `chunk` configurations per sbt run (each run takes the lock anew)."""
    ccs = recording_countries(pair)
    todo = [c for c in configs if not all(os.path.exists(os.path.join(out, c["id"], f"done-{cc}"))
                                          for cc in reach(c, ccs))]
    chunks = [todo[i:i + chunk] for i in range(0, len(todo), chunk)] or ([[]] if not os.path.exists(os.path.join(out, "base")) else [])
    for i, part in enumerate(chunks):
        wanted = sorted({cc for c in part for cc in reach(c, ccs)}) or ccs
        os.makedirs(out, exist_ok=True)
        plan = os.path.join(out, f"plan-{i}.json")
        with open(plan, "w", encoding="utf-8") as f:
            json.dump({"configs": [{"id": c["id"], "rules": c["rules"], **({"countries": c["countries"]} if c["countries"] else {})}
                                   for c in part]}, f)
        command = (f"worker/IntegrationTest/runMain integration.IdentityRuleAblation --plan {plan} --out {out} "
                   f"--full {','.join(wanted)} --corpus-dir {pair}/corpus --fixtures {pair}")
        argv = gate(lock) + sbt_argv(repo, command)
        print(f"[ablation] {recording}: chunk {i + 1}/{len(chunks)}, {len(part)} configurations, countries {','.join(wanted)}", flush=True)
        with open(log, "a", encoding="utf-8") as f:
            status = subprocess.call(argv, cwd=repo, stdout=f, stderr=subprocess.STDOUT)
        if status != 0:
            raise SystemExit(f"[ablation] {recording}: sbt run failed (exit {status}), log {log}")


def sbt_argv(repo, command):
    """The sbt run of `command`: against the MongoDB MONGODB_URI names when one is set (CI's), else in a mongod of its
    own (scripts/it-local.sh, the laptop's), niced either way."""
    if os.environ.get("MONGODB_URI"):
        return ["nice", "-n", "10", "sbt", command]
    return ["nice", "-n", "10", os.path.join(repo, "scripts", "it-local.sh"), command]


def shard_of(configs, shard):
    """The configurations of shard "I/N": every N-th, from the I-th (0-based); all for none."""
    if not shard:
        return configs
    i, n = (int(x) for x in shard.split("/"))
    return [c for k, c in enumerate(configs) if k % n == i]


def merge(states):
    """One state of several shards' (each settling its own rules): their settled rules, interactions and fired counts."""
    out = {"round": max((s.get("round", 0) for s in states), default=0), "settled": {}, "interactions": [], "pending": [], "fired": {}}
    for s in states:
        out["settled"].update(s.get("settled", {}))
        out["interactions"] += s.get("interactions", [])
        out["fired"].update(s.get("fired", {}))
        out["pending"] += s.get("pending") or []
    return out


def run_recordings(repo, recordings, configs, round_dir, lock, chunk, out):
    """run_plan over every recording, ONE AFTER ANOTHER: two sbt runs in one checkout collide on its server socket
    ("ServerAlreadyBootingException: Address already in use") and its compile output."""
    for rec, pair in recordings.items():
        run_plan(repo, rec, pair, configs, os.path.join(round_dir, rec), lock, chunk, os.path.join(out, f"sbt-{rec}.log"))


def gate(lock):
    """What an sbt run is wrapped in: an executable --lock is a slot runner (`<runner> <command...>`), any other path a
    lockf(1) file, none nothing."""
    if not lock:
        return []
    return [lock] if os.path.isfile(lock) and os.access(lock, os.X_OK) else ["lockf", "-t", "7200", lock]


def reach(config, ccs):
    return [cc for cc in ccs if not config.get("countries") or cc in config["countries"]]


def outcomes(configs, round_dir, recordings, rows):
    """{config id: outcome} summed over every recording and the countries each configuration reaches."""
    out = {c["id"]: empty_outcome() for c in configs}
    for rec, pair in recordings.items():
        ccs = recording_countries(pair)
        for c in configs:
            for cc in reach(c, ccs):
                path = os.path.join(round_dir, rec, c["id"], f"moves-{cc}.jsonl")
                if not os.path.exists(path):
                    raise SystemExit(f"[ablation] {path} missing: the run did not finish")
                add(out[c["id"]], f"{rec}/{cc}", judge_moves(path, cc, rows))
    return out


def determinism(round_dir, recordings):
    """Moves of the second baseline: any is noise the whole ablation would read as moves."""
    noisy = []
    for rec, pair in recordings.items():
        for cc in recording_countries(pair):
            path = os.path.join(round_dir, rec, "base-again", f"moves-{cc}.jsonl")
            if os.path.exists(path) and os.path.getsize(path) > 0:
                noisy.append(f"{rec}/{cc}")
    return noisy


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    p.add_argument("--recordings", default="38713274617,573,528")
    p.add_argument("--out", required=True)
    p.add_argument("--judgements", default=None)
    p.add_argument("--lock", default=os.environ.get("KINOWO_HEAVY_LOCK", ""))
    p.add_argument("--hot", type=int, default=8, help="batch size of rules a baseline listing fires")
    p.add_argument("--cold", type=int, default=48, help="batch size of rules none fires")
    p.add_argument("--chunk", type=int, default=25, help="configurations per sbt run")
    p.add_argument("--only", default="", help="comma-separated rule names: ablate these alone")
    p.add_argument("--kinds", default="title", help="the kinds of rule ablated: title (and, once the resolver can switch them "
                   "off, accept, veto, join, apart)")
    p.add_argument("--shard", default="", help="I/N: bisect only every N-th first-round batch, from the I-th (a CI row's)")
    p.add_argument("--merge", nargs="*", help="state.json files of several shards: write their one report to --out, run nothing")
    a = p.parse_args(argv)
    if a.merge is not None:
        state = merge([json.load(open(path, encoding="utf-8")) for path in a.merge])
        os.makedirs(a.out, exist_ok=True)
        with open(os.path.join(a.out, "state.json"), "w", encoding="utf-8") as f:
            json.dump(state, f, indent=1)
        write_report(state, os.path.join(a.out, "report.md"))
        print(f"[ablation] merged {len(a.merge)} shards: {len(state['settled'])} rules settled, {len(state['pending'])} unsettled")
        return 0 if not state["pending"] else 1
    repo = os.path.dirname(HERE)
    cache = os.environ.get("KINOWO_IDENTITY_CACHE", os.path.expanduser("~/.cache/kinowo-identity"))
    recordings = {r: os.path.join(cache, "recordings", r, "pair", "test", "resources", "fixtures") for r in a.recordings.split(",")}
    rows = read_judgements(a.judgements or os.path.join(repo, "test", "resources", "fixtures", "identity-judgements.tsv"))
    os.makedirs(a.out, exist_ok=True)
    state_path = os.path.join(a.out, "state.json")
    state = json.load(open(state_path)) if os.path.exists(state_path) else {"round": 0, "settled": {}, "interactions": [], "pending": None}

    def save():
        with open(state_path + ".tmp", "w", encoding="utf-8") as f:
            json.dump(state, f, indent=1)
        os.replace(state_path + ".tmp", state_path)

    # round 0: the baselines alone (their fired rules choose the batches; base-again proves the replay deterministic)
    r0 = os.path.join(a.out, "round-0")
    run_recordings(repo, recordings, [], r0, a.lock, a.chunk, a.out)
    noisy = determinism(r0, recordings)
    if noisy:
        raise SystemExit(f"[ablation] the baseline moved on a second resolve in {noisy}: the replay is not deterministic")
    if state["pending"] is None:
        fired, title_countries = defaultdict(int), {}
        for rec in recordings:
            title_countries = read_title_rules(os.path.join(r0, rec, "title-rules.tsv"))
            for cc in recording_countries(recordings[rec]):
                for rule, n in read_fired(os.path.join(r0, rec, "base", f"fired-{cc}.tsv")).items():
                    fired[rule] += n
        rules = dict(title_countries) if "title" in a.kinds.split(",") else {}
        rules.update({r: None for r in identity_rules(fired) if r.split(":", 1)[0] in a.kinds.split(",")})
        if a.only:
            rules = {r: rules.get(r) for r in a.only.split(",")}
        idfired = identity_rules(fired)
        state["fired"] = {r: fired.get(r, 0) or idfired.get(r, 0) for r in rules}
        state["pending"] = shard_of(batches(rules, lambda r: rules[r], state["fired"], a.hot, a.cold), a.shard)
        state["round"] = 1
        save()
    while state["pending"]:
        configs = state["pending"]
        rd = os.path.join(a.out, f"round-{state['round']}")
        print(f"[ablation] round {state['round']}: {len(configs)} configurations, {sum(len(c['rules']) for c in configs)} rules", flush=True)
        run_recordings(repo, recordings, configs, rd, a.lock, a.chunk, a.out)
        noisy = determinism(rd, recordings)
        if noisy:
            raise SystemExit(f"[ablation] round {state['round']}: the baseline moved on a second resolve in {noisy}")
        results = outcomes(configs, rd, recordings, rows)
        nxt, settled, interactions = next_round(results, configs)
        state["settled"].update(settled)
        state["interactions"] += [{"rules": rs, "outcome": o} for rs, o in interactions]
        state["pending"] = nxt
        state["round"] += 1
        save()
    write_report(state, os.path.join(a.out, "report.md"))
    print(f"[ablation] done: {len(state['settled'])} rules settled, report {os.path.join(a.out, 'report.md')}")
    return 0


def write_report(state, path):
    settled, fired = state["settled"], state.get("fired", {})
    movers = sorted((r for r, o in settled.items() if total(o)), key=lambda r: -settled[r]["moved"])
    zero = sorted(r for r, o in settled.items() if not total(o))
    harm = [r for r in movers if harm_only(settled[r])]
    cols = ["moved", "cure", "gained-right", "right-lost", "right->wrong", "new-wrong", "to-right", "unjudged", "gained-unjudged"]
    lines = [f"rules settled: {len(settled)}; move takes: {len(movers)}; move none: {len(zero)}; only harm: {len(harm)}", "",
             "| rule | fired | " + " | ".join(cols) + " |", "|" + "---|" * (len(cols) + 2)]
    for r in movers:
        o = settled[r]
        lines.append(f"| `{r}` | {fired.get(r, 0)} | " + " | ".join(str(o[c]) for c in cols) + " |")
    lines += ["", "## Only harm", ""] + [f"- `{r}`: " + "; ".join(settled[r]["examples"][:3]) for r in harm]
    lines += ["", "## Interactions", ""] + [f"- {', '.join('`' + r + '`' for r in i['rules'])}: moved {i['outcome']['moved']}"
                                           for i in state["interactions"]]
    lines += ["", "## Move nothing", ""] + [f"- `{r}` (fired {fired.get(r, 0)})" for r in zero]
    with open(path, "w", encoding="utf-8") as f:
        f.write("\n".join(lines) + "\n")


if __name__ == "__main__":
    sys.exit(main())
