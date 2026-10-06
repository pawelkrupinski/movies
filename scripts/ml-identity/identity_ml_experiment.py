#!/usr/bin/env python3
"""Offline experiment: can matching rules be LEARNED (logistic regression, gradient-boosted trees) from the pinned
identity corpus, and how do they compare with today's resolver at ZERO wrong takes?

Input: the unified evidence model's contender rows, test/resources/fixtures/identity-unified/training.tsv.gz
(integration.IdentityUnifiedDataset: every cluster of the five recorded full corpora, every contender film, every
signal of the resolver's model and the agreement stage as UnifiedEvidence extracts them — the very features the
shipped fit, scripts.IdentityUnifiedFit, reads), labelled HAND (labels.tsv) or WEAK (today's model take on a matched
cluster no label judges).

Evaluation: per cluster the model takes its best-scoring contender when the score clears a threshold; a take is right
or wrong by the contender's label. Two hold-outs:
  * grouped 5-fold CV, groups = connected components of clusters joined by any film they hold a POSITIVE label for
    (so neither the same cluster nor the same right film sits in train and test);
  * leave-one-country-out.
Operating points: "oracle" (the threshold picked on the hold-out's own scores — an upper bound) and "honest" (the
threshold picked by an INNER grouped CV on the training part only: the lowest score above every inner wrong take,
then applied to the hold-out, whose wrong takes are counted as they fall).

Writes target/identity-ml/results.json, the LightGBM model dumps (for the JVM scorer bench) and prints a report.

    python3 -m venv <env> && <env>/bin/pip install scikit-learn lightgbm numpy
    timeout 1800 <env>/bin/python scripts/ml-identity/identity_ml_experiment.py
"""
import gzip
import json
import os
import sys
import time
from collections import defaultdict

import lightgbm as lgb
import numpy as np
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import roc_auc_score
from sklearn.preprocessing import StandardScaler

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
TRAINING = os.path.join(ROOT, "test/resources/fixtures/identity-unified/training.tsv.gz")
OUT = os.path.join(ROOT, "target/identity-ml")
SEED = 7

# the resolver's own features (the calibrated model's logit — itself the sum of the IdentityMeasures tables: title,
# originalTitle, year/titleYear/season delta, director, runtime, country, search rank, rivals, popularity, venues
# corroborating — plus the acceptance rules that fired and the model's vote); everything else is the agreement stage
RESOLVER = ["model.logit", "model.unscored", "model.deniedBySome", "rule.title", "rule.director", "rule.calibrated",
            "rule.imdb", "rule.stage", "rule.proposal", "rule.pooled", "model.lean", "model.best",
            "production.season", "production.house"]
# UnifiedEvidence.Signals' directions, for the monotone GBM (0: unconstrained)
NEGATIVE = {"model.deniedBySome", "family.turnedDown", "family.dissent", "listing.contradicts", "title.anothersOwn",
            "title.namesNone", "edition.apart", "bill.several", "stage.work", "poster.otherMatches"}
UNCONSTRAINED = {"model.unscored", "rule.proposal"}


def load():
    with gzip.open(TRAINING, "rt", encoding="utf-8") as f:
        lines = [l.rstrip("\n").split("\t") for l in f if l.strip()]
    header, rows = lines[0], lines[1:]
    features = header[13:]
    X = np.array([[float(v) for v in r[13:]] for r in rows])
    meta = [dict(country=r[0], cluster=(r[0], r[1]), origin=r[2], venue=r[3], listings=int(r[4]), raw=r[5], film=r[6],
                 filmTitle=r[7], today=r[8] == "1", label={"1": 1, "0": 0}.get(r[9]), source=r[10]) for r in rows]
    return features, X, meta


def groups_of(meta):
    """Union-find over clusters and the films they hold a positive label for: no right film, no cluster, on both sides."""
    parent = {}

    def find(a):
        parent.setdefault(a, a)
        while parent[a] != a:
            parent[a] = parent[parent[a]]
            a = parent[a]
        return a

    for m in meta:
        find(("c",) + m["cluster"])
        if m["label"] == 1:
            parent[find(("c",) + m["cluster"])] = find(("f", m["film"]))
    return [find(("c",) + m["cluster"]) for m in meta]


def folds_of(group_ids, k):
    """Deterministic group → fold, balanced by size (largest groups first, to the lightest fold)."""
    sizes = defaultdict(int)
    for g in group_ids:
        sizes[g] += 1
    load_ = [0] * k
    fold = {}
    for g, s in sorted(sizes.items(), key=lambda t: (-t[1], str(t[0]))):
        i = int(np.argmin(load_))
        fold[g] = i
        load_[i] += s
    return np.array([fold[g] for g in group_ids]), max(sizes.values())


# ── models ────────────────────────────────────────────────────────────────────────────────────

class Logistic:
    name = "logistic"

    def __init__(self, cols, c=1.0):
        self.cols, self.c = cols, c

    def fit(self, X, y, w):
        self.scaler = StandardScaler().fit(X[:, self.cols])
        self.m = LogisticRegression(C=self.c, max_iter=2000).fit(self.scaler.transform(X[:, self.cols]), y, sample_weight=w)
        return self

    def score(self, X):
        return self.m.predict_proba(self.scaler.transform(X[:, self.cols]))[:, 1]


class Gbm:
    def __init__(self, cols, features, monotone=False, rounds=300):
        self.cols, self.features, self.monotone, self.rounds = cols, features, monotone, rounds

    def params(self):
        p = dict(objective="binary", learning_rate=0.05, num_leaves=15, min_data_in_leaf=20, feature_fraction=0.9,
                 bagging_fraction=0.9, bagging_freq=1, lambda_l2=1.0, verbose=-1, seed=SEED, deterministic=True,
                 num_threads=4)
        if self.monotone:
            p["monotone_constraints"] = [0 if self.features[i] in UNCONSTRAINED else (-1 if self.features[i] in NEGATIVE else 1)
                                         for i in self.cols]
            p["monotone_constraints_method"] = "advanced"
        return p

    def fit(self, X, y, w):
        self.m = lgb.train(self.params(), lgb.Dataset(X[:, self.cols], y, weight=w,
                                                      feature_name=[self.features[i].replace(".", "_") for i in self.cols]),
                           num_boost_round=self.rounds)
        return self

    def score(self, X):
        return self.m.predict(X[:, self.cols])




# ── decisions ─────────────────────────────────────────────────────────────────────────────────

def cluster_index(meta, idx):
    by = defaultdict(list)
    for i in idx:
        by[meta[i]["cluster"]].append(i)
    return by


def best_takes(meta, idx, score):
    """Each cluster's best contender: (row, score)."""
    return [(max(rows, key=lambda i: score[i]), max(score[i] for i in rows)) for rows in cluster_index(meta, idx).values()]


class Mode:
    """Which clusters a decision is made for, and which takes count against the zero-wrong bar.
       strict: every cluster; any take of a contender labelled wrong.
       new:    every cluster; a wrong take today's stack does not already make (the bar the shipped fit uses).
       fill:   only the clusters today's stack takes nothing in; any wrong take (augmenting, never overriding)."""

    def __init__(self, name, meta, untaken):
        self.name, self.meta, self.untaken = name, meta, untaken

    def applies(self, i):
        return self.name != "fill" or self.meta[i]["cluster"] in self.untaken

    def wrong(self, i):
        m = self.meta[i]
        return m["label"] == 0 and not (self.name == "new" and m["today"])


def threshold(mode, takes, allowed):
    """The lowest cut leaving at most `allowed` counted-wrong takes: just above the (allowed+1)-th highest."""
    wrong = sorted((s for i, s in takes if mode.applies(i) and mode.wrong(i)), reverse=True)
    return float(np.nextafter(wrong[allowed], 2.0)) if len(wrong) > allowed else 0.0


def outcome(mode, takes, cut, subset=lambda m: True):
    """What the model's takes above `cut` do: right, wrong (any), counted-wrong, unlabelled, right takes today's stack
       does not make (gained), today's right takes it does not make (lost); clusters and listings."""
    meta = mode.meta
    o = defaultdict(int)
    for i, s in takes:
        m = meta[i]
        if not mode.applies(i) or not subset(m):
            continue
        if m["today_right"]:
            o["todayRight"] += 1
        taken = s >= cut
        if taken and m["label"] == 1:
            o["right"] += 1; o["rightListings"] += m["listings"]
            if not m["today"]:
                o["gained"] += 1; o["gainedListings"] += m["listings"]
        if m["today_right"] and not (taken and m["label"] == 1):
            o["lost"] += 1; o["lostListings"] += m["listings"]
        if taken and m["label"] == 0:
            o["wrong"] += 1; o["wrongListings"] += m["listings"]
            if mode.wrong(i):
                o["countedWrong"] += 1
        if taken and m["label"] is None:
            o["unlabelled"] += 1
    return o


def positives(meta, takes, mode, subset=lambda m: True):
    return sum(1 for i, _ in takes if mode.applies(i) and subset(meta[i]) and meta[i]["cluster_positive"])


def named(meta, i, s=None):
    m = meta[i]
    d = dict(country=m["country"], raw=m["raw"], film=m["filmTitle"], label=m["label"], source=m["source"], today=m["today"])
    if s is not None:
        d["score"] = round(float(s), 4)
    return d


# ── the experiment ────────────────────────────────────────────────────────────────────────────

POINTS = [("strict", 0), ("strict", 1), ("strict", 3), ("new", 0), ("new", 1), ("fill", 0), ("fill", 1)]


def run(features, X, meta):
    os.makedirs(OUT, exist_ok=True)
    n = len(meta)
    labelled = np.array([m["label"] is not None for m in meta])
    y = np.array([m["label"] or 0 for m in meta])
    groups = groups_of(meta)
    folds, biggest = folds_of(groups, 5)
    order = {g: k for k, g in enumerate(sorted(set(groups), key=str))}
    inner_assign = np.array([(order[g] * 2654435761) % 4 for g in groups])  # inner folds: still whole groups
    countries = sorted({m["country"] for m in meta})
    res_cols = [features.index(f) for f in RESOLVER if f in features]
    all_cols = list(range(len(features)))
    clusters = cluster_index(meta, range(n))
    positive_clusters = {c for c, rows in clusters.items() if any(meta[i]["label"] == 1 for i in rows)}
    today_right = {c for c, rows in clusters.items() if any(meta[i]["today"] and meta[i]["label"] == 1 for i in rows)}
    for m in meta:
        m["cluster_positive"] = m["cluster"] in positive_clusters
        m["today_right"] = m["cluster"] in today_right
    untaken = {c for c, rows in clusters.items() if not any(meta[i]["today"] for i in rows)}
    hand_clusters = {m["cluster"] for m in meta if m["source"] == "hand"}
    in_hand = lambda m: m["cluster"] in hand_clusters
    modes = {k: Mode(k, meta, untaken) for k in ("strict", "new", "fill")}
    weights = np.where([m["source"] == "hand" for m in meta], 5.0, 1.0)
    everything = np.arange(n)

    models = {
        "(b) logistic L2, resolver features": lambda: Logistic(res_cols),
        "(c) GBM, resolver features": lambda: Gbm(res_cols, features),
        "(d1) logistic L2, + agreement": lambda: Logistic(all_cols),
        "(d2) GBM, + agreement": lambda: Gbm(all_cols, features),
        "(e) monotone GBM, + agreement": lambda: Gbm(all_cols, features, monotone=True),
    }

    def fit_on(make, idx):
        t = time.perf_counter()
        idx = idx[labelled[idx]]
        model = make().fit(X[idx], y[idx], weights[idx])
        return model, time.perf_counter() - t

    def oof(make, train_idx, assign):
        s = np.zeros(n)
        for f in sorted(set(assign[train_idx])):
            tr, te = train_idx[assign[train_idx] != f], train_idx[assign[train_idx] == f]
            model, _ = fit_on(make, tr)
            s[te] = model.score(X[te])
        return s

    all_takes_today = [(i, 1.0) for rows in clusters.values() for i in rows if meta[i]["today"]]
    report = {"dataset": dict(
        rows=n, labelledRows=int(labelled.sum()), handRows=sum(m["source"] == "hand" for m in meta),
        weakRows=sum(m["source"] == "weak" for m in meta), unlabelledRows=int((~labelled).sum()), clusters=len(clusters),
        clustersWithPositive=len(positive_clusters), handClusters=len(hand_clusters),
        handClustersWithPositive=len(positive_clusters & hand_clusters), untakenClusters=len(untaken),
        untakenWithPositive=len(positive_clusters & untaken), todayRight=len(today_right), groups=len(set(groups)),
        biggestGroupRows=biggest, features=len(features),
        today=dict(right=sum(meta[i]["label"] == 1 for i, _ in all_takes_today),
                   wrong=[named(meta, i) for i, _ in all_takes_today if meta[i]["label"] == 0],
                   unlabelled=sum(meta[i]["label"] is None for i, _ in all_takes_today),
                   handRight=sum(meta[i]["label"] == 1 and in_hand(meta[i]) for i, _ in all_takes_today))),
        "models": {}, "countryHoldout": {}}
    print(json.dumps(report["dataset"], ensure_ascii=False), flush=True)

    for name, make in models.items():
        print(f"\n== {name}", flush=True)
        outer = np.zeros(n)
        honest = {p: defaultdict(int) for p in POINTS}
        honest_hand = defaultdict(int)
        train_seconds = []
        for f in range(5):
            tr, te = everything[folds != f], everything[folds == f]
            model, secs = fit_on(make, tr)
            train_seconds.append(secs)
            outer[te] = model.score(X[te])
            inner_takes = best_takes(meta, tr, oof(make, tr, inner_assign))
            te_takes = best_takes(meta, te, outer)
            for mode_name, allowed in POINTS:
                mode = modes[mode_name]
                cut = threshold(mode, inner_takes, allowed)
                for k, v in outcome(mode, te_takes, cut).items():
                    honest[(mode_name, allowed)][k] += v
                if (mode_name, allowed) == ("new", 0):
                    for k, v in outcome(mode, te_takes, cut, in_hand).items():
                        honest_hand[k] += v
        takes = best_takes(meta, everything, outer)
        hand_rows = np.array([m["source"] == "hand" for m in meta])
        entry = dict(trainSeconds=float(np.mean(train_seconds)), auc=float(roc_auc_score(y[labelled], outer[labelled])),
                     aucHand=float(roc_auc_score(y[hand_rows], outer[hand_rows])), points={})
        for mode_name, allowed in POINTS:
            mode = modes[mode_name]
            P = positives(meta, takes, mode)
            cut = threshold(mode, takes, allowed)
            o = outcome(mode, takes, cut)
            h = honest[(mode_name, allowed)]
            entry["points"][f"{mode_name}@{allowed}"] = dict(
                positives=P, oracle=dict(cut=cut, recall=o["right"] / P, **o), honest=dict(recall=h["right"] / P, **h))
        mode = modes["new"]
        Ph = positives(meta, takes, mode, in_hand)
        o = outcome(mode, takes, threshold(mode, takes, 0), in_hand)
        entry["hand.new@0"] = dict(positives=Ph, oracle=dict(recall=o["right"] / Ph, **o),
                                   honest=dict(recall=honest_hand["right"] / Ph, **honest_hand))
        # names: the wrong takes nearest the cuts, and what the model gains over today at them
        for mode_name in ("new", "fill"):
            mode = modes[mode_name]
            cut = threshold(mode, takes, 0)
            ranked = sorted(((s, i) for i, s in takes if mode.applies(i) and mode.wrong(i)), key=lambda t: -t[0])
            entry[f"{mode_name}.topWrong"] = [named(meta, i, s) for s, i in ranked[:10]]
            entry[f"{mode_name}.gained"] = [named(meta, i, s) for i, s in takes
                                            if mode.applies(i) and s >= cut and meta[i]["label"] == 1 and not meta[i]["today"]][:20]
            entry[f"{mode_name}.unlabelledTaken"] = [named(meta, i, s) for i, s in takes
                                                     if mode.applies(i) and s >= cut and meta[i]["label"] is None][:20]
        # reliability: held-out probability buckets vs observed rate (labelled rows)
        entry["reliability"] = []
        for lo, hi in [(0, .01), (.01, .1), (.1, .3), (.3, .5), (.5, .7), (.7, .9), (.9, .99), (.99, .999), (.999, 1.01)]:
            sel = labelled & (outer >= lo) & (outer < hi)
            if sel.sum():
                entry["reliability"].append(dict(lo=lo, hi=min(hi, 1.0), n=int(sel.sum()), mean=float(outer[sel].mean()),
                                                 observed=float(y[sel].mean())))
        # speed: a whole-corpus score, per cluster
        model, full_seconds = fit_on(make, everything)
        entry["trainAllSeconds"] = full_seconds
        t = time.perf_counter()
        for _ in range(5):
            model.score(X)
        per_corpus = (time.perf_counter() - t) / 5
        entry["inferenceCorpusMs"] = per_corpus * 1e3
        entry["inferencePerClusterUs"] = per_corpus / len(clusters) * 1e6
        if isinstance(model, Logistic):
            coef = model.m.coef_[0]
            entry["weights"] = sorted(((features[c], float(w)) for c, w in zip(model.cols, coef)), key=lambda t: -abs(t[1]))
            entry["modelBytes"] = 8 * (len(model.cols) * 3 + 1)  # weight, mean, scale per feature; intercept
        else:
            imp = model.m.feature_importance("gain")
            total = imp.sum()
            entry["importance"] = sorted(((features[c], float(g / total)) for c, g in zip(model.cols, imp)), key=lambda t: -t[1])
            slug = name.split()[0].strip("()")
            dump = os.path.join(OUT, f"gbm-{slug}.json")
            with open(dump, "w") as fh:
                json.dump(dict(features=[features[c] for c in model.cols], model=model.m.dump_model()), fh)
            # the scores the JVM evaluator must reproduce, in the rows' order
            np.savetxt(os.path.join(OUT, f"scores-{slug}.txt"), model.score(X), fmt="%.10f")
            entry["modelBytes"] = len(model.m.model_to_string().encode())
            entry["trees"] = model.m.num_trees()
        report["models"][name] = entry
        print(json.dumps(entry["points"]), flush=True)

        # leave one country out: the cut from the other four countries' grouped out-of-fold scores
        loco = {}
        for c in countries:
            tr = np.array([i for i in everything if meta[i]["country"] != c])
            te = np.array([i for i in everything if meta[i]["country"] == c])
            model, _ = fit_on(make, tr)
            s = np.zeros(n)
            s[te] = model.score(X[te])
            inner_takes = best_takes(meta, tr, oof(make, tr, folds))
            te_takes = best_takes(meta, te, s)
            loco[c] = {}
            for mode_name in ("strict", "new", "fill"):
                mode = modes[mode_name]
                P = positives(meta, te_takes, mode)
                o = outcome(mode, te_takes, threshold(mode, inner_takes, 0))
                loco[c][mode_name] = dict(positives=P, recall=o["right"] / max(P, 1), **o)
        report["countryHoldout"][name] = loco
        print(json.dumps(loco), flush=True)

    with open(os.path.join(OUT, "results.json"), "w") as fh:
        json.dump(report, fh, indent=1, ensure_ascii=False)
    print(f"\nwrote {os.path.join(OUT, 'results.json')}")
    return report


if __name__ == "__main__":
    features, X, meta = load()
    print(f"{len(meta)} rows, {len(features)} features", flush=True)
    run(features, X, meta)
