#!/usr/bin/env python3
"""Experiment `weighted-guards` (docs/identity-experiments.md): do the guard signals separate right from wrong as WEIGHTS?

Reads the labelled units `scripts.IdentityGuardSignals` (captures: `units.tsv`, hand labels) or
`integration.IdentityGuardUnits` (whole recorded corpora: `units-<cc>.tsv`, weak labels = the node's take, judgements
overriding) write, and for each variant (each signal alone, all together) fits every level's weight as the calibration does
(log-likelihood ratio, Alpha 0.5, a missing side neutral) on four folds grouped by film, adds it to the shipped model's
log-odds, Platt-scales on the same folds, and scores the held-out fold: AUC, log-loss, Brier, and the share of nodes whose
best-scored candidate is the right one -- against the shipped log-odds alone. Pure Python, no dependencies.

  python3 scripts/ml-identity/weighted_guards.py capture target/weighted-guards/units.tsv
  python3 scripts/ml-identity/weighted_guards.py corpus target/weighted-guards/units-*.tsv
  python3 scripts/ml-identity/weighted_guards.py weights <variant> <signal,...> target/weighted-guards/units-*.tsv
      writes identity/src/main/resources/identity-weights.json with each named signal's table fitted from the corpus units
"""
import csv
import math
import sys
import zlib
from collections import Counter, defaultdict

ALPHA = 0.5
FOLDS = 5
JUDGEMENTS = "test/resources/fixtures/identity-judgements.tsv"


def spaced(s):
    return " ".join(s.split()).lower()


def judgements():
    out = defaultdict(list)  # (cc, rawTitle) -> [(venue, film, right)]
    with open(JUDGEMENTS, encoding="utf-8") as f:
        for r in csv.DictReader((l for l in f if not l.startswith("#")), delimiter="\t"):
            if r.get("verdict") not in ("right", "wrong"):
                continue
            out[(r["country"], spaced(r["rawTitle"]))].append((r["venue"], r["film"], r["verdict"] == "right"))
    return out


def capture_units(path):
    signals = ["release", "certificate", "poster", "synopsis"]
    for r in csv.DictReader(open(path, encoding="utf-8"), delimiter="\t"):
        yield dict(group=r["group"], node=r["country"] + r["node"], label=r["same"] == "true", logit=float(r["logit"]),
                   levels={s: r[s] for s in signals}, judged=True, taken=False)


def corpus_units(paths):
    labels = judgements()
    for path in paths:
        by_node = defaultdict(list)
        for r in csv.DictReader(open(path, encoding="utf-8"), delimiter="\t", quoting=csv.QUOTE_NONE):
            if None in r or r.get("certificate") is None or not r["take"].isdigit() and r["take"] != "":
                continue  # a raw title carrying a line break splits its row: left out
            by_node[(r["country"], r["node"])].append(r)
        for (cc, node), rows in by_node.items():
            first = rows[0]
            said = [(v, film, right) for v, film, right in labels.get((cc, spaced(first["rawTitle"])), []) if v in ("*", first["venue"])]
            own = {(film, right) for v, film, right in said if v == first["venue"]}
            said = own or {(film, right) for v, film, right in said}
            right = {film for film, ok in said if ok}
            wrong = {film for film, ok in said if not ok}
            take = first["take"]
            if len(right) > 1:
                continue
            for r in rows:
                film = "tmdb:" + r["tmdbId"]
                if right:
                    label, judged = film in right, True
                elif film in wrong:
                    label, judged = False, True
                elif take:
                    label, judged = r["tmdbId"] == take, False
                else:
                    continue
                group = next(iter(right)) if right else ("tmdb:" + take if take else cc + node)
                yield dict(group=group, node=cc + node, label=label, logit=float(r["logit"]), judged=judged,
                           taken=r["tmdbId"] == take, levels={"release": r["release"], "certificate": r["certificate"]},
                           venue=first["venue"], rawTitle=first["rawTitle"], tmdb=r["tmdbId"], cc=cc)


def fold_of(group):
    return zlib.crc32(group.encode()) % FOLDS


def neutral(level):
    return level.startswith("missing") or level == "-"


def fit_levels(units, signal):
    counts = defaultdict(lambda: [0, 0])
    for u in units:
        lv = u["levels"][signal]
        counts[lv][0 if u["label"] else 1] += 1
    tp = sum(c[0] for c in counts.values())
    tn = sum(c[1] for c in counts.values())
    cells = len(counts)
    return {lv: 0.0 if neutral(lv) else
            math.log((p + ALPHA) / (tp + ALPHA * cells)) - math.log((n + ALPHA) / (tn + ALPHA * cells))
            for lv, (p, n) in counts.items()}, counts


def platt(scores, labels):
    """`(a, b)` minimising the log-loss of sigmoid(a * score + b): damped Newton, each step halved until the loss falls."""
    def loss(a, b):
        total = 0.0
        for x, y in zip(scores, labels):
            z = a * x + b
            total += math.log1p(math.exp(-z)) if y else math.log1p(math.exp(z)) if z < 30 else z
        return total
    a, b = 1.0, 0.0
    current = loss(a, b)
    for _ in range(100):
        ga = gb = haa = hab = hbb = 0.0
        for x, y in zip(scores, labels):
            p = 1 / (1 + math.exp(-max(-35, min(35, a * x + b))))
            g = p - (1 if y else 0)
            w = max(p * (1 - p), 1e-9)
            ga += g * x; gb += g; haa += w * x * x; hab += w * x; hbb += w
        det = haa * hbb - hab * hab
        if abs(det) < 1e-12:
            break
        da = (hbb * ga - hab * gb) / det
        db = (haa * gb - hab * ga) / det
        step = 1.0
        while step > 1e-6 and loss(a - step * da, b - step * db) > current:
            step /= 2
        if step <= 1e-6:
            break
        a -= step * da; b -= step * db
        now = loss(a, b)
        if current - now < 1e-9:
            current = now
            break
        current = now
    return a, b


def solve(matrix, vector):
    """`matrix` x = `vector` by Gaussian elimination with partial pivoting (a few dozen unknowns)."""
    n = len(vector)
    m = [row[:] + [vector[i]] for i, row in enumerate(matrix)]
    for c in range(n):
        pivot = max(range(c, n), key=lambda r: abs(m[r][c]))
        m[c], m[pivot] = m[pivot], m[c]
        if abs(m[c][c]) < 1e-12:
            continue
        for r in range(c + 1, n):
            f = m[r][c] / m[c][c]
            if f:
                for k in range(c, n + 1):
                    m[r][k] -= f * m[c][k]
    x = [0.0] * n
    for c in range(n - 1, -1, -1):
        x[c] = 0.0 if abs(m[c][c]) < 1e-12 else (m[c][n] - sum(m[c][k] * x[k] for k in range(c + 1, n))) / m[c][c]
    return x


def joint(rows, labels, l2=1.0):
    """A logistic over sparse rows (lists of (feature, value)), fitted jointly by damped Newton with an L2 penalty."""
    d = 1 + max((f for row in rows for f, _ in row), default=0)
    w = [0.0] * d
    def loss(w):
        total = sum(w[i] * w[i] for i in range(d)) * l2 / 2
        for row, y in zip(rows, labels):
            z = sum(w[f] * v for f, v in row)
            total += (math.log1p(math.exp(-z)) if z > -30 else -z) if y else (math.log1p(math.exp(z)) if z < 30 else z)
        return total
    current = loss(w)
    for _ in range(30):
        g = [l2 * x for x in w]
        h = [[l2 if i == j else 0.0 for j in range(d)] for i in range(d)]
        for row, y in zip(rows, labels):
            z = sum(w[f] * v for f, v in row)
            p = 1 / (1 + math.exp(-max(-35, min(35, z))))
            r = p - (1 if y else 0)
            q = max(p * (1 - p), 1e-12)
            for f, v in row:
                g[f] += r * v
                hf = h[f]
                for f2, v2 in row:
                    hf[f2] += q * v * v2
        step_dir = solve(h, g)
        step = 1.0
        while step > 1e-6 and loss([a - step * b for a, b in zip(w, step_dir)]) > current:
            step /= 2
        if step <= 1e-6:
            break
        w = [a - step * b for a, b in zip(w, step_dir)]
        now = loss(w)
        if current - now < 1e-7 * max(1.0, abs(current)):
            break
        current = now
    return w


def evaluate_joint(units, signals):
    """Each held-out fold scored by a logistic fitted on the other folds over the shipped log-odds, an intercept, and one
    indicator per known level of each signal (a missing side none): the signals' weights refitted jointly."""
    index = {}
    def row(u):
        out = [(0, 1.0), (1, u["logit"])]
        for s in signals:
            lv = u["levels"][s]
            if not neutral(lv):
                out.append((index.setdefault((s, lv), 2 + len(index)), 1.0))
        return out
    rows = [row(u) for u in units]
    held = []
    weights_seen = {}
    for k in range(FOLDS):
        train = [(r, u["label"]) for r, u in zip(rows, units) if fold_of(u["group"]) != k]
        w = joint([r for r, _ in train], [y for _, y in train])
        w += [0.0] * (2 + len(index) - len(w))
        weights_seen[k] = w
        for r, u in zip(rows, units):
            if fold_of(u["group"]) == k:
                z = sum(w[f] * v for f, v in r)
                held.append((u, 1 / (1 + math.exp(-max(-35, min(35, z)))), z))
    names = {v: f"{s}={lv}" for (s, lv), v in index.items()}
    return held, {names[i]: round(sum(weights_seen[k][i] for k in weights_seen) / FOLDS, 2) for i in names}


def metrics(held):
    eps = 1e-12
    logloss = -sum(math.log(max(eps, p if u["label"] else 1 - p)) for u, p, _ in held) / len(held)
    brier = sum((p - (1 if u["label"] else 0)) ** 2 for u, p, _ in held) / len(held)
    nodes = defaultdict(list)
    for u, p, x in held:
        nodes[u["node"]].append((x, u["label"]))
    with_right = [c for c in nodes.values() if any(y for _, y in c)]
    top1 = sum(1 for c in with_right if max(c)[1]) / max(1, len(with_right))
    return dict(auc=auc([(x, u["label"]) for u, _, x in held]), logloss=logloss, brier=brier, top1=top1)


def auc(pairs):
    pos = [s for s, y in pairs if y]
    neg = [s for s, y in pairs if not y]
    if not pos or not neg:
        return float("nan")
    ranked = sorted([(s, 1) for s in pos] + [(s, 0) for s in neg])
    rank_sum = 0.0
    i = 0
    while i < len(ranked):
        j = i
        while j < len(ranked) and ranked[j][0] == ranked[i][0]:
            j += 1
        avg = (i + j + 1) / 2
        rank_sum += avg * sum(1 for k in range(i, j) if ranked[k][1])
        i = j
    return (rank_sum - len(pos) * (len(pos) + 1) / 2) / (len(pos) * len(neg))


def evaluate(units, signals):
    held = []  # (unit, probability, score)
    for k in range(FOLDS):
        train = [u for u in units if fold_of(u["group"]) != k]
        test = [u for u in units if fold_of(u["group"]) == k]
        weights = {s: fit_levels(train, s)[0] for s in signals}
        score = lambda u: u["logit"] + sum(weights[s].get(u["levels"][s], 0.0) for s in signals)
        a, b = platt([score(u) for u in train], [u["label"] for u in train])
        for u in test:
            x = score(u)
            held.append((u, 1 / (1 + math.exp(-max(-35, min(35, a * x + b)))), x))
    eps = 1e-12
    logloss = -sum(math.log(max(eps, p if u["label"] else 1 - p)) for u, p, _ in held) / len(held)
    brier = sum((p - (1 if u["label"] else 0)) ** 2 for u, p, _ in held) / len(held)
    nodes = defaultdict(list)
    for u, p, x in held:
        nodes[u["node"]].append((x, u["label"]))
    with_right = [c for c in nodes.values() if any(y for _, y in c)]
    top1 = sum(1 for c in with_right if max(c)[1]) / max(1, len(with_right))
    return dict(auc=auc([(x, u["label"]) for u, _, x in held]), logloss=logloss, brier=brier, top1=top1, nodes=len(with_right)), held


SIGNAL_OF = {"release": "release.country", "certificate": "certificate"}
NEUTRAL = {"listing": "the venue's country is unknown, or it bills no certificate",
           "unrated": "the venue's country rates no film (Poland): no scale to read a certificate on"}


def jackson(value, indent):
    """`value` as Play's Json.prettyPrint lays it out (`"key" : value`, two spaces a level), so the artefact's diff is the table."""
    pad = "  " * indent
    if isinstance(value, dict):
        if not value:
            return "{ }"
        return "{\n" + ",\n".join(f'{pad}  "{k}" : {jackson(v, indent + 1)}' for k, v in value.items()) + f"\n{pad}}}"
    if isinstance(value, list):
        if not value:
            return "[ ]"
        return "[ " + ", ".join(jackson(v, indent + 1) for v in value) + " ]"
    if isinstance(value, str):
        import json
        return json.dumps(value, ensure_ascii=False)
    return repr(value)


def write_weights(variant, signals, paths):
    units = list(corpus_units(paths))
    artefact = "identity/src/main/resources/identity-weights.json"
    with open(artefact, encoding="utf-8") as f:
        text = f.read()
    blocks = []
    for s in signals:
        w, c = fit_levels(units, s)
        table = {
            "kind": "categorical",
            "categories": {lv: w[lv] for lv in sorted(c) if not neutral(lv)},
            "bins": [],
            "missing": {lv[len("missing:"):]: 0.0 for lv in sorted(c) if lv.startswith("missing:")},
            "counts": {lv: list(c[lv]) for lv in sorted(c)},
            "neutral": {lv[len("missing:"):]: NEUTRAL.get(lv[len("missing:"):], "missing evidence, weighed by neither side")
                        for lv in sorted(c) if lv.startswith("missing:")},
        }
        blocks.append(f'        "{SIGNAL_OF[s]}" : {jackson(table, 4)},\n')
        print(s, {lv: round(x, 3) for lv, x in w.items()}, dict(c))
    anchor = '"listing-film" : {\n      "prior" : '
    at = text.index("\n", text.index('"signals" : {', text.index(anchor))) + 1
    text = text[:at] + "".join(blocks) + text[at:]
    version = text.index('"version" : "') + len('"version" : "')
    text = text[:text.index('"', version)] + "+weighted-guards-" + variant + text[text.index('"', version):]
    with open(artefact, "w", encoding="utf-8") as f:
        f.write(text)


def main():
    mode, paths = sys.argv[1], sys.argv[2:]
    if mode == "weights":
        return write_weights(paths[0], paths[1].split(","), paths[2:])
    units = list(capture_units(paths[0]) if mode == "capture" else corpus_units(paths))
    signals = list(units[0]["levels"].keys())
    print(f"units {len(units)}, right {sum(u['label'] for u in units)}, judged {sum(u['judged'] for u in units)}, groups {len({u['group'] for u in units})}")
    for s in signals:
        w, c = fit_levels(units, s)
        known = sum(n for lv, (p, n) in c.items() if not neutral(lv)) + sum(p for lv, (p, n) in c.items() if not neutral(lv))
        print(f"  {s}: known on {known}/{len(units)} ({100 * known / len(units):.1f}%); " +
              ", ".join(f"{lv} {p}/{n} w={w[lv]:+.2f}" for lv, (p, n) in sorted(c.items())))
    variants = [("base", [])] + [(s, [s]) for s in signals] + [("all", signals)]
    base_held = None
    print(f"{'variant':12} {'AUC':>7} {'logloss':>8} {'Brier':>8} {'top1':>7}  AUC(judged)  AUC(known)")
    for name, sigs in variants:
        m, held = evaluate(units, sigs)
        judged = auc([(x, u["label"]) for u, _, x in held if u["judged"]])
        known = auc([(x, u["label"]) for u, _, x in held if any(not neutral(u["levels"][s]) for s in sigs)]) if sigs else float("nan")
        known_base = auc([(u["logit"], u["label"]) for u, _, x in held if any(not neutral(u["levels"][s]) for s in sigs)]) if sigs else float("nan")
        print(f"{name:12} {m['auc']:.4f} {m['logloss']:.5f} {m['brier']:.5f} {m['top1']:.4f}  {judged:.4f}       {known:.4f} (base {known_base:.4f})")
        jheld, jweights = evaluate_joint(units, sigs)
        jm = metrics(jheld)
        jjudged = auc([(x, u["label"]) for u, _, x in jheld if u["judged"]])
        print(f"{'  joint':12} {jm['auc']:.4f} {jm['logloss']:.5f} {jm['brier']:.5f} {jm['top1']:.4f}  {jjudged:.4f}       weights {jweights}")
        if name == "base":
            base_held = {id(u): x for u, _, x in held}
        if sigs and mode == "corpus":
            # the judged-wrong takes: which a variant pushes down, and by how much
            wrong = [(u, x) for u, _, x in held if u["judged"] and not u["label"] and u["taken"]]
            down = [(u, x - base_held[id(u)]) for u, x in wrong if x - base_held[id(u)] < -0.3]
            print(f"   judged-wrong takes {len(wrong)}; pushed down >0.3 log-odds: {len(down)}" +
                  "".join(f"\n     {u['cc']} {u['venue']} «{u['rawTitle']}» tmdb {u['tmdb']} {d:+.2f} {u['levels']}" for u, d in down[:8]))
            right = [(u, x) for u, _, x in held if u["judged"] and u["label"] and u["taken"]]
            hurt = [(u, x - base_held[id(u)]) for u, x in right if x - base_held[id(u)] < -0.3]
            print(f"   judged-right takes {len(right)}; pushed down >0.3: {len(hurt)}" +
                  "".join(f"\n     {u['cc']} {u['venue']} «{u['rawTitle']}» tmdb {u['tmdb']} {d:+.2f} {u['levels']}" for u, d in hurt[:5]))


if __name__ == "__main__":
    main()
