#!/usr/bin/env python3
"""Fit each feed's copied-fact share from the judged candidates (scored-<cc>.jsonl, integration.CopiedFactReliability).

Each judged (listing node, film) pair: L its log-odds as the resolve scores it, S the part of L its feed-copied facts
contribute (year.delta, director, runtime.delta, originalTitle, when the node's facts are one feed's). Logistic model
  P(right) = sigmoid(a + b*(L - S) + sum_f [feed=f] * (sp_f * max(S,0) + sd_f * min(S,0)))
fitted by maximum likelihood (Newton). The share of feed f is sp_f/b (support) and sd_f/b (dissent): how much of a
venue-own fact's weight its copied fact earns. Bootstrap (by listing title) gives a 90% interval.
usage: copied-fact-share-fit.py <scored-*.jsonl>... [--human] [--boot N]
"""
import json, sys, math, random, collections

Valued = {"--boot", "--write", "--min-pairs"}
args = [a for i, a in enumerate(sys.argv[1:], 1) if not a.startswith("--") and sys.argv[i - 1] not in Valued]
human = "--human" in sys.argv
boot = int(sys.argv[sys.argv.index("--boot") + 1]) if "--boot" in sys.argv else 200
SIG = ("year.delta", "director", "runtime.delta", "originalTitle")
rows = []
seen = set()
for path in args:
    for line in open(path):
        r = json.loads(line)
        srcs = r["sources"]
        feed = srcs[0] if len(srcs) == 1 and srcs[0] != "venue" else None
        for v in r["verdicts"]:
            verdict, ev = (v.split("\t") + [""])[:2]
            if verdict not in ("right", "wrong"):
                continue
            if human and not ev.startswith(("review page", "hand label")):
                continue
            key = (path, tuple(r["keys"]), r["id"])
            if key in seen:
                continue
            seen.add(key)
            S = sum(r["weights"].get(s, 0.0) for s in SIG) if feed else 0.0
            rows.append((feed, r["logOdds"], S, 1 if verdict == "right" else 0, r["keys"][0].split("|", 1)[1]))

feeds = sorted({f for f, *_ in rows if f})
names = ["a", "b"] + [f"{f}{d}" for f in feeds for d in ("+", "-")]

def x_of(row):
    f, L, S, y, _ = row
    x = [1.0, L - S] + [0.0] * (2 * len(feeds))
    if f:
        i = 2 + 2 * feeds.index(f)
        x[i] = max(S, 0.0); x[i + 1] = min(S, 0.0)
    return x

def fit(sample, ridge=1e-3):
    X = [x_of(r) for r in sample]; Y = [r[3] for r in sample]
    n = len(names); w = [0.0] * n
    for _ in range(50):
        g = [0.0] * n; H = [[0.0] * n for _ in range(n)]
        for x, y in zip(X, Y):
            z = sum(wi * xi for wi, xi in zip(w, x)); p = 1 / (1 + math.exp(-max(-30, min(30, z))))
            for i in range(n):
                g[i] += (y - p) * x[i]
                for j in range(n):
                    H[i][j] -= p * (1 - p) * x[i] * x[j]
        for i in range(n):
            g[i] -= ridge * w[i]; H[i][i] -= ridge
        # solve H d = -g
        A = [H[i][:] + [-g[i]] for i in range(n)]
        for c in range(n):
            piv = max(range(c, n), key=lambda r: abs(A[r][c])); A[c], A[piv] = A[piv], A[c]
            if abs(A[c][c]) < 1e-12:
                continue
            for r in range(n):
                if r != c:
                    k = A[r][c] / A[c][c]
                    A[r] = [a - k * b for a, b in zip(A[r], A[c])]
        d = [A[i][n] / A[i][i] if abs(A[i][i]) > 1e-12 else 0.0 for i in range(n)]
        w = [wi + di for wi, di in zip(w, d)]
        if max(abs(di) for di in d) < 1e-6:
            break
    return w

w = fit(rows)
counts = collections.Counter((f, y, "+" if S > 0 else "-" if S < 0 else "0") for f, L, S, y, _ in rows)
print(f"pairs {len(rows)}  (right {sum(r[3] for r in rows)}, wrong {sum(1 - r[3] for r in rows)})")
print("fit:", "  ".join(f"{n}={v:.3f}" for n, v in zip(names, w)))
titles = sorted({r[4] for r in rows}); random.seed(7)
shares = collections.defaultdict(list)
by_title = collections.defaultdict(list)
for r in rows: by_title[r[4]].append(r)
for _ in range(boot):
    sample = [r for t in random.choices(titles, k=len(titles)) for r in by_title[t]]
    bw = fit(sample)
    for i, n in enumerate(names[2:], 2):
        shares[n].append(bw[i] / bw[1] if bw[1] > 1e-6 else float("nan"))
print("feed\tsign\tn right/wrong\tshare\t90% interval")
for f in feeds:
    for k, d in enumerate(("+", "-")):
        i = 2 + 2 * feeds.index(f) + k
        s = sorted(x for x in shares[f + d] if x == x)
        lo, hi = (s[int(0.05 * len(s))], s[int(0.95 * len(s)) - 1]) if s else (float("nan"),) * 2
        print(f"{f}\t{d}\t{counts[(f, 1, d)]}/{counts[(f, 0, d)]}\t{w[i] / w[1]:.3f}\t[{lo:.3f}, {hi:.3f}]")

# --write <identity-weights.json>: the fitted shares as the listing-film scope's `copied` (IdentityCalibration.CopiedShare,
# which only the tag experiment/copied-share-fit-a reads: NOT LANDED, docs/identity-experiments.md 2026-10-21),
# each clipped to [0, 1] (a copy never weighs more than the venue's own fact), a feed with fewer than --min-pairs judged
# pairs on a side left at 1 for that side.
if "--write" in sys.argv:
    import re
    target = sys.argv[sys.argv.index("--write") + 1]
    min_pairs = int(sys.argv[sys.argv.index("--min-pairs") + 1]) if "--min-pairs" in sys.argv else 20
    entries = []
    for f in feeds:
        def share(k, d):
            n = counts[(f, 1, d)] + counts[(f, 0, d)]
            return (min(1.0, max(0.0, w[2 + 2 * feeds.index(f) + k] / w[1])) if n >= min_pairs else 1.0), n
        (sp, np_), (sd, nd) = share(0, "+"), share(1, "-")
        if sp < 1.0 or sd < 1.0:
            entries.append(f'        "{f}" : {{\n          "support" : {sp:.3f},\n          "dissent" : {sd:.3f},\n          "n" : {np_ + nd}\n        }}')
    text = open(target).read()
    text = re.sub(r'\n      "copied" : \{.*?\n      \},', "", text, flags=re.S)
    block = '\n      "copied" : {\n' + ",\n".join(entries) + '\n      },'
    anchor = '"listing-film" : {'
    i = text.index(anchor) + len(anchor)
    open(target, "w").write(text[:i] + (block if entries else "") + text[i:])
    print(f"wrote {len(entries)} feed share(s) into {target}")
