"""The takes of identity slices at two refs, compared (scripts/identity-slice): every listing whose final take moved, with
its labels.tsv verdict at each ref, and the moves tallied — CURED (a WRONG take gone), LOST (a RIGHT take gone),
NEW-WRONG (a take now WRONG), and the unjudged moves.

    python3 identity_slice_diff.py BASE.tsv HEAD.tsv BASE_NAME HEAD_NAME LABELS.tsv

Each TSV is scripts.IdentitySlice's `--out` (header: selector key venue rawTitle film title basis confidence model rules
blocker runnerUp verdict explanation)."""
import csv
import sys


def read(path):
    with open(path, encoding="utf-8") as f:
        rows = list(csv.DictReader(f, delimiter="\t", quoting=csv.QUOTE_NONE))
    return {r["key"]: r for r in rows}


def classify(a, b):
    """What a listing's move from take `a` to take `b` (rows; None when the slice did not decide it) did."""
    va = a["verdict"] if a else "-"
    vb = b["verdict"] if b else "-"
    if va == "WRONG" and vb != "WRONG":
        return "cured"
    if va == "RIGHT" and vb != "RIGHT":
        return "lost"
    if vb == "WRONG" and va != "WRONG":
        return "new-wrong"
    if vb == "RIGHT" and va != "RIGHT":
        return "won"
    return "moved (unjudged)"


def take(r):
    if r is None:
        return "not decided"
    title = f" '{r['title']}'" if r["title"] else ""
    return f"{r['film']}{title} via {r['basis']} [{r['verdict']}]"


def main(argv):
    base, head, base_name, head_name = read(argv[0]), read(argv[1]), argv[2], argv[3]
    keys = sorted(set(base) | set(head))
    moves = [(k, base.get(k), head.get(k)) for k in keys
             if (base.get(k) or {}).get("film") != (head.get(k) or {}).get("film")]
    tally = {}
    print(f"\n{len(keys)} listing(s) decided; {len(moves)} moved between {base_name} and {head_name}")
    for kind in ("cured", "lost", "new-wrong", "won", "moved (unjudged)"):
        of_kind = [m for m in moves if classify(m[1], m[2]) == kind]
        tally[kind] = len(of_kind)
        if not of_kind:
            continue
        print(f"\n{kind.upper()} ({len(of_kind)}):")
        for k, a, b in of_kind:
            r = a or b
            print(f"  {r['venue']} | {r['rawTitle']}: {take(a)} -> {take(b)}")
            why = (b or a)["explanation"]
            if why:
                print(f"      {why[:220]}")
    print("\nTALLY " + ", ".join(f"{k} {v}" for k, v in tally.items()))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
