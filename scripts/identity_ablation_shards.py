"""SHARDS OF A RULE ABLATION (scripts/identity_rule_ablation.py): which configurations a CI row bisects, where the rows'
state.json files are, and one state merged of them."""
import os
import sys
from typing import Any, Dict, List, TypedDict

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from seams.tool_files import Files  # noqa: E402


def shard_of(configs: List[Any], shard: str) -> List[Any]:
    """The configurations of shard "I/N": every N-th, from the I-th (0-based); all for none."""
    if not shard:
        return configs
    i, n = (int(x) for x in shard.split("/"))
    return [c for k, c in enumerate(configs) if k % n == i]


def shard_states(files: Files, root: str) -> List[str]:
    """Every shard's state.json under root, at any depth: download-artifact@v6 extracts a pattern matching ONE artifact
    straight into its path, two or more each into a directory of its name (run 42377038878's collect)."""
    if not files.is_dir(root):
        return []
    return [path for path in files.under(root) if os.path.basename(path) == "state.json"]


class MergedState(TypedDict):
    round: int
    settled: Dict[str, object]
    interactions: List[object]
    pending: List[object]
    fired: Dict[str, object]


def merge(states: List[Dict[str, Any]]) -> MergedState:
    """One state of several shards' (each settling its own rules): their settled rules, interactions and fired counts."""
    out: MergedState = {"round": max((s.get("round", 0) for s in states), default=0), "settled": {}, "interactions": [], "pending": [], "fired": {}}
    for s in states:
        out["settled"].update(s.get("settled", {}))
        out["interactions"] += s.get("interactions", [])
        out["fired"].update(s.get("fired", {}))
        out["pending"] += s.get("pending") or []
    return out
