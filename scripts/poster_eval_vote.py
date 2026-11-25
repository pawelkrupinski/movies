"""THE EVALUATION of poster_eval: a variant's posters resolved to hashes, each (cluster, url, candidate) pair's nearest bits,
and PosterEvidence's vote and veto over them -- numpy over the dataset's pair arrays, never a loop over pairs."""
from __future__ import annotations

from typing import Dict, List, Optional, Sequence, Tuple

from poster_eval_common import Array, Arrays, FiledPosters, Meta, Missing, Variant, np
from poster_eval_images import Hashed, Images, TmdbLists, chosen

TmdbCdn = "https://image.tmdb.org/t/p/w185/"
FilmPoster = Tuple[Optional[str], Hashed]  # (name or None, filed hash or None)
Result = Tuple[Array, Array, Array]  # per cluster: the film voted for (-1 none), whether prod's take is vetoed, the vote's bits


def _filed(arrays: Arrays, j: int) -> List[int]:
    offsets, hashes = arrays["film_offsets"], arrays["film_hashes"]
    return [int(h) for h in hashes[offsets[j]:offsets[j + 1]]]


def _wants_list(arrays: Arrays, j: int, n: int) -> bool:
    """Does key `j` need TMDB's poster list: no filed answer, or more posters asked than prod files."""
    return not _filed(arrays, j) or n > FiledPosters


def _film_mine(meta: Meta, arrays: Arrays, tmdb: TmdbLists, j: int, n: int) -> List[FilmPoster]:
    cc, film = meta["keys"][j].split("|")
    filed = _filed(arrays, j)
    filed_paths: Sequence[Optional[str]] = meta["film_paths"].get(meta["keys"][j]) or [None] * len(filed)
    pairs: List[FilmPoster] = [(p, h) for p, h in zip(filed_paths, filed)]
    mine = pairs[:n]
    if _wants_list(arrays, j, n):
        have = {p for p, _ in mine}
        mine += [(Images.film_name(p), None) for p in chosen(tmdb.posters(film), cc, n) if p not in have]
    return mine


def film_posters(meta: Meta, arrays: Arrays, tmdb: TmdbLists, n: int) -> Dict[int, List[FilmPoster]]:
    """{key index: [(name or None, filed hash or None)]}: each candidate's N posters for the variant."""
    keys = meta["keys"]
    tmdb.ensure({int(k.split("|")[1]) for j, k in enumerate(keys) if _wants_list(arrays, j, n)})
    return {j: _film_mine(meta, arrays, tmdb, j, n) for j in range(len(keys))}


def _wanted(meta: Meta, has: Array, per_film: Dict[int, List[FilmPoster]]) -> Dict[str, str]:
    """Every image a variant needs and no hash is filed for: {cache name: source url}."""
    wanted = {Images.venue_name(u): u for i, u in enumerate(meta["urls"]) if not has[i]}
    for posters in per_film.values():
        for name, h in posters:
            if h is None and name is not None:
                wanted[name] = TmdbCdn + name[2:]
    return wanted


def _fill_venues(meta: Meta, images: Images, venue: Array, has: Array) -> None:
    for i, u in enumerate(meta["urls"]):
        if not has[i]:
            h = images.hash_of(Images.venue_name(u))
            if h not in (None, -1):
                venue[i], has[i] = h, True


def _hash(images: Images, name: Optional[str], filed: Hashed) -> Hashed:
    """A film poster's hash: the filed one, else the cached one of its image."""
    return images.hash_of(name) if filed is None and name is not None else filed


def _film_hashes(meta: Meta, images: Images, per_film: Dict[int, List[FilmPoster]]) -> Tuple[List[int], List[int]]:
    flat: List[int] = []
    offsets = [0]
    for j in range(len(meta["keys"])):
        flat += [h for h in (_hash(images, name, h) for name, h in per_film[j]) if h is not None and h != -1]
        offsets.append(len(flat))
    return flat, offsets


def resolve(meta: Meta, arrays: Arrays, images: Images, tmdb: TmdbLists, n: int) -> Tuple[Array, Array, Array, Array]:
    """(venue hash per url, has, film hashes flat, offsets) for a variant with `n` posters per film."""
    venue, has = arrays["venue_filed"].copy(), arrays["venue_has"].copy()
    per_film = film_posters(meta, arrays, tmdb, n)
    images.ensure(_wanted(meta, has, per_film))
    _fill_venues(meta, images, venue, has)
    flat, offsets = _film_hashes(meta, images, per_film)
    return venue, has, np.array(flat, np.uint64), np.array(offsets, np.int64)


def nearest(arrays: Arrays, venue: Array, has: Array, film_hashes: Array, film_offsets: Array) -> Array:
    """Each (cluster, url, candidate) pair's nearest bits between the venue poster and any of the candidate's posters
    (Missing when either has none): numpy XOR + popcount over every (pair, poster), a min per pair."""
    keys = arrays["pair_key"]
    counts = (film_offsets[1:] - film_offsets[:-1])[keys]
    counts = np.where(has[arrays["pair_url"]], counts, 0)
    total = int(counts.sum())
    out = np.full(len(keys), Missing, np.int32)
    if total == 0:
        return out
    pair = np.repeat(np.arange(len(keys)), counts)
    starts = np.cumsum(counts) - counts
    poster = film_offsets[keys][pair] + (np.arange(total) - starts[pair])
    bits = np.bitwise_count(venue[arrays["pair_url"]][pair] ^ film_hashes[poster]).astype(np.int32)
    nonempty = counts > 0
    out[nonempty] = np.minimum.reduceat(bits, starts[nonempty])
    return out


def vetoed(arrays: Arrays, d: Array, target: Array, n_clusters: int, veto_match: int, veto: int) -> Array:
    """Per cluster: does any venue poster name another candidate within `veto_match` while the cluster's `target` film
    (-1: none) stays beyond `veto` of it (or shows no poster)? PosterEvidence.veto."""
    cu, cl = arrays["pair_cu"], arrays["pair_cluster"]
    n_cu = len(arrays["cu_cluster"])
    is_target = arrays["pair_film"] == target[cl]
    target_bits = np.full(n_cu, Missing, np.int32)
    np.minimum.at(target_bits, cu[is_target], d[is_target])
    other = np.zeros(n_cu, bool)
    other[cu[~is_target & (d <= veto_match)]] = True
    by_cu = (target_bits > veto) & other
    out = np.zeros(n_clusters, bool)
    out[arrays["cu_cluster"][by_cu]] = True
    return out


def evaluate(meta: Meta, arrays: Arrays, d: Array, variant: Variant) -> Result:
    """Per cluster: the film the posters vote for (-1 none), whether prod's take is vetoed, the vote's bits."""
    n = len(meta["clusters"])
    cl, film = arrays["pair_cluster"], arrays["pair_film"]
    hit = d <= variant["vote"]
    pairs = np.unique(np.stack([cl[hit], film[hit]]), axis=1) if hit.any() else np.zeros((2, 0), np.int64)
    per = np.bincount(pairs[0], minlength=n)
    vote = np.full(n, -1, np.int64)
    sole = per[pairs[0]] == 1
    vote[pairs[0][sole]] = pairs[1][sole]
    vote[vetoed(arrays, d, vote, n, variant["veto_match"], variant["veto"]) & (vote >= 0)] = -1
    taken = np.array([c["film"] if c["film"] is not None else -1 for c in meta["clusters"]], np.int64)
    take_vetoed = vetoed(arrays, d, taken, n, variant["veto_match"], variant["veto"]) & (taken >= 0)
    bits = np.full(n, Missing, np.int32)
    voted_rows = vote[cl] == film
    np.minimum.at(bits, cl[voted_rows], d[voted_rows])
    return vote, take_vetoed, bits


def run_variant(meta: Meta, arrays: Arrays, images: Images, tmdb: TmdbLists, variant: Variant) -> Result:
    venue, has, film_hashes, film_offsets = resolve(meta, arrays, images, tmdb, variant["posters"])
    d = nearest(arrays, venue, has, film_hashes, film_offsets)
    return evaluate(meta, arrays, d, variant)
