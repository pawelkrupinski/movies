"""WHAT THE LOCAL PROD MIRROR HOLDS of a listing (scripts/listing_history.py): the mongosh query over its identity
collections, and the lines that say what it returned -- traces, family decisions, agreements, pins, listings -- and
prod's reason for one listing now."""
import json
from typing import Any, Dict, List, Optional, Set, Tuple

State = Dict[str, Any]  # the query's answer: traces, families, venues, agreements, pins

MIRROR_QUERY = """
const venue = new RegExp(VENUE, 'i'), title = new RegExp(TITLE, 'i');
const traces = db.identity_traces.find({'listing.venue': venue, 'listing.rawTitle': title}).limit(40).toArray();
const families = db.identity_model_families.find({_id: {$in: traces.map(t => t.family)}},
  {decisions: 1}).toArray();
const venues = db.identity_listings.find({_id: venue}).limit(40).toArray().map(v => ({_id: v._id,
  films: (v.films || []).filter(f => title.test((f.movie || {}).rawTitle || (f.movie || {}).title || ''))}))
  .filter(v => v.films.length > 0);
// the agreement stage's stored verdict (filed under the cluster's key, whose first listing is its family's id; under the
// family's id itself before keys, else one naming the title) and pins
const ids = traces.map(t => t.family);
const keyed = db.identity_agreement_keys.find({first: {$in: ids}}).toArray().map(k => k._id.toString());
const agreements = db.identity_agreements.find({$or: [{_id: {$in: ids.concat(keyed)}}, {_id: title}]}).limit(10).toArray()
  .map(a => ({_id: a._id, agreed: a.agreed, filled: a.filled, correction: a.correction, rules: a.rules}));
const pins = db.identity_pins.find({'listings.rawTitle': title}).limit(10).toArray();
print(EJSON.stringify({traces: traces, families: families, venues: venues, agreements: agreements, pins: pins},
  {relaxed: true}));
"""


def _family_id(raw: str) -> str:
    return raw.replace("\x00", " · ")


def _trace_lines(trace: State) -> List[str]:
    listing = trace.get("listing", {})
    lines = [f"TRACE {listing.get('venue')} | {listing.get('rawTitle')}: film {trace.get('film') or 'none'} via "
             f"{trace.get('basis')}" + (f", vetoed by {trace['vetoedBy']}" if trace.get("vetoedBy") else "")
             + (f"; rules {', '.join(trace['rules'])}" if trace.get("rules") else "")]
    if trace.get("evidence"):
        lines.append(f"    evidence: {'; '.join(trace['evidence'][:5])}")
    for refusal in trace.get("refusals", [])[:3]:
        lines.append(f"    refused: {refusal if isinstance(refusal, str) else json.dumps(refusal, ensure_ascii=False)}")
    if trace.get("candidates"):
        lines.append(f"    candidates: {'; '.join(trace['candidates'][:3])}")
    return lines + [f"    family: {_family_id(trace.get('family', ''))}"]


def _members(decision: State) -> Set[Tuple[Any, Any]]:
    """A decision's members: its listings, or its listings by node (a list of lists)."""
    flat = [m for group in decision.get("members", []) for m in (group if isinstance(group, list) else [group])]
    return {(m.get("venue"), m.get("rawTitle")) for m in flat}


def _decision_lines(family_id: str, decision: State, members: Set[Tuple[Any, Any]]) -> List[str]:
    confidence = decision.get("confidence")
    agreed = f", agreed {json.dumps(decision['agreed'], ensure_ascii=False)}" if decision.get("agreed") else ""
    line = (f"FAMILY {_family_id(family_id)}: film {decision.get('film') or 'none'} via {decision.get('basis')}"
            + (f" at {confidence:.0%}" if isinstance(confidence, (int, float)) else "") + f", {len(members)} member(s)" + agreed)
    return [line] + [f"    {text}" for text in decision.get("explanation", [])[:3]]


def _family_lines(state: State) -> List[str]:
    keys = {(t["listing"].get("venue"), t["listing"].get("rawTitle")) for t in state.get("traces", []) if t.get("listing")}
    lines: List[str] = []
    for family in state.get("families", []):
        for decision in family.get("decisions", []):
            members = _members(decision)
            if not keys or members & keys:
                lines += _decision_lines(family["_id"], decision, members)
    return lines


def _agreement_line(agreement: State) -> str:
    agreed, filled, correction = agreement.get("agreed"), agreement.get("filled"), agreement.get("correction")
    what = [f"agreed «{agreed.get('title')}» by {', '.join(agreed.get('families', []))}" if agreed else "agreed on nothing",
            f"filled by {filled.get('rule')} (tmdb {filled.get('tmdb')})" if filled else None,
            f"corrected to {correction.get('film')}" if correction and correction.get("film") else None]
    return f"AGREEMENT {_family_id(agreement['_id'])}: " + "; ".join(w for w in what if w)


def _listing_line(venue_id: str, film: State) -> str:
    movie = film.get("movie", {})
    facts = [f"year {movie['releaseYear']}" if movie.get("releaseYear") else None,
             f"runtime {movie['runtimeMinutes']}" if movie.get("runtimeMinutes") else None,
             f"original title «{movie['originalTitle']}»" if movie.get("originalTitle") else None,
             f"director {', '.join(film['director'])}" if film.get("director") else None,
             f"{len(film.get('showtimes', []))} showtimes",
             f"ids {json.dumps(film['externalIds'])}" if film.get("externalIds") else None]
    return f"LISTING {venue_id} | {movie.get('rawTitle') or movie.get('title')}: " + ", ".join(f for f in facts if f)


def mirror_lines(state: State) -> List[str]:
    lines = [line for trace in state.get("traces", []) for line in _trace_lines(trace)]
    lines += _family_lines(state)
    lines += [_agreement_line(a) for a in state.get("agreements", [])]
    lines += [f"PIN {pin.get('kind')} {pin.get('tmdbId') or ''} by {pin.get('author')}: {pin.get('reason')}" for pin in state.get("pins", [])]
    lines += [_listing_line(venue["_id"], film) for venue in state.get("venues", []) for film in venue.get("films", [])]
    return lines or ["the mirror holds no trace, family or listing matching it"]


def _reason_of(trace: State) -> str:
    optional = [("rules", " ".join(trace.get("rules") or [])), ("blocker", trace.get("blocker")), ("vetoed by", trace.get("vetoedBy"))]
    return f"film {trace.get('film') or 'none'} via {trace.get('basis')}" + "".join(f"; {name} {value}" for name, value in optional if value)


def trace_reason(state: Optional[State], key: str) -> str:
    """Prod's reason for `key` (`venue | raw title`) now, from the mirror's identity_traces: film, basis, rules, blocker."""
    if state is None:
        return "the mirror is not readable"
    venue, listing = key.split(" | ", 1)
    for trace in state.get("traces", []):
        if trace.get("listing", {}).get("venue") == venue and trace.get("listing", {}).get("rawTitle") == listing:
            return _reason_of(trace)
    return "the mirror holds no trace of it now (not listed, or billed otherwise)"
