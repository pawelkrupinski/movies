"""The mirror scripts of flip_timeline.py (scripts/flip-timeline): one mongosh script per read, under a header of constants."""

# Every mirror read is one mongosh script under a header of constants; the marker comment names it (tests dispatch on it).
TAKES_QUERY = """// takes
const vre = new RegExp(VENUE, 'i'), tre = new RegExp(TITLE, 'i'), since = new Date(SINCE);
const q = {kind: 'take', at: {$gte: since}, 'after.venue': vre, 'after.listing': tre};
if (AFTER_ID) q.$and = [{$or: [{at: {$gt: new Date(AFTER_AT)}}, {at: new Date(AFTER_AT), _id: {$gt: ObjectId(AFTER_ID)}}]}];
const page = db.identity_history.find(q).sort({at: 1, _id: 1}).limit(PAGE).toArray();
print(JSON.stringify(page.map(t => ({at: t.at.getTime(), id: t.id, oid: t._id.toString(), build: t.build || '',
  readIds: (t.after.reads || []).filter(r => !r.includes('identity_family_answers')).map(r => r.slice(r.indexOf(' ') + 1)),
  after: Object.assign({}, t.after, {reads: undefined})}))));
"""

SIBLINGS_QUERY = """// siblings
const since = new Date(SINCE);
const found = db.identity_history.find({kind: 'take', at: {$gte: since}, id: {$in: IDS}}, {id: 1, at: 1, 'after.members': 1})
  .sort({at: 1}).toArray();
print(JSON.stringify(found.map(t => ({at: t.at.getTime(), id: t.id, members: (t.after || {}).members || []}))));
"""

# The posters a cluster's decision compares -- each candidate film's (`poster|film|<tmdb>`: the members' traces' candidates
# and the films the reversal moved between) and each member's own venue poster (`poster|venue|<url>`, from the venues'
# listings now) -- as the question ids their asks are filed under. Poster answers (`poster|...`) are the one ask a
# candidate set leads to that names no title: a reversal restored when one was filed late (US «Game», 2026-11-12: «Blind
# Game»'s poster, a new search candidate) read as UNCLASSIFIED without them.
POSTER_SUBJECTS_JS = """
const posterSubject = id => { const i = id.indexOf('poster|'); return i < 0 ? '' : id.slice(i); };
const posterSubjects = (members, films) => {
  const subjects = new Set(films.map(f => 'poster|film|' + f));
  members.slice(0, 20).forEach(m => {
    const parts = m.split('\\u0000'), venue = parts[1], raw = parts[parts.length - 1] || parts[parts.length - 2];
    const trace = db.identity_traces.findOne({_id: m}, {candidates: 1});
    ((trace || {}).candidates || []).forEach(c => { const id = String(c).split(' ')[0]; if (/^[0-9]+$/.test(id)) subjects.add('poster|film|' + id); });
    const venueListings = db.identity_listings.findOne({_id: venue}, {films: 1});
    ((venueListings || {}).films || []).forEach(f => {
      if (f.posterUrl && (((f.movie || {}).title || '') === raw || (parts.length > 3 && f.filmUrl === parts[2]))) subjects.add('poster|venue|' + f.posterUrl);
    });
  });
  return subjects;
};
"""

ASKS_QUERY = """// asks
""" + POSTER_SUBJECTS_JS + """
const since = new Date(SINCE), H = db.identity_history;
const near = WINDOWS.map(w => ({at: {$gte: new Date(w[0]), $lte: new Date(w[1])}}));
const askTitle = TITLE ? new RegExp('\\|title\\|.*' + TITLE, 'i') : null;
const clauses = (askTitle ? [{id: askTitle}] : []).concat(near.length ? [{id: /\\|record\\|/, $or: near}] : []);
const asks = clauses.length ? H.find({kind: 'answer:identity_question_markers', at: {$gte: since}, $or: clauses}).sort({at: 1}).limit(2000).toArray() : [];
// the posters its decisions compare (its candidates', its members' own), filed around its takes
const subjects = near.length ? posterSubjects(MEMBERS, FILMS) : new Set();
if (near.length) H.find({kind: 'answer:identity_question_markers', id: /\\|poster\\|/, $or: near}).sort({at: 1}).limit(500)
  .forEach(a => { if (subjects.has(posterSubject(a.id)) && !asks.some(b => b.at.getTime() === a.at.getTime() && b.id === a.id)) asks.push(a); });
asks.sort((a, b) => a.at - b.at);
const readIds = READ_IDS;
const answers = readIds.length && near.length ? H.find({kind: {$in: ['answer:tmdb_queries', 'answer:tmdb_films']},
  id: {$in: readIds}, $or: near}, {kind: 1, id: 1, at: 1}).sort({at: 1}).limit(500).toArray() : [];
print(JSON.stringify({
  asks: asks.map(a => ({at: a.at.getTime(), id: a.id, outcome: (a.after || {}).outcome || '', failures: (a.after || {}).failures || 0})),
  answers: answers.map(a => ({at: a.at.getTime(), kind: a.kind, id: a.id}))}));
"""

LISTINGS_QUERY = """// listings
const since = new Date(SINCE);
const found = db.identity_history.find({kind: 'listing', at: {$gte: since}, id: {$in: IDS}}, {id: 1, at: 1}).sort({at: 1}).limit(2000).toArray();
print(JSON.stringify(found.map(l => ({at: l.at.getTime(), id: l.id}))));
"""

# --all: the question-marker asks filed in each reversal's window whose subject is its (cleaned) title; and the member
# listings re-published inside a window, for the reversals nothing else explained
ALL_ASKS_QUERY = """// all-asks
const H = db.identity_history, seen = new Map(), out = [];
const add = (a, owner) => { const k = a.at.getTime() + a.id; let row = seen.get(k);
  if (!row) { row = {at: a.at.getTime(), id: a.id, outcome: (a.after || {}).outcome || '', failures: (a.after || {}).failures || 0};
    seen.set(k, row); out.push(row); }
  if (owner) row.for = Array.from(new Set((row.for || []).concat([owner]))); };
""" + POSTER_SUBJECTS_JS + """
WINDOWS.forEach(w => {
  const titled = H.find({kind: 'answer:identity_question_markers', at: {$gte: new Date(w.lo), $lte: new Date(w.hi)}, id: new RegExp(w.needle, 'i')})
    .sort({at: 1}).limit(200).toArray();
  titled.forEach(a => add(a));
  // the record asks its family filed after its first title ask (candidates found by the title answers)
  if (titled.length) H.find({kind: 'answer:identity_question_markers', id: /\\|record\\|/, at: {$gte: titled[0].at, $lte: new Date(w.hi)}})
    .sort({at: 1}).limit(300).forEach(a => add(a));
  // the posters its decision compares (its candidates', its members' own), each tagged with the reversal it belongs to
  const subjects = posterSubjects(w.members, w.films);
  H.find({kind: 'answer:identity_question_markers', id: /\\|poster\\|/, at: {$gte: new Date(w.lo), $lte: new Date(w.hi)}})
    .sort({at: 1}).limit(300).forEach(a => { if (subjects.has(posterSubject(a.id))) add(a, w.key); });
});
print(JSON.stringify(out));
"""

ALL_LISTINGS_QUERY = """// all-listings
const H = db.identity_history, seen = new Set(), out = [];
WINDOWS.forEach(w => H.find({kind: 'listing', at: {$gte: new Date(w.lo), $lte: new Date(w.hi)}, id: {$in: w.members}}, {id: 1, at: 1})
  .limit(200).forEach(l => { const k = l.at.getTime() + l.id; if (!seen.has(k)) { seen.add(k); out.push({at: l.at.getTime(), id: l.id}); } }));
print(JSON.stringify(out));
"""
