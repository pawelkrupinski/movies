// Assertions for schema-profile.js. No Mongo:
//
//   mongosh --nodb --quiet --file scripts/mongo/schema-profile.js --file scripts/mongo/schema-profile-spec.js
//
// Exits 0 when every case passes, 1 otherwise.

let failures = 0;
function check(what, got, expected) {
  const g = JSON.stringify(got), e = JSON.stringify(expected);
  if (g === e) { print(`  ok   ${what}`); return; }
  failures++;
  print(`  FAIL ${what}: expected ${e}, got ${g}`);
}

print("[spec] schema profile");

const fields = SchemaProfile.add({}, [
  { _id: 1, title: "Lalka", year: 1968, rating: 7.5, tags: ["drama", "classic"], at: new Date(0) },
  { _id: 2, title: "Lalka", year: null, film: { tmdb: 1, imdb: "tt1" } },
]);

check("a field in every document counts each once", fields.title, { types: { string: 2 }, count: 2 });
check("a field's types are tallied per value", fields.year, { types: { int: 1, null: 1 }, count: 2 });
check("a fractional number is a double", fields.rating.types, { double: 1 });
check("a date is a date", fields.at.types, { date: 1 });
check("an array's elements profile under []", fields["tags[]"], { types: { string: 2 }, count: 1 });
check("a sub-document's names are field paths", Object.keys(fields).filter(k => k.startsWith("film.")).sort(), ["film.imdb", "film.tmdb"]);

const many = {};
for (let i = 0; i < 20; i++) many["k" + i] = i;
const maps = SchemaProfile.add({}, [{ byVenue: { "https://kino.pl/a": 1, "https://kino.pl/b": 2 } }, { counts: many }]);
check("an object keyed by URLs is a map", maps.byVenue.map, true);
check("...whose entries profile under {key}", maps["byVenue.{key}"].types, { int: 2 });
check("an object with more keys than MapKeys is a map", maps.counts.map, true);
check("...walked through at most five entries", maps["counts.{key}"].types, { int: 5 });

const deep = SchemaProfile.add({}, [{ a: { b: { c: { d: { e: 1 } } } } }]);
check("paths deeper than Depth are not walked", Object.keys(deep).sort(), ["a", "a.b", "a.b.c", "a.b.c.d"]);

if (failures) { print(`[spec] ${failures} failed`); quit(1); }
print("[spec] all passed");
