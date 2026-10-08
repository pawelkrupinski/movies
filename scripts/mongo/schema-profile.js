// The pure half of sample-schema.js: what a sample of documents says about a collection's fields —
// every field path, the BSON types seen at it, in how many documents it appears, and whether an
// object at it is a MAP (keys that are data, not names: too many of them, or ids/URLs as keys),
// whose entries are then profiled under `<path>.{key}` instead of one path per key.
// No Mongo needed: schema-profile-spec.js asserts it with `mongosh --nodb`.

const SchemaProfile = {
  MapKeys: 12,   // more keys than this and an object is read as a map
  Depth: 4,      // field paths deeper than this are not walked

  typeOf(v) {
    if (v === null) return "null";
    if (Array.isArray(v)) return "array";
    if (v instanceof Date) return "date";
    if (typeof v === "object" && v._bsontype) return v._bsontype === "ObjectId" ? "objectId" : v._bsontype.toLowerCase();
    if (typeof v === "number") return Number.isInteger(v) ? "int" : "double";
    return typeof v;
  },

  isMap(obj) {
    const keys = Object.keys(obj);
    return keys.length > SchemaProfile.MapKeys || keys.some(k => /[ .|:\/]/.test(k) && k.length > 3);
  },

  // Adds `docs` to `fields` ({path: {types: {type: n}, count, map?}}) and returns it.
  add(fields, docs) {
    for (const doc of docs) {
      const seen = new Set();
      const walk = (v, path, depth) => {
        const t = SchemaProfile.typeOf(v);
        const f = fields[path] || (fields[path] = { types: {}, count: 0 });
        if (!seen.has(path)) { f.count++; seen.add(path); }
        f.types[t] = (f.types[t] || 0) + 1;
        if (depth >= SchemaProfile.Depth) return;
        if (t === "object") {
          if (SchemaProfile.isMap(v)) { f.map = true; for (const k of Object.keys(v).slice(0, 5)) walk(v[k], path + ".{key}", depth + 1); }
          else for (const k of Object.keys(v)) walk(v[k], path + "." + k, depth + 1);
        } else if (t === "array" && v.length) {
          for (const x of v.slice(0, 3)) walk(x, path + "[]", depth + 1);
        }
      };
      for (const k of Object.keys(doc)) walk(doc[k], k, 1);
    }
    return fields;
  },
};
