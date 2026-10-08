// Read-only: samples every collection of the named databases and prints, as one JSON object per
// collection name, the databases holding it, their document counts, and each field path's types and
// presence (schema-profile.js). Collections of the same name across databases (the country DBs) merge.
//
//   SAMPLE_DBS=kinowo,kinowo_uk,kinowo_de,kinowo_us,kinowo_es \
//     mongosh "$MONGODB_URI" --quiet --file scripts/mongo/schema-profile.js --file scripts/mongo/sample-schema.js
//
// SAMPLE_DBS defaults to the database the URI names. SAMPLE_SIZE (default 300) is the $sample per
// collection. Never writes.

const dbNames = (process.env.SAMPLE_DBS || db.getName()).split(",").map(s => s.trim()).filter(Boolean);
const sampleSize = Number(process.env.SAMPLE_SIZE || 300);
const schema = {};
for (const name of dbNames) {
  const d = db.getSiblingDB(name);
  for (const info of d.getCollectionInfos({ type: "collection" })) {
    const coll = d.getCollection(info.name);
    const entry = schema[info.name] || (schema[info.name] = { dbs: [], docs: 0, sampled: 0, fields: {} });
    entry.dbs.push(name);
    entry.docs += coll.estimatedDocumentCount();
    let docs;
    try { docs = coll.aggregate([{ $sample: { size: sampleSize } }], { allowDiskUse: true }).toArray(); }
    catch (e) { entry.error = String(e.message || e); continue; }
    entry.sampled += docs.length;
    SchemaProfile.add(entry.fields, docs);
  }
}
print(JSON.stringify(schema));
