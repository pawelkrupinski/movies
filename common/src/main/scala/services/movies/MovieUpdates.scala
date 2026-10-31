package services.movies

import models.Source
import org.bson.conversions.Bson
import org.mongodb.scala.bson.BsonDateTime
import org.mongodb.scala.model.Updates

private[movies] object MovieUpdates {
  // Translate a `MovieRecordPatch` into a `$set`/`$unset` Mongo update. Each
  // scalar field gets its own atom; the `data` map gets per-source
  // `sourceData.<sourceName>` paths so a Tmdb-only refresh doesn't touch a
  // cinema's slot and vice versa. `updatedAt` bumps alongside the real change — and
  // under the slots split that bump is sometimes the ONLY atom: a slots-only change
  // reaches here with an empty patch precisely so the write fires a change event on the
  // channel the projector listens to. `updateIfPresent` still skips a patch that is empty
  // with nothing else to announce.
  def of(p: MovieRecordPatch, updatedAt: java.time.Instant): Bson = {
    val atoms = scala.collection.mutable.ListBuffer.empty[Bson]
    def scalar[A](field: String, u: FieldUpdate[A], toBson: A => org.bson.BsonValue): Unit = u match {
      case FieldUpdate.NoChange => ()
      case FieldUpdate.Unset    => atoms += Updates.unset(field)
      case FieldUpdate.SetTo(v) => atoms += Updates.set(field, toBson(v))
    }
    scalar("imdbId",            p.imdbId,            (s: String) => new org.mongodb.scala.bson.BsonString(s))
    scalar("imdbRating",        p.imdbRating,        (d: Double) => new org.mongodb.scala.bson.BsonDouble(d))
    scalar("metascore",         p.metascore,         (i: Int)    => new org.mongodb.scala.bson.BsonInt32(i))
    scalar("filmwebUrl",        p.filmwebUrl,        (s: String) => new org.mongodb.scala.bson.BsonString(s))
    scalar("filmwebRating",     p.filmwebRating,     (d: Double) => new org.mongodb.scala.bson.BsonDouble(d))
    scalar("rottenTomatoes",    p.rottenTomatoes,    (i: Int)    => new org.mongodb.scala.bson.BsonInt32(i))
    scalar("tmdbId",            p.tmdbId,            (i: Int)    => new org.mongodb.scala.bson.BsonInt32(i))
    scalar("tmdbBasis",         p.tmdbBasis,         (s: String) => new org.mongodb.scala.bson.BsonString(s))
    scalar("tmdbProvisional",   p.tmdbProvisional,   (b: Boolean) => org.mongodb.scala.bson.BsonBoolean(b))
    scalar("wikidataId",        p.wikidataId,        (s: String) => new org.mongodb.scala.bson.BsonString(s))
    scalar("metacriticUrl",     p.metacriticUrl,     (s: String) => new org.mongodb.scala.bson.BsonString(s))
    scalar("rottenTomatoesUrl", p.rottenTomatoesUrl, (s: String) => new org.mongodb.scala.bson.BsonString(s))
    scalar("searchTitle",       p.searchTitle,       (s: String) => new org.mongodb.scala.bson.BsonString(s))
    scalar("tmdbAttempt",       p.tmdbAttempt,       (a: services.resolution.TmdbAttempt) =>
      new org.bson.BsonDocument("evidence", new org.mongodb.scala.bson.BsonString(a.evidence))
        .append("at", BsonDateTime(a.at.toEpochMilli)))
    scalar("retainedSynopses", p.retainedSynopses, (m: Map[Source, String]) => {
      val doc = new org.bson.BsonDocument()
      m.foreach { case (source, synopsis) => doc.put(source.displayName, new org.mongodb.scala.bson.BsonString(synopsis)) }
      doc
    })
    p.data.foreach {
      case (source, FieldUpdate.SetTo(sd)) => atoms += Updates.set(s"sourceData.${source.displayName}", sd)
      case (source, FieldUpdate.Unset)     => atoms += Updates.unset(s"sourceData.${source.displayName}")
      case (_, FieldUpdate.NoChange)       => ()
    }
    atoms += Updates.set("updatedAt", BsonDateTime(updatedAt.toEpochMilli))
    Updates.combine(atoms.toSeq*)
  }
}
