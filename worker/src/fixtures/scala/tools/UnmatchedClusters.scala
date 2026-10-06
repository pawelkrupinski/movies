package tools

import models.{Country, Source}
import org.bson.{BsonArray, BsonDocument, BsonDouble, BsonInt32, BsonNull, BsonString, BsonValue}
import org.bson.json.{JsonMode, JsonWriterSettings}
import services.identity._
import services.identity.agreement.{AgreementStage, FamilyAnswers, InMemoryAgreementVerdicts, VoterFamily}
import services.movies.{ListingKey, TitleNormalizer}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentHashMap
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.jdk.CollectionConverters._

/**
 * The clusters the identity model leaves UNMATCHED on a recorded full corpus, captured with every answer the resolver
 * and the agreement stage read for them — TMDB's (candidate searches, film records, venue detail pages, IMDb-id finds),
 * the other film database families' (`identity_family_answers` documents, as [[FamilyAnswerStore]] files them) and the
 * venue and TMDB posters' hashes (filed beside them by [[PosterAnswerStore]]) —
 * so the resolver plus the agreement over them is a pure function of one checked-in file per country
 * (`test/resources/fixtures/identity-unmatched/<cc>.json.gz`). `UnmatchedClustersCaptureIntegrationSpec`
 * records it from the recorded corpora and the signal-combination experiment's answer cache (real answers, never
 * hand-written); `UnmatchedClustersRatchetSpec` replays it: zero wrong takes, and the right ones only ever grow.
 */
object UnmatchedClusters {

  val Directory: Path = Path.of("test", "resources", "fixtures", "identity-unmatched")
  def fixturePath(country: Country): Path = Directory.resolve(s"${country.code}.json.gz")

  /** The venue film pages whose catalogue links the catalogue take reads, as the worker declares them. */
  val CataloguePages: Seq[CatalogueLinkPages] =
    Seq(services.cinemas.common.FlicksClient.CatalogueLinkPages, services.cinemas.pl.KinotekaClient.CatalogueLinkPages)

  /** A country's unmatched clusters — the model's decisions on them over the whole corpus, and their listings — and
   *  every answer the agreement stage read for them. The decisions are the whole corpus's, never a re-resolve of the
   *  clusters alone: alone, a cluster loses the neighbours that hold it apart from a namesake (a Met relay alone took
   *  the Royal Opera's record of the same opera). */
  final case class Capture(country: Country, listings: Seq[Listing], decisions: Seq[ResolverDecision], queries: Map[CandidateQuery, Seq[Hit]],
                           films: Map[Int, Option[IdentityMeasures.Film]], details: Map[String, Option[DetailFacts]],
                           withDetail: Set[String], families: Map[String, BsonDocument], finds: Map[String, Option[Int]])

  // ── what the agreement makes of the clusters ─────────────────────────────────────────────

  /** The model's no-match decisions, and what the agreement stage made of them. */
  final case class Outcome(model: Resolution, agreed: Resolution, stage: AgreementStage)

  /** The families each country asks, as the worker wires them (`IdentityCutoverWiring.agreementFamilies`). */
  def familiesOf(country: Country): Seq[VoterFamily] =
    Seq(VoterFamily.Imdb, VoterFamily.Wiki, VoterFamily.Metacritic, VoterFamily.RottenTomatoes) ++
      Option.when(modules.wiring.IdentityCutoverWiring.FilmwebVoting(country.code))(VoterFamily.Filmweb)

  /** The model's decisions as a resolution the agreement stage is handed. */
  def resolutionOf(decisions: Seq[ResolverDecision]): Resolution =
    Resolution(decisions, decisions.map(_.members.size).sum, decisions.zipWithIndex.flatMap { case (d, i) => d.members.map(_ -> i) }.toMap,
      Nil, Nil, 0, 0, 0, 0, 0, Map.empty)

  /** The agreement stage over the model's decisions, as the worker runs it on the way to the projection. */
  def agree(decisions: Seq[ResolverDecision], listings: Seq[Listing], lookups: IdentityLookups, families: Map[VoterFamily, FamilyAnswers],
            version: Long, tmdbOf: String => Answer[Option[Int]], normalizer: TitleNormalizer, posters: PosterAnswers,
            identities: Seq[VoterFamily], catalogue: CatalogueAnswers, listedOn: Option[VoterFamily] = None): Outcome = {
    val model = resolutionOf(decisions)
    val stage = new AgreementStage(families, lookups, normalizer, IdentityCalibration.resolver, tmdbOf, new InMemoryAgreementVerdicts,
      clock = SpecClock.Pinned, posters = posters, tmdb = Some(lookups), identities = identities, catalogue = catalogue, listedOn = listedOn)
    val byKey = listings.map(l => l.key -> l).toMap
    Outcome(model, stage.apply(model, byKey.get, version), stage)
  }

  /** The capture replayed: `Unknown` for anything it does not hold — a stale fixture, re-captured, never guessed. */
  def replay(capture: Capture): Outcome = {
    val docs = new InMemoryTmdbDocuments
    docs.put(TmdbKind.Family, capture.families.toSeq)
    val store = new FamilyAnswerStore(docs, SpecClock.Pinned)
    agree(capture.decisions, capture.listings, new Replay(capture), familiesOf(capture.country).map(f => f -> store.answers(f)).toMap, version = 1,
      imdb => capture.finds.get(imdb).fold[Answer[Option[Int]]](Answer.Unknown)(Answer.Known(_)), TitleNormalizer.forCountry(capture.country),
      new PosterAnswerStore(store, SpecClock.Pinned), modules.wiring.IdentityCutoverWiring.identities(capture.country.code),
      new CatalogueAnswerStore(store, SpecClock.Pinned, CataloguePages), modules.wiring.IdentityCutoverWiring.listedOn(capture.country.code))
  }

  /** One listing's take: the film its cluster took (TMDB's, or a fallback film), the IMDb id it is known by, and the
   *  fallback film's id where another database's stands for it ("filmweb:10105049", "wikidata:Q141180912"). */
  final case class Take(country: String, venue: String, rawTitle: String, tmdb: Option[Int], imdb: Option[String], basis: String, title: String,
                        agreed: Map[String, String] = Map.empty, standsOn: Option[String] = None) {
    /** Every id it is known by: TMDB's, IMDb's, the one it stands on, and each agreeing family's own ("filmweb:880000",
     *  "wikidata:Q1"). */
    def ids: Set[String] = tmdb.map(id => s"tmdb:$id").toSet ++ imdb.map(id => s"imdb:$id") ++ standsOn ++
      agreed.map { case (family, id) => s"${if (family == "wiki") "wikidata" else family}:$id" }
    def film: String = tmdb.fold(imdb.fold(standsOn.getOrElse(""))(id => s"imdb:$id"))(id => s"tmdb:$id")
  }

  private val NamedImdb = """ (tt\d+)$""".r.unanchored

  /** Every listing whose cluster took a film, in a stable order. */
  def takes(capture: Capture, outcome: Outcome): Seq[Take] = {
    val byKey = capture.listings.map(l => l.key -> l).toMap
    outcome.agreed.decisions.filter(d => d.film.isDefined || d.fallback.isDefined).flatMap { d =>
      val record = d.film.flatMap(id => outcome.agreed.films.get(id).orElse(capture.films.get(id).flatten))
      val imdb = d.fallback.filter(_.source == "imdb").map(_.id)
        .orElse(record.filter(_.imdbNumber > 0).map(f => f"tt${f.imdbNumber}%07d"))
        .orElse(d.explanation.lastOption.collect { case NamedImdb(id) => id })
      val title = record.map(f => s"${f.title} (${f.year.getOrElse("?")})").getOrElse(d.explanation.lastOption.getOrElse(""))
      val standsOn = d.fallback.filter(_.source != "imdb").map(taken => s"${taken.source}:${taken.id}")
      d.members.flatMap(byKey.get).map(l => Take(capture.country.code, l.venue, l.rawTitle, d.film, imdb, d.basis.toString, title, d.agreed, standsOn))
    }.sortBy(t => (t.country, t.venue, t.rawTitle))
  }

  // ── labels: what each listing is, and is not ─────────────────────────────────────────────

  /** A judgement of one listing (`venue` "*" for every venue billing `rawTitle`): `film` ("tmdb:913760",
   *  "imdb:tt16315948") is `right` or `wrong` for it. */
  final case class Label(country: String, venue: String, rawTitle: String, film: String, right: Boolean, note: String) {
    def covers(take: Take): Boolean = take.country == country && take.rawTitle == rawTitle && (venue == "*" || venue == take.venue)
  }

  def readLabels(path: Path): Seq[Label] = Files.readAllLines(path, StandardCharsets.UTF_8).asScala.toSeq.drop(1).filter(_.nonEmpty).map { line =>
    line.split("\t", -1) match {
      case Array(country, venue, raw, film, verdict, note) => Label(country, venue, raw, film, verdict == "right", note)
      case _ => throw new IllegalArgumentException(s"malformed label line: $line")
    }
  }

  /** A take's verdict by the labels: `Some(true)` right, `Some(false)` wrong — a label denies its film, or the labels
   *  name its listing's right film in a database the take has an id in and this is not it — `None` unjudged. */
  def verdict(take: Take, labels: Seq[Label]): Option[Boolean] = {
    val about     = labels.filter(_.covers(take))
    val databases = take.ids.map(_.takeWhile(_ != ':'))
    if (about.exists(l => !l.right && take.ids(l.film))) Some(false)
    else if (about.exists(l => l.right && take.ids(l.film))) Some(true)
    else if (about.exists(l => l.right && databases(l.film.takeWhile(_ != ':')))) Some(false)
    else None
  }

  def takeLine(take: Take): String = Seq(take.country, take.venue, take.rawTitle, take.film, take.title).mkString("\t")
  def readTakeLines(path: Path): Set[(String, String, String, String)] =
    if (!Files.exists(path)) Set.empty
    else Files.readAllLines(path, StandardCharsets.UTF_8).asScala.toSeq.drop(1).filter(_.nonEmpty).map(_.split("\t", -1)).map(a => (a(0), a(1), a(2), a(3))).toSet

  // ── recording and replaying the lookups ──────────────────────────────────────────────────

  /** `inner`, every answer it gave noted — what a capture writes. */
  final class Recording(inner: IdentityLookups) extends IdentityLookups {
    val queries    = new ConcurrentHashMap[CandidateQuery, Seq[Hit]]()
    val films      = new ConcurrentHashMap[Int, Option[IdentityMeasures.Film]]()
    val details    = new ConcurrentHashMap[String, Option[DetailFacts]]()
    val withDetail = ConcurrentHashMap.newKeySet[String]()
    override def hasDetail(listing: Listing): Boolean = {
      val has = inner.hasDetail(listing)
      if (has) withDetail.add(ListingKey.serialised(listing.key))
      has
    }
    override def prefetch(queries: Iterable[CandidateQuery], films: Iterable[Int], details: Iterable[Listing]): Unit = inner.prefetch(queries, films, details)
    override def prefetchAnswered(): Unit = inner.prefetchAnswered()
    override def detail(listing: Listing): Answer[Option[DetailFacts]] = noted(inner.detail(listing))(details.put(ListingKey.serialised(listing.key), _))
    override def candidates(query: CandidateQuery): Answer[Seq[Hit]] = noted(inner.candidates(query))(queries.put(query, _))
    override def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = noted(inner.film(tmdbId))(films.put(tmdbId, _))
    private def noted[A](answer: Answer[A])(keep: A => Any): Answer[A] = { answer.toOption.foreach(keep); answer }
  }

  /** The captured answers as the resolver's lookups. */
  final class Replay(capture: Capture) extends IdentityLookups {
    override def hasDetail(listing: Listing): Boolean = capture.withDetail(ListingKey.serialised(listing.key))
    override def detail(listing: Listing): Answer[Option[DetailFacts]] = known(capture.details.get(ListingKey.serialised(listing.key)))
    override def candidates(query: CandidateQuery): Answer[Seq[Hit]] = known(capture.queries.get(query))
    override def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = known(capture.films.get(tmdbId))
    private def known[A](held: Option[A]): Answer[A] = held.fold[Answer[A]](Answer.Unknown)(Answer.Known(_))
  }

  // ── the file ─────────────────────────────────────────────────────────────────────────────

  def write(path: Path, capture: Capture): Unit = {
    val d = new BsonDocument("country", new BsonString(capture.country.code))
      .append("listings", array(capture.listings.sortBy(_.sortKey).map(listingDoc)))
      .append("decisions", array(capture.decisions.sortBy(_.members.headOption.map(ListingKey.serialised).getOrElse("")).map(ResolverDecisionBson.encode)))
      .append("queries", array(capture.queries.toSeq.sortBy(_._1.sortKey).map { case (q, hits) => queryDoc(q).append("hits", array(hits.map(hitDoc))) }))
      .append("films", array(capture.films.toSeq.sortBy(_._1).map { case (id, f) => new BsonDocument("id", new BsonInt32(id)).append("film", IdentityAnswerBson.film(f)) }))
      .append("details", array(capture.details.toSeq.sortBy(_._1).map { case (k, f) => new BsonDocument("key", new BsonString(k)).append("detail", f.fold[BsonValue](new BsonNull)(detailDoc)) }))
      .append("withDetail", array(capture.withDetail.toSeq.sorted.map(new BsonString(_))))
      .append("families", array(capture.families.toSeq.sortBy(_._1).map { case (id, doc) => new BsonDocument("_id", new BsonString(id)).append("doc", doc) }))
      .append("finds", array(capture.finds.toSeq.sortBy(_._1).map { case (imdb, tmdb) => new BsonDocument("imdb", new BsonString(imdb)).append("tmdb", tmdb.fold[BsonValue](new BsonNull)(new BsonInt32(_))) }))
    Files.createDirectories(path.getParent)
    val out = new GZIPOutputStream(Files.newOutputStream(path))
    try out.write(d.toJson(JsonWriterSettings.builder().outputMode(JsonMode.RELAXED).build()).getBytes(StandardCharsets.UTF_8)) finally out.close()
  }

  def read(path: Path): Capture = {
    val in = new GZIPInputStream(Files.newInputStream(path))
    val d  = try BsonDocument.parse(new String(in.readAllBytes(), StandardCharsets.UTF_8)) finally in.close()
    val country = Country.all.find(_.code == d.getString("country").getValue).getOrElse(sys.error(s"no country in $path"))
    def docs(name: String) = d.getArray(name).getValues.asScala.toSeq.map(_.asDocument)
    Capture(country,
      docs("listings").map(listingOf),
      docs("decisions").map(ResolverDecisionBson.decode),
      docs("queries").map(q => queryOf(q) -> q.getArray("hits").getValues.asScala.toSeq.map(h => hitOf(h.asDocument))).toMap,
      docs("films").map(f => f.getInt32("id").getValue -> IdentityAnswerBson.filmOf(f.get("film"))).toMap,
      docs("details").map(x => x.getString("key").getValue -> Option(x.get("detail")).filterNot(_.isNull).map(v => detailOf(v.asDocument))).toMap,
      d.getArray("withDetail").getValues.asScala.map(_.asString.getValue).toSet,
      docs("families").map(f => f.getString("_id").getValue -> f.getDocument("doc")).toMap,
      docs("finds").map(f => f.getString("imdb").getValue -> Option(f.get("tmdb")).filterNot(_.isNull).map(_.asNumber.intValue)).toMap)
  }

  private def array(values: Seq[BsonValue]): BsonArray = new BsonArray(values.asJava)
  private def strings(values: Seq[String]): BsonArray = array(values.map(new BsonString(_)))
  private def stringsOf(d: BsonDocument, name: String): Seq[String] =
    Option(d.get(name)).toSeq.flatMap(_.asArray.getValues.asScala.map(_.asString.getValue))
  private def opt(d: BsonDocument, name: String): Option[String] = Option(d.get(name)).filterNot(_.isNull).map(_.asString.getValue)
  private def optInt(d: BsonDocument, name: String): Option[Int] = Option(d.get(name)).filterNot(_.isNull).map(_.asNumber.intValue)
  private def put(d: BsonDocument, name: String, value: Option[String]): BsonDocument = { value.foreach(v => d.append(name, new BsonString(v))); d }
  private def putInt(d: BsonDocument, name: String, value: Option[Int]): BsonDocument = { value.foreach(v => d.append(name, new BsonInt32(v))); d }

  private def listingDoc(l: Listing): BsonDocument = {
    val d = new BsonDocument("cinema", new BsonString(l.cinema.displayName)).append("key", new BsonString(ListingKey.serialised(l.key)))
      .append("rawTitle", new BsonString(l.rawTitle)).append("title", new BsonString(l.title)).append("cleanTitle", new BsonString(l.cleanTitle))
      .append("directors", strings(l.directors)).append("countries", strings(l.countries))
      .append("catalogueIds", array(l.catalogueIds.map(c => new BsonDocument("source", new BsonString(c.source)).append("id", new BsonString(c.id)))))
    putInt(d, "year", l.year); putInt(d, "runtime", l.runtime); put(d, "page", l.page); put(d, "originalTitle", l.originalTitle)
    if (!l.screenings.isEmpty) d.append("screenings", strings(l.screenings.days.map(_.toString)))
    put(d, "searchTitle", l.searchTitle); put(d, "poster", l.poster)
  }
  private def listingOf(d: BsonDocument): Listing = {
    val name   = d.getString("cinema").getValue
    val cinema = Source.byDisplayName.get(name).flatMap(Source.cinemaOf).getOrElse(sys.error(s"no venue named $name"))
    Listing(cinema, ListingKey.parse(d.getString("key").getValue).getOrElse(sys.error(s"bad key in ${d.toJson}")), d.getString("rawTitle").getValue,
      d.getString("title").getValue, d.getString("cleanTitle").getValue, optInt(d, "year"), stringsOf(d, "directors"), optInt(d, "runtime"),
      opt(d, "page"), opt(d, "originalTitle"), stringsOf(d, "countries"),
      d.getArray("catalogueIds").getValues.asScala.toSeq.map(_.asDocument).map(c => CatalogueId(c.getString("source").getValue, c.getString("id").getValue)),
      opt(d, "searchTitle"), opt(d, "poster"), ScreeningDays.of(stringsOf(d, "screenings").map(java.time.LocalDate.parse)))
  }
  private def queryDoc(q: CandidateQuery): BsonDocument = q match {
    case CandidateQuery.Title(text)       => new BsonDocument("title", new BsonString(text))
    case CandidateQuery.Director(name)    => new BsonDocument("director", new BsonString(name))
    case CandidateQuery.Imdb(title)       => new BsonDocument("imdb", new BsonString(title))
    case CandidateQuery.ImdbTitled(title) => new BsonDocument("imdbTitled", new BsonString(title))
  }
  private def queryOf(d: BsonDocument): CandidateQuery =
    opt(d, "title").map(CandidateQuery.Title(_)).orElse(opt(d, "director").map(CandidateQuery.Director(_)))
      .orElse(opt(d, "imdb").map(CandidateQuery.Imdb(_))).orElse(opt(d, "imdbTitled").map(CandidateQuery.ImdbTitled(_)))
      .getOrElse(sys.error(s"no query in ${d.toJson}"))
  private def hitDoc(h: Hit): BsonDocument = {
    val d = new BsonDocument("id", new BsonInt32(h.tmdbId)).append("title", new BsonString(h.title)).append("popularity", new BsonDouble(h.popularity))
    put(d, "originalTitle", h.originalTitle); putInt(d, "year", h.year)
  }
  private def hitOf(d: BsonDocument): Hit =
    Hit(d.getInt32("id").getValue, d.getString("title").getValue, opt(d, "originalTitle"), optInt(d, "year"), d.getNumber("popularity").doubleValue)
  private def detailDoc(f: DetailFacts): BsonDocument = {
    val d = new BsonDocument("directors", strings(f.directors)).append("countries", strings(f.countries))
    putInt(d, "year", f.year); putInt(d, "runtime", f.runtime); put(d, "originalTitle", f.originalTitle)
  }
  private def detailOf(d: BsonDocument): DetailFacts =
    DetailFacts(optInt(d, "year"), stringsOf(d, "directors"), optInt(d, "runtime"), opt(d, "originalTitle"), stringsOf(d, "countries"))
}
