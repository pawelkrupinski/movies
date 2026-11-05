package services.identity

import models.Country
import org.mongodb.scala.MongoClient
import services.movies.{ListingKey, TitleNormalizer}

import java.time.{Clock, Instant}

/**
 * THE OFFLINE COUNTERFACTUAL FLIP PREDICTOR, on the LOCAL prod mirror (`mongodb://127.0.0.1:28017`, read-only): the
 * country's corpus at `--at` ([[MirrorRecompute.corpusAt]]), its family index ([[FamilyIndex.build]]) and, for every
 * family of two listings or more, the family resolved again under each SINGLE perturbation -- the model's take, as
 * [[FamilyConvergence]] walks its lattice (the same [[PopulationMonotonicity.Without]] context, the same
 * [[IdentityResolver.resolveRegion]]):
 *   - REMOVE: each member taken out (the context without it);
 *   - ADD: each listing of ANOTHER family that carries one of this family's block keys put in (at most `--max-adds` per
 *     family, the first by listing key; the corpus context already holds it).
 * A listing CAN FLIP when some perturbation of its family gives it a take other than the one it has. A prediction of
 * what a later arrival or departure could move, not a rule; nothing here moves a take.
 *
 * TSV: listingKey (venue|rawTitle), takeAt, canFlip, nPerturbationsFlipping, firstTrigger, familySize, nPerturbations.
 *
 * One thread by default: the stored TMDB lookups hold one prefetch's documents per instance, so concurrent family
 * resolves over them race (an NPE in StoredTmdbLookups.load).
 *
 *   worker/Test/runMain services.identity.FamilyCounterfactual <cc> --at ISO [--out FILE] [--threads 1] [--max-adds 10]
 */
object FamilyCounterfactual {

  /** One listing's row. `firstTrigger`: the perturbation (`-venue|title` removed, `+venue|title` added) and the take it gives. */
  final case class Row(listing: ListingKey, takeAt: Option[String], flipping: Int, firstTrigger: Option[String], familySize: Int, perturbations: Int) {
    def canFlip: Boolean = flipping > 0
    def tsv: String = Seq(s"${listing.venue}|${listing.rawTitle}", show(takeAt), canFlip, flipping, firstTrigger.getOrElse("-"), familySize, perturbations).mkString("\t")
  }
  val Header = "listingKey\ttakeAt\tcanFlip\tnPerturbationsFlipping\tfirstTrigger\tfamilySize\tnPerturbations"

  def show(take: Option[String]): String = take.fold("pending")(t => if (t == TakeFlips.NoFilm) "none" else t)

  def rows(listings: Seq[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer, inputs: ResolveInputs, threads: Int, maxAdds: Int): Seq[Row] = {
    val all        = listings.sorted.distinctBy(_.key)
    val built      = FamilyIndex.build(all, lookups, normalizer, inputs, threads)
    val generation = new CandidateGeneration(all, lookups, normalizer, inputs.pins, inputs.decorations, lazyLookups = false)
    val without    = new PopulationMonotonicity.Without.Index(generation.nodes)
    val byKey      = all.map(l => l.key -> l).toMap
    val holders    = FamilyIndex.blockKeyHolders(built.index)

    def takesOf(present: Seq[Listing], absent: Set[ListingKey]): Map[ListingKey, Option[String]] = {
      val regions = IdentityResolver.resolveRegion(present, lookups, normalizer, inputs.calibration, inputs.pins, inputs.decorations,
        without(generation.context, absent))
      regions.flatMap(_.decisions).flatMap(d => d.members.map(_ -> PopulationMonotonicity.takeOf(d))).toMap
    }

    def rowsOf(entry: FamilyIndex.Entry): Seq[Row] = {
      val members = all.filter(l => entry.members.contains(l.key))
      val keys    = members.map(_.key).toSet
      val removals = members.map { gone =>
        (s"-${gone.venue}|${gone.rawTitle}", takesOf(members.filterNot(_ == gone), Set(gone.key)), keys - gone.key)
      }
      val outside = entry.blockKeys.toSeq.sorted.flatMap(holders(_).toSeq).distinct.filterNot(keys).sortBy(_.toString).flatMap(byKey.get).take(maxAdds)
      val additions = outside.map(added => (s"+${added.venue}|${added.rawTitle}", takesOf(members :+ added, Set.empty), keys))
      val perturbed = removals ++ additions
      members.map { l =>
        val was     = entry.takes(l.key)
        val flipped = perturbed.flatMap { case (name, takes, present) =>
          Option.when(present(l.key) && takes.get(l.key).exists(_ != was))(name -> takes(l.key))
        }
        Row(l.key, was, flipped.size, flipped.headOption.map { case (name, take) => s"$name -> ${show(take)}" }, members.size, perturbed.count(_._3(l.key)))
      }
    }

    val entries = built.index.entries.filter(_.members.sizeIs >= 2)
    val pool    = tools.DaemonExecutors.boundedEC("family-counterfactual", threads)
    try entries.map(e => pool.submit(() => rowsOf(e))).flatMap(_.get)
    finally { val _ = pool.shutdownNow() }
  }

  def main(args: Array[String]): Unit = {
    val clock   = Clock.systemUTC()
    val cc      = args.headOption.getOrElse(sys.error("a country code"))
    val country = Country.all.find(_.code == cc).getOrElse(sys.error(s"no country $cc"))
    def opt(name: String) = args.toList.sliding(2).collectFirst { case List(`name`, value) => value }
    val at      = Instant.parse(opt("--at").getOrElse(sys.error("--at ISO")))
    val threads = opt("--threads").fold(1)(_.toInt)
    val maxAdds = opt("--max-adds").fold(10)(_.toInt)
    val out     = java.nio.file.Path.of(opt("--out").getOrElse(s"target/family-counterfactual-$cc.tsv"))
    val client  = MongoClient("mongodb://127.0.0.1:28017/?directConnection=true")
    try {
      val db         = client.getDatabase(services.MongoConnection.mirrorDbFor(country.mongoDb))
      val normalizer = TitleNormalizer.forCountry(country)
      val inputs     = ResolveInputs.of(new MongoPinStore(Some(db)).all())
      val started    = tools.Stopwatch.start()
      val corpus     = MirrorRecompute.corpusAt(db, country, at, normalizer, clock)
      System.err.println(f"[family-counterfactual] $cc at $at: ${corpus.listings.size} listings (${started.seconds}%.0fs)")
      val result     = rows(corpus.listings, corpus.lookups, normalizer, inputs, threads, maxAdds)
      java.nio.file.Files.createDirectories(out.toAbsolutePath.getParent)
      java.nio.file.Files.writeString(out, (Header +: result.map(_.tsv)).mkString("", "\n", "\n"))
      println(f"## $cc $at: ${result.size} listings in families of 2+, ${result.count(_.canFlip)} can flip -> $out (${started.seconds}%.0fs)")
    } finally client.close()
  }
}
