package integration

import services.identity.{Evidence, FilmFacts, Listing}
import services.movies.ListingKey

/**
 * The historical identity incidents as LABELLED CHECKS (docs/design/identity-resolver.md §7a):
 * what each incident says must be true of the answer, judged the same way for the resolver and
 * for today's pipeline. They are test labels only — no title, year or id below reaches the
 * resolver. A check whose listings a corpus does not hold is "n/a" there.
 */
object IdentityHistoricalChecks {

  /** Selects listings: by a regex on the lower-cased raw title, and optionally the year the
   *  listing states (its field or its title's bracket) and a regex on its directors. */
  final case class Sel(title: String, year: Option[Int] = None, director: Option[String] = None) {
    private val re = title.r
    def apply(l: Listing, e: Evidence): Boolean =
      re.findFirstIn(l.rawTitle.toLowerCase(java.util.Locale.ROOT)).isDefined &&
        year.forall(y => e.statedYear.contains(y)) &&
        director.forall(d => e.directors.exists(_.toLowerCase(java.util.Locale.ROOT).matches(s".*$d.*")))
  }

  enum Expect {
    /** No listing of `a` is one film with a listing of `b`. */
    case Apart(a: Sel, b: Sel)
    /** Every listing selected is film `tmdbId`. */
    case Film(s: Sel, tmdbId: Int)
    /** No listing selected is film `tmdbId`. */
    case NotFilm(s: Sel, tmdbId: Int)
    /** No listing selected is a film dated `year`. */
    case NotYear(s: Sel, year: Int)
    /** Undecidable from the evidence: reported, never judged. */
    case Report(s: Sel)
  }

  final case class Check(country: String, name: String, expect: Expect)

  val All: Seq[Check] = {
    import Expect._
    Seq(
      Check("us", "A Star Is Born 1954 vs 2018", Apart(Sel("^a star is born", year = Some(1954)), Sel("^a star is born", year = Some(2018)))),
      Check("us", "A Star Is Born 1976 vs 2018", Apart(Sel("^a star is born", year = Some(1976)), Sel("^a star is born", year = Some(2018)))),
      Check("pl", "Met Samson i Dalila 2026 is not DeMille's", NotFilm(Sel("samson i dalila", year = Some(2026)), 29993)),
      Check("pl", "Così fan tutte broadcast is not Brass 1992", NotYear(Sel("cos[iì] fan tutte"), 1992)),
      Check("de", "Zärtlich kreist die Faust is not Murnau's Faust", Apart(Sel("zärtlich kreist die faust"), Sel("^faust$"))),
      Check("de", "Zärtlich kreist die Faust is not tmdb 10728", NotFilm(Sel("zärtlich kreist die faust"), 10728)),
      Check("us", "It Ends with Us is not It Ends", Apart(Sel("^it ends with us"), Sel("^it ends$"))),
      Check("us", "It Ends with Us is 1079091", Film(Sel("^it ends with us$"), 1079091)),
      Check("pl", "Happy Together is not Kim Jeong-hwan's 2018 film", NotFilm(Sel("happy together"), 551655)),
      Check("us", "It 1990 vs It 2017", Apart(Sel("^it\\b.*1990|^it$", year = Some(1990)), Sel("^it\\b", year = Some(2017)))),
      Check("uk", "Catching Fire vs Mockingjay", Apart(Sel("catching fire"), Sel("mockingjay"))),
      Check("us", "Catching Fire vs Mockingjay", Apart(Sel("catching fire"), Sel("mockingjay"))),
      Check("uk", "Mockingjay Part 1 vs Part 2", Apart(Sel("mockingjay.*(part 1|part one)"), Sel("mockingjay.*(part 2|part two)"))),
      Check("uk", "Mockingjay Part 2 (2026) is the 2015 film", Film(Sel("mockingjay.*part 2.*2026"), 131634)),
      Check("pl", "Skarpetek 3 vs 4", Apart(Sel("skarpetek.*\\b3\\b"), Sel("skarpetek.*\\b4\\b"))),
      Check("pl", "Decorated Lalka spellings are Kawalski's Lalka",
        Film(Sel("^lalka – salon kultury|^lalka \\(dolly\\)|oficjalna premiera: lalka"), 1321666)),
      Check("pl", "Opętanie | klasyka w 4k (undecidable)", Report(Sel("opętanie \\| klasyka"))),
      Check("uk", "Belle 2013 vs 2021", Apart(Sel("^belle", year = Some(2013)), Sel("^belle", year = Some(2021)))),
      Check("us", "Planet of the Apes: Schaffner vs Burton", Apart(Sel("^planet of the apes$", director = Some("schaffner")), Sel("^planet of the apes \\(2001\\)"))),
      Check("pl", "Avengers: Koniec gry re-release is Endgame", Film(Sel("avengers: koniec gry"), 299534)),
      Check("de", "Sinn und Sinnlichkeit 1995 vs 2026", Apart(Sel("sinn und sinnlichkeit", year = Some(1995)), Sel("sinn und sinnlichkeit", year = Some(2026)))),
      Check("de", "Bad Apples 2018 vs 2025", Apart(Sel("^bad apples", year = Some(2018)), Sel("^bad apples", year = Some(2025))))
    )
  }

  /** One system's answer, as a check reads it: the cluster a listing is in and its film. */
  final case class Answer(clusterOf: ListingKey => Option[Any], filmOf: ListingKey => Option[FilmFacts])

  enum Verdict {
    case Pass, Fail, NotApplicable
    case Reported(what: String)
  }

  def judge(check: Check, listings: Seq[(Listing, Evidence)], answer: Answer): Verdict = {
    def pick(s: Sel) = listings.collect { case (l, e) if s(l, e) => l.key }
    check.expect match {
      case Expect.Apart(a, b) =>
        val (as, bs) = (pick(a), pick(b))
        if (as.isEmpty || bs.isEmpty) Verdict.NotApplicable
        else {
          val clustersA = as.flatMap(answer.clusterOf).toSet
          if (bs.flatMap(answer.clusterOf).exists(clustersA)) Verdict.Fail else Verdict.Pass
        }
      case Expect.Film(s, id) =>
        val ls = pick(s)
        if (ls.isEmpty) Verdict.NotApplicable else if (ls.forall(l => answer.filmOf(l).exists(_.tmdbId == id))) Verdict.Pass else Verdict.Fail
      case Expect.NotFilm(s, id) =>
        val ls = pick(s)
        if (ls.isEmpty) Verdict.NotApplicable else if (ls.exists(l => answer.filmOf(l).exists(_.tmdbId == id))) Verdict.Fail else Verdict.Pass
      case Expect.NotYear(s, year) =>
        val ls = pick(s)
        if (ls.isEmpty) Verdict.NotApplicable else if (ls.exists(l => answer.filmOf(l).exists(_.year.contains(year)))) Verdict.Fail else Verdict.Pass
      case Expect.Report(s) =>
        val ls = pick(s)
        if (ls.isEmpty) Verdict.NotApplicable
        else Verdict.Reported(ls.map(l => answer.filmOf(l).fold("no film")(f => s"${f.tmdbId}/${f.year.getOrElse("?")}")).distinct.mkString(", "))
    }
  }
}
