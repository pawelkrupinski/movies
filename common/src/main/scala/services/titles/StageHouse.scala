package services.titles

/** An opera or ballet HOUSE as a title bills it: the word table's house words ([[TitleWords.Concept.StageHouse]]), the
 *  houses billed under more than one name (`stage-houses.tsv`, hand-kept: Covent Garden's Royal Opera House, Royal Ballet
 *  and Opera since 2024, billed under both names and both initials), and every name's initials, derived from it ("RBO",
 *  "ROH"; never listed). Read by the identity model (a relay's house, its banner) and by a venue's slots (two seasons of
 *  one staging are two productions; a programme's two seasons are not), so both read one house. */
object StageHouse {
  val Resource = "stage-houses.tsv"

  /** Lines `<name>|<name>…`, one house each: every name's words. */
  def parse(lines: Iterator[String]): Seq[Seq[Seq[String]]] =
    lines.map(_.trim).filter(line => line.nonEmpty && !line.startsWith("#"))
      .map(_.split('|').toSeq.map(name => TitleContainment.tokens(name).toSeq).filter(_.nonEmpty)).filter(_.sizeIs >= 2).toSeq

  /** Every house's names on the classpath. */
  lazy val houses: Seq[Seq[Seq[String]]] = tools.ClasspathTable.read(Resource)(parse)


  val SeasonBannersResource = "cinema-season-banners.tsv"

  /** A CINEMA SEASON's banner ([[seasonBanners]]): its words, and the countries (ISO-3166-1, as a listing's venue is
   *  placed: `GB`, `US`) whose venues bill it so. */
  final case class SeasonBanner(words: Seq[String], countries: Set[String])

  /** `cinema-season-banners.tsv`'s rows (`banner<TAB>where<TAB>why`, a header and `#` comments aside): each banner's
   *  words, and the countries `where` names by their codes (`uk us`, [[models.Country.code]]). A row naming no country,
   *  or one no deployment serves, fails loudly: a banner read everywhere would let a house's own season ("RBO Kino
   *  Saison") carry another house's records. */
  def parseSeasonBanners(lines: Iterator[String]): Seq[SeasonBanner] =
    lines.filterNot(line => line.trim.isEmpty || line.startsWith("#") || line.startsWith("banner\t")).map { line =>
      val cells     = line.split('\t')
      val banner    = cells.head
      val codes     = cells.lift(1).toSeq.flatMap(_.split("[\\s,]+")).map(_.trim.toLowerCase(java.util.Locale.ROOT)).filter(_.nonEmpty)
      val countries = codes.map(code => models.Country.all.find(_.code == code).map(_.language.getCountry)
        .getOrElse(throw new IllegalArgumentException(s"$SeasonBannersResource: banner '$banner' names '$code', no country's code")))
      require(countries.nonEmpty, s"$SeasonBannersResource: banner '$banner' names no country in its `where` column")
      SeasonBanner(bannerWords(TitleContainment.tokens(banner).toSeq), countries.toSet)
    }.filter(_.words.nonEmpty).toSeq
  /** A banner's words as [[isSeasonBanner]] matches them: its numbers and [[UninitialledWords]] out, split initials joined. */
  private def bannerWords(words: Seq[String]): Seq[String] =
    initialsJoined(words).filterNot(word => word.forall(_.isDigit) || UninitialledWords(word))

  /** A CINEMA SEASON's banners over stage relays (`cinema-season-banners.tsv`): names under which venues bill more than
   *  one house's productions — the Royal Ballet & Opera's UK cinema season, "RBO Cinema Season 2026-27: Macbeth" the
   *  Met's. A relay under one may be another house's record of its season; under any other house's name, never. */
  lazy val seasonBanners: Seq[SeasonBanner] = tools.ClasspathTable.read(SeasonBannersResource)(parseSeasonBanners)

  /** Does a house fragment's words (a relay's banner, numbers out), billed at a venue in `country` (ISO-3166-1), carry a
   *  cinema's season banner of that country ([[seasonBanners]]) as a run of its words? A banner is a season only where
   *  its row says venues bill it so: DE "RBO Kino Saison" and PL "Royal Ballet and Opera Sezon Kinowy" bill the house's
   *  own relays alone. */
  def isSeasonBanner(house: Seq[String], country: Option[String]): Boolean =
    country.exists(here => seasonBanners.exists(banner => banner.countries(here) && bannerWords(house).containsSlice(banner.words)))

  /** Words a house's initials skip ("RBO" of "The Royal Ballet and Opera"). */
  val UninitialledWords: Set[String] = Set("the", "and", "und", "of")

  /** `name`'s initials, its numbers and [[UninitialledWords]] skipped: "Royal Ballet & Opera" → "rbo". */
  def initialsOf(name: Seq[String]): String =
    name.filterNot(word => word.forall(_.isDigit) || UninitialledWords(word)).flatMap(_.headOption).mkString

  /** The initials of every house name of three initialled words or more ("rbo", "roh"), derived from the names, never
   *  listed: two letters ("MO" of "Metropolitan Opera") are any title's word. */
  def initialsOfHouses(houses: Seq[Seq[Seq[String]]]): Set[String] =
    houses.flatten.map(initialsOf).filter(_.length >= 3).toSet

  /** [[initialsOfHouses]] of the classpath's houses. */
  lazy val initials: Set[String] = initialsOfHouses(houses)

  /** Each house's names, its names' [[initials]] among them: "ROH" and "Royal Ballet & Opera" name one house. */
  lazy val namedWithInitials: Seq[Seq[Seq[String]]] =
    houses.map(names => names ++ names.map(initialsOf).filter(initials).distinct.map(Seq(_)))

  /** Words naming a house or its relay, as their own tokens ("Dreigroschenoper" or "Operation" is no house): the table's
   *  and the houses' initials. */
  lazy val Words: Set[String] = TitleWords.tokens(TitleWords.Concept.StageHouse) ++ initials

  /** `name` with each run of a house's INITIALS that punctuation split into words joined back into one: DE "RB+O" folds to
   *  "rb" and "o", the initials "rbo" of Royal Ballet & Opera. A run is two words or more of two letters at most, one of
   *  them a single letter — "de la" are words, never initials. */
  def initialsJoined(name: Seq[String]): Seq[String] = {
    def short(word: String) = word.length <= 2 && word.forall(_.isLetter)
    def isInitials(run: Seq[String]) = run.sizeIs >= 2 && run.exists(_.length == 1)
    name.foldLeft(Vector.empty[Vector[String]]) { (runs, word) =>
      runs.lastOption match {
        case Some(run) if short(word) && run.forall(short) => runs.init :+ (run :+ word)
        case _                                             => runs :+ Vector(word)
      }
    }.flatMap(run => if (run.forall(short) && isInitials(run)) Seq(run.mkString) else run)
  }

  /** Do `words` hold a house's initials that punctuation split into words ("RB&O" read as "rb", "o")? */
  def splitInitials(words: Seq[String]): Boolean =
    initialsJoined(words).exists(word => initials(word) && !words.contains(word))

  /** A house's initials as a word of its own: no letter or digit beside them, nor a hyphened word ("Tosca (ROH)", never
   *  "Jin-Roh"). */
  private lazy val StandaloneInitials = Option.when(initials.nonEmpty)(
    initials.toSeq.sorted.map(java.util.regex.Pattern.quote).mkString("""(?iu)(?<![\p{L}\d]-?)(?:""", "|", """)(?!-?[\p{L}\d])""").r)

  /** Does one of `titles` name an opera or ballet house ([[Words]])? A house's initials only standing alone, or split by
   *  punctuation ([[splitInitials]]: UK Penrith's "RB&O 26/27: Carmen", DE "Carmen (RB+O 2026/27)"): DE "Jin-Roh - Die
   *  Wolfsbrigade" is Okiura's anime, no Royal Opera House relay, whatever its hyphen splits off. */
  def billed(titles: Seq[String]): Boolean = titles.exists { title =>
    val words = TitleContainment.tokens(title)
    words.exists(word => Words(word) && !initials(word)) || StandaloneInitials.exists(_.findFirstIn(title).isDefined) || splitInitials(words)
  }
}
