package modules.wiring

import services.identity.TakeJudgements

import java.nio.charset.StandardCharsets
import scala.io.Source
import scala.util.Using

/** The hand judgements of takes the worker is built with (`/identity-judgements.tsv`, generated from
 *  `test/resources/fixtures/identity-judgements.tsv` by build.sbt), read once at the composition root and handed to
 *  identity code as a parameter ([[TakeJudgements.Index]]): identity code reads no classpath resource of its own. */
object BundledJudgements {
  val Resource = "/identity-judgements.tsv"

  /** Every bundled judgement; none when the worker was built without them. */
  lazy val all: Seq[TakeJudgements.Judgement] =
    Option(getClass.getResourceAsStream(Resource)).fold(Seq.empty[TakeJudgements.Judgement])(stream =>
      Using.resource(Source.fromInputStream(stream, StandardCharsets.UTF_8.name()))(source => TakeJudgements.parse(source.getLines())))

  /** `country`'s bundled judgements. */
  def index(country: String): TakeJudgements.Index = new TakeJudgements.Index(all.filter(_.country == country))
}
