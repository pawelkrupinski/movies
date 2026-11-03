package services.identity

import java.nio.file.Path
import scala.util.hashing.MurmurHash3

/**
 * THE REAL-FAMILY SPECS IN PARTS: [[TitleRelationSpec]], [[ReadSetClosureSpec]] and [[PopulationMonotonicitySpec]] each
 * check every committed slice, minutes of resolves apiece (400-690 s in the landing check), so one class was the floor no
 * identity shard went below (scripts/ci/shard_tests.py splits by class). Each is [[Count]] classes (`<Spec>Part<k>Spec`),
 * part k checking the slices whose name hashes to k: a slice keeps its part as others come and go, every slice is in
 * exactly one part, and part 1 also holds the spec's checks of no single slice. [[All]] is every slice and every check in
 * one suite (what the parts together are: RealFamilyPartsSpec proves it, case for case).
 */
object SliceParts {

  /** Into how many parts each real-family spec splits the slices. */
  val Count = 4

  /** Not a part: every slice and every check. */
  val All = 0

  /** The part (1 to [[Count]]) checking `dir`'s slice: a hash of its name under the fixtures, stable across machines. */
  def partOf(dir: Path): Int = math.floorMod(MurmurHash3.stringHash(PopulationMonotonicitySpec.relative(dir)), Count) + 1

  /** The slices of `dirs` that `part` checks. */
  def of(part: Int, dirs: Seq[Path]): Seq[Path] = {
    require(part == All || (1 to Count).contains(part), s"part $part: not 1 to $Count, nor All")
    if (part == All) dirs else dirs.filter(partOf(_) == part)
  }

  /** Whether `part` holds the spec's checks of no single slice. */
  def holdsWholeChecks(part: Int): Boolean = part == All || part == 1

  /** The slices of `dirs` in the countries a [[tools.CountryScope]] test may check: the rest cancel before they read
   *  a result, so they are never resolved. */
  def inScope(dirs: Seq[Path], countries: Set[String]): Seq[Path] =
    dirs.filter(dir => countries(PopulationMonotonicitySpec.countryOf(dir)))
}
