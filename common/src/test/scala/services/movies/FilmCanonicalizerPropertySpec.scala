package services.movies

import models.MovieRecord
import org.scalacheck.Gen
import services.IdentityPropertySpec
import services.IdentityGenerators.genRows
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * `FilmCanonicalizer` partitions a row SET: whatever order the rows arrive in,
 * the same rows land together, every row lands exactly once, and a cluster
 * collapses to the same key.
 */
class FilmCanonicalizerPropertySpec extends IdentityPropertySpec {

  private type Row = (CacheKey, MovieRecord)

  private def asPartition(clusters: Seq[Seq[Row]]): Set[Set[CacheKey]] =
    clusters.map(_.map(_._1).toSet).toSet

  private def coversExactlyOnce(rows: Seq[Row], clusters: Seq[Seq[Row]]): Unit = {
    val placed = clusters.flatten.map(_._1)
    placed.size  shouldBe rows.size
    placed.toSet shouldBe rows.map(_._1).toSet
  }

  "FilmCanonicalizer.groupByFilm" should "partition the same rows the same way in any order" in {
    forAll(withPermutation(genRows)) { case (rows, permuted) =>
      asPartition(FilmCanonicalizer.groupByFilm(permuted, titleNormalizer)) shouldBe
        asPartition(FilmCanonicalizer.groupByFilm(rows, titleNormalizer))
    }
  }

  it should "place every row exactly once" in {
    forAll(genRows) { rows =>
      coversExactlyOnce(rows, FilmCanonicalizer.groupByFilm(rows, titleNormalizer))
    }
  }

  "FilmCanonicalizer.clusterByFilm" should "cluster the same rows the same way in any order" in {
    forAll(withPermutation(genRows)) { case (rows, permuted) =>
      asPartition(FilmCanonicalizer.clusterByFilm(permuted, titleNormalizer)) shouldBe
        asPartition(FilmCanonicalizer.clusterByFilm(rows, titleNormalizer))
    }
  }

  it should "place every row exactly once" in {
    forAll(genRows) { rows =>
      coversExactlyOnce(rows, FilmCanonicalizer.clusterByFilm(rows, titleNormalizer))
    }
  }

  "FilmCanonicalizer.canonical" should "collapse a cluster to the same key and record in any order" in {
    forAll(genRows, Gen.long) { (rows, seed) =>
      for {
        component <- FilmCanonicalizer.groupByFilm(rows, titleNormalizer)
        cluster   <- FilmCanonicalizer.clusterByFilm(component, titleNormalizer)
      } {
        val (key, merged)                 = FilmCanonicalizer.canonical(cluster, titleNormalizer)
        val (permutedKey, permutedMerged) = FilmCanonicalizer.canonical(permute(seed, cluster), titleNormalizer)
        permutedKey    shouldBe key
        permutedMerged shouldBe merged
      }
    }
  }
}
