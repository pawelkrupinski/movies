package services.sharecards

import java.nio.file.Path

import scala.util.Try

import tools.{CityCardRenderer, OgCardRenderer, PosterDecode}

/** A city card as the image written to disk: `heading` and `brand` over the posters at `posters`, each decoded to a tile
 *  (one that cannot be decoded left out). */
object CityCardDrawing {
  def png(heading: String, brand: CityCardRenderer.Brand, posters: Seq[Path]): Array[Byte] = {
    val images = posters.flatMap(poster =>
      Try(PosterDecode.fromFile(poster.toFile, CityCardRenderer.TileWidth, CityCardRenderer.TileHeight)).toOption.flatten)
    OgCardRenderer.encodeCard(CityCardRenderer.render(heading, brand, images))
  }
}
