package services.sharecards

import java.nio.file.{Files, Path}

import scala.util.{Try, Using}

/** What a JPEG's header says about the decode it would cost, read before any pixel is. */
final case class JpegHeader(progressive: Boolean, width: Int, height: Int, sampling: Seq[(Int, Int)]) {
  /** The DCT coefficients a PROGRESSIVE decode holds in memory until its last scan: every block of
   *  every component (chroma planes scaled by their subsampling), 64 coefficients of 2 bytes each. */
  def coefficientBytes: Long = {
    val (hMax, vMax) = (sampling.map(_._1).max, sampling.map(_._2).max)
    sampling.map { case (h, v) =>
      val w = math.ceil(width.toDouble * h / hMax).toLong
      val t = math.ceil(height.toDouble * v / vMax).toLong
      ((w + 7) / 8) * ((t + 7) / 8) * 64 * 2
    }.sum
  }
}

object JpegHeader {
  /** The frame header of the JPEG at `file`, or None for anything that isn't a readable JPEG. */
  def read(file: Path): Option[JpegHeader] =
    Try(Using.resource(new java.io.DataInputStream(new java.io.BufferedInputStream(Files.newInputStream(file)))) { in =>
      if (in.readUnsignedShort() != 0xffd8) None else firstFrame(in)
    }).toOption.flatten

  /** Walk the segments to the first frame header: None at the scan or the image's end before any. */
  @scala.annotation.tailrec
  private def firstFrame(in: java.io.DataInputStream): Option[JpegHeader] = {
    val code = nextMarker(in)
    if (code == 0xd9 || code == 0xda) None                                  // EOI / SOS before any frame
    else if (code >= 0xd0 && code <= 0xd7 || code == 0x01) firstFrame(in)   // standalone markers
    else {
      val length = in.readUnsignedShort()
      if (startsFrame(code)) Some(frame(in, code))
      else { in.skipNBytes((length - 2).toLong); firstFrame(in) }
    }
  }

  /** The next marker's code, past any fill bytes. */
  private def nextMarker(in: java.io.DataInputStream): Int = {
    var marker = in.readUnsignedByte()
    while (marker != 0xff) marker = in.readUnsignedByte()
    var code = in.readUnsignedByte()
    while (code == 0xff) code = in.readUnsignedByte()
    code
  }

  /** A start-of-frame marker (SOF0..SOF15, less DHT, JPG and DAC). */
  private def startsFrame(code: Int): Boolean = code >= 0xc0 && code <= 0xcf && code != 0xc4 && code != 0xc8 && code != 0xcc

  /** The frame header after a start-of-frame marker `code`. */
  private def frame(in: java.io.DataInputStream, code: Int): JpegHeader = {
    in.readUnsignedByte()                                                    // precision
    val height = in.readUnsignedShort(); val width = in.readUnsignedShort()
    val sampling = (1 to in.readUnsignedByte()).map { _ =>
      in.readUnsignedByte(); val hv = in.readUnsignedByte(); in.readUnsignedByte()
      ((hv >> 4) max 1, (hv & 0x0f) max 1)
    }
    JpegHeader(progressive = Set(0xc2, 0xc6, 0xca, 0xce)(code), width, height, sampling)
  }
}
