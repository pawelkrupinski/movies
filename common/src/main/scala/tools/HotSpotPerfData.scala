package tools

import java.nio.{ByteOrder, MappedByteBuffer}
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardOpenOption}

/** This JVM's HotSpot perf data — the shared-memory counters `jstat` reads (`hsperfdata_<user>/<pid>`),
 *  mapped once and read live. Some counters exist nowhere else: how many classes came from the AOT
 *  cache (`java.cls.sharedLoadedClasses`) has no JMX bean.
 *
 *  The layout is HotSpot's `PerfDataPrologue` followed by `PerfDataEntry` records; only scalar long
 *  counters are read. */
final class HotSpotPerfData private (buffer: MappedByteBuffer) {
  import HotSpotPerfData.*

  private val longs: Map[String, Int] = {
    val entries = buffer.getInt(EntryOffsetAt)
    val count   = buffer.getInt(EntryCountAt)
    Iterator.iterate(entries)(at => at + buffer.getInt(at)).take(count).flatMap { at =>
      val isScalarLong = buffer.get(at + DataTypeAt) == 'J'.toByte && buffer.getInt(at + VectorLengthAt) == 0
      Option.when(isScalarLong)(nameAt(at + buffer.getInt(at + NameOffsetAt)) -> (at + buffer.getInt(at + DataOffsetAt)))
    }.toMap
  }

  /** The counter's current value, or None when this JVM has no such scalar counter. */
  def long(name: String): Option[Long] = longs.get(name).map(buffer.getLong)

  private def nameAt(at: Int): String = {
    val end = Iterator.from(at).find(buffer.get(_) == 0).get
    val bytes = new Array[Byte](end - at)
    buffer.get(at, bytes)
    new String(bytes, StandardCharsets.US_ASCII)
  }
}

object HotSpotPerfData {
  private val Magic          = 0xcafec0c0
  private val ByteOrderAt    = 4
  private val EntryOffsetAt  = 24
  private val EntryCountAt   = 28
  private val NameOffsetAt   = 4
  private val VectorLengthAt = 8
  private val DataTypeAt     = 12
  private val DataOffsetAt   = 16

  /** This JVM's perf data, where HotSpot writes it: `/tmp` on Linux, the per-user temp directory on
   *  macOS. None when the JVM keeps none (`-XX:-UsePerfData`) or the file is unreadable. */
  def own(): Option[HotSpotPerfData] = {
    val process = ProcessHandle.current()
    val user    = process.info().user().orElse("")
    val tempDir = scala.util.Try { val probe = Files.createTempFile("hsperf", ""); try probe.getParent finally Files.delete(probe) }.toOption
    (Paths.get("/tmp") +: tempDir.toSeq).distinct
      .map(_.resolve(s"hsperfdata_$user").resolve(process.pid.toString))
      .find(Files.isReadable(_))
      .flatMap(open)
  }

  private def open(file: Path): Option[HotSpotPerfData] = scala.util.Try {
    val channel = FileChannel.open(file, StandardOpenOption.READ)
    val buffer  = try channel.map(FileChannel.MapMode.READ_ONLY, 0, channel.size()) finally channel.close()
    buffer.order(ByteOrder.BIG_ENDIAN)
    require(buffer.getInt(0) == Magic, s"$file is not HotSpot perf data")
    buffer.order(if (buffer.get(ByteOrderAt) == 0) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
    new HotSpotPerfData(buffer)
  }.toOption
}
