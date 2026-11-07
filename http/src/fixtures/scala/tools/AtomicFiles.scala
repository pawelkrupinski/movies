package tools

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}

/** A file written whole or not at all: to a sibling temporary first, then renamed over its name, so a reader sees the
 *  old file or the new one and never one being written. The live-answer stores (`target/identity-live-gaps`,
 *  `target/agreement-live`) are shared by the captures `scripts/identity-capture.sh` runs side by side, and a plain
 *  write let one JVM read the half another had written; a gap-fill leg's order passes each record the answer they
 *  share to the same fixture file while the others replay it (`RecordingHttpFetch`). */
object AtomicFiles {
  def write(file: Path, bytes: Array[Byte]): Unit = {
    Files.createDirectories(file.getParent)
    val part = Files.createTempFile(file.getParent, file.getFileName.toString, ".part")
    try {
      Files.write(part, bytes)
      Files.move(part, file, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
      ()
    } finally { val _ = Files.deleteIfExists(part) }
  }

  def writeString(file: Path, text: String): Unit = write(file, text.getBytes(StandardCharsets.UTF_8))
}
