package tools

import java.nio.file.{Files, Path, StandardCopyOption}

/** A file written whole or not at all: to a sibling temporary first, then renamed over its name. The live-answer stores
 *  (`target/identity-live-gaps`, `target/agreement-live`) are shared by the captures `scripts/identity-capture.sh` runs
 *  side by side, and a plain write let one JVM read the half another had written. */
object AtomicFiles {
  def writeString(file: Path, text: String): Unit = {
    Files.createDirectories(file.getParent)
    val part = Files.createTempFile(file.getParent, file.getFileName.toString, ".part")
    Files.writeString(part, text)
    Files.move(part, file, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
  }
}
