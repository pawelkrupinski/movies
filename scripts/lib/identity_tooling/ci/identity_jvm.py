"""THE JVM EVERY IDENTITY TOOL RUNS IN, for the Python tools (scripts/lib; stdlib only): identity-tools.sh's
IDENTITY_LOCALE and IDENTITY_SBT. English number formats ("0.97", never "0,97": the reports and the weights the JVM
writes are read back as text), and a `java` the tool starts itself headless (on macOS a JVM touching AWT without it
registers as a foreground app and steals focus).

  SBT + (heap, "-batch", command)    sbt in that locale (headless from .jvmopts)
  JAVA_OPTIONS                       the locale and headless options for a `java` started directly"""
from __future__ import annotations

LOCALE = ("-Duser.language=en", "-Duser.country=US")
SBT = ("sbt", *(f"-J{option}" for option in LOCALE))
JAVA_OPTIONS = ("-Djava.awt.headless=true", *LOCALE)
