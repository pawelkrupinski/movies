#!/usr/bin/env bash
# Sourced by a profiled takes or drift row: every JVM the step starts from here on (sbt, each resolve) records a JFR
# CPU profile into $RUNNER_TEMP/jfr/<pid>.jfr, which the row uploads as convergence-profile-<code>-<phase>. Through
# JDK_JAVA_OPTIONS, which job-telemetry already sets (the GC log), so the flag is appended, never replacing it.
mkdir -p "${RUNNER_TEMP:-/tmp}/jfr"
export JDK_JAVA_OPTIONS="${JDK_JAVA_OPTIONS:-} -XX:StartFlightRecording=filename=${RUNNER_TEMP:-/tmp}/jfr/%p.jfr,settings=profile,dumponexit=true"
