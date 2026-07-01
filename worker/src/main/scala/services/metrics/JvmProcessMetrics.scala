package services.metrics

import io.prometheus.metrics.instrumentation.jvm.JvmMetrics
import io.prometheus.metrics.model.registry.PrometheusRegistry

/** Registers the Prometheus client's standard JVM + process resource collectors
 *  into the worker's shared metrics registry — the RESOURCE signals, not any
 *  business data. One `JvmMetrics.builder().register(...)` call installs:
 *
 *    - `process_cpu_seconds_total` — total CPU seconds the worker process has
 *      burned across ALL threads. This is the series the CPU-starvation
 *      investigation had to do without: it's process-wide, so unlike per-task
 *      worker-thread timing it also captures the `ParallelDetailFetch` fan-out
 *      threads where cinema scrapes do their HTML/JSON parsing. Rate it in
 *      Grafana and overlay against Fly's CPU-credit balance/throttle to see how
 *      much CPU the worker actually draws while credit-starved.
 *    - `process_resident_memory_bytes` / `process_start_time_seconds` /
 *      `process_open_fds` — RSS (vs the 1 GB cgroup), uptime (so a restart is
 *      visible as the counter reset it is), and fd pressure.
 *    - `jvm_memory_used_bytes` (heap + non-heap), `jvm_gc_*` pause counts/seconds,
 *      `jvm_threads_*`, class-loading and buffer-pool gauges — the JVM-internal
 *      view the Fly-level VM metrics can't see (e.g. heap headroom vs the tuned
 *      320 MB, GC pauses under load).
 *
 *  Deliberately NOT added: a per-task-type CPU counter. Cinema scrapes fan their
 *  parse work onto `ParallelDetailFetch`'s own executor, so `ThreadMXBean` on the
 *  task's worker thread would undercount the dominant task type and mislead — the
 *  process-wide series above is the honest CPU signal instead. */
object JvmProcessMetrics {

  /** Install the JVM + process collectors on `registry`. Registering the same
   *  collector names twice throws, so call this exactly once per registry (the
   *  worker does it when the shared registry is built). */
  def register(registry: PrometheusRegistry): Unit =
    JvmMetrics.builder().register(registry)
}
