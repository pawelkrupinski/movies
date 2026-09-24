# Keep the kinowo pods' JVM heap dumps on this node, and keep them bounded.
#
# WHERE THEY ARE: every web and worker pod mounts `/var/lib/kinowo/heapdumps/<app>-<country>/`
# (a hostPath with a per-app-country subPath, in the movies-gitops base manifests) at
# /data/heapdumps, and each JVM start writes a uniquely named dump there on a heap OOM. It is a
# hostPath because the dump has to OUTLIVE THE POD: web-pl heap-OOMed on 2026-09-23 and its dump
# was on an emptyDir, gone the moment the pod was replaced. A PVC per app-country would do the
# same for ten times the objects, and would scatter the dumps across ten pvc-<uid> directories
# that no single budget or `ls` could see at once. Dumps are forensics: fine to lose with the
# host, which is the standing assumption for this node (hosts/k3s-worker-1/disko.nix).
#
# WHY A NODE TIMER AS WELL AS THE CONTAINER'S BOOT PRUNE. The image runs the same script before
# each JVM starts, but only over its OWN directory and only when it restarts. A pod that never
# restarts never prunes, and no container can see the total across all ten. This timer runs the
# whole budget -- per directory AND across them -- whatever the pods are doing, compresses what
# has settled, and publishes what is kept.
#
# THE SCRIPT IS ../../files/heap-dumps.sh, THE SAME FILE THE IMAGE SHIPS (build.sbt copies it into
# both dists). The numbers, why they are what they are, and the in-progress-dump rule are all
# documented there; infra/test/test_heap_dumps.sh pins them.
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.heapDumps;
  textfileDir = config.fleet.observability.textfileDirectory;

  heapDumps = pkgs.writeShellScript "kinowo-heap-dumps" (builtins.readFile ../../files/heap-dumps.sh);

  run = pkgs.writeShellScript "kinowo-heap-dumps-run" ''
    set -euo pipefail
    ${heapDumps} compress ${cfg.root}
    ${heapDumps} prune-all ${cfg.root}
    # WRITTEN LAST AND THROUGH A TEMPORARY, for ./container-image-gc.nix's reason: node_exporter
    # drops every series in a textfile it cannot parse, so a half-written one would read as "no
    # dumps" at exactly the moment one is being written.
    tmp="${textfileDir}/kinowo-heap-dumps.prom.tmp"
    {
      ${heapDumps} report ${cfg.root}
      echo "# HELP kinowo_heapdumps_prune_last_success_timestamp_seconds When this node last completed a heap-dump prune."
      echo "# TYPE kinowo_heapdumps_prune_last_success_timestamp_seconds gauge"
      echo "kinowo_heapdumps_prune_last_success_timestamp_seconds $(date +%s)"
    } > "$tmp"
    chmod 0644 "$tmp"
    mv "$tmp" "${textfileDir}/kinowo-heap-dumps.prom"
  '';
in
{
  options.fleet.heapDumps = {
    enable = lib.mkEnableOption "the kinowo heap-dump directory, its budget timer and its metrics";

    root = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/kinowo/heapdumps";
      description = ''
        The hostPath the web and worker pods mount their dump directories from. MUST match the
        `hostPath.path` in movies-gitops web/base/all.yaml and worker/base/all.yaml.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    # Created here as well as by the kubelet (`DirectoryOrCreate`), so the timer has something to
    # report on before the first pod mounts it. Either way it is root:root 0755, and the JVMs run
    # as root (no USER in the Dockerfile, pinned by WorkerDurableDiagnosticsConfigSpec), so the
    # order the two arrive in does not matter.
    systemd.tmpfiles.rules = [ "d ${cfg.root} 0755 root root -" ];

    # See ./container-image-gc.nix: a oneshot budget pass bounced by an unattended switch costs
    # nothing, and without this every later edit to it would strand the host's whole queue.
    # The FIRST apply, which introduces the units, is refused regardless and is done by hand,
    # switching to the closure CI already staged (docs/heap-dumps.md has the command).
    fleet.autoApply.restartableUnits = [
      "kinowo-heap-dumps.service"
      "kinowo-heap-dumps.timer"
    ];

    systemd.services.kinowo-heap-dumps = {
      description = "Compress, prune and report the kinowo pods' JVM heap dumps";
      path = [ pkgs.coreutils pkgs.gzip ];
      serviceConfig = {
        Type = "oneshot";
        User = "root";
        # gzip -1 of a 1.4 GB dump is ~15s of one core; the first run after a crash loop may have
        # several. Generous, because a pass killed part way leaves the metric unwritten.
        TimeoutStartSec = "30min";
        ExecStart = run;
        # Never urgent. Losing every race to the pods this node serves is the right outcome.
        IOSchedulingClass = "idle";
        CPUSchedulingPolicy = "idle";
      };
    };

    systemd.timers.kinowo-heap-dumps = {
      description = "Compress, prune and report the kinowo pods' JVM heap dumps";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        # Every 15 minutes: a crash-looping pod writes at most one dump per restart, and the
        # container's own boot prune already holds each directory between runs. This is the
        # backstop for the total and for pods that never restart.
        OnCalendar = "*:0/15";
        RandomizedDelaySec = "2min";
        Persistent = true;
      };
    };
  };
}
