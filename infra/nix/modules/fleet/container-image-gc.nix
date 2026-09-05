# Collect the container images k3s has pulled and no longer runs.
#
# THE SIBLING OF ./nix-gc.nix, AND THE SAME ARGUMENT ONE LAYER UP. That module exists because a
# NixOS host accumulates its own deploy history at the rate of MERGES and nothing in nixpkgs
# collects it. This one exists because a k3s NODE accumulates the same history in a different
# store: every image Flux rolls out stays in containerd for ever, and nothing in k3s collects it
# either. Two stores, one deploy model, one failure.
#
# WHAT HAPPENED HERE, 2026-09-05: `FilesystemWillFillWithin7Days` fired for `/` on k3s-worker-1.
# The root filesystem was 84G of 150G used, of which 73G was
# /var/lib/rancher/k3s/agent/containerd -- 209 images, of which NINE were in use by a running
# container. The other 200 were movies-web and movies-worker builds, one per merge, going back to
# the day the cluster took its first workload. Measured over the seven days before the alert they
# arrived at roughly 5 GB per DAY, so the six-hour projection the alert makes was not being fooled
# by a one-off bulk write; it was reading a slope that had been there the whole time.
#
# WHY THE KUBELET'S OWN IMAGE GC IS NOT THE MECHANISM, since it already exists and is the first
# thing to reach for. It is a LEVEL, not a collection: it does nothing until the image filesystem
# crosses `--image-gc-high-threshold` (85% by default) and then evicts by last-use until it reaches
# the low threshold. On this node the image filesystem IS `/`, so that threshold is 85% of the root
# disk -- which is the exact point `FilesystemSpaceLow` in filesystem-capacity.rules fires. The
# kubelet's backstop and the alert it is meant to prevent land together, so as a mechanism it
# guarantees the page rather than avoiding it. Lowering the threshold instead is worse: the disk
# would then sit in a sawtooth between the two thresholds, and the trend alert's second clause
# (under 40% free) would be permanently satisfied, so every rising edge of the sawtooth would page.
#
# A COLLECTION KEEPS THE DISK AT THE SIZE OF WHAT IS RUNNING -- 13G rather than 84G on the node
# above -- which puts it so far below every threshold in filesystem-capacity.rules that none of
# them can be reached by images at all. That is the property worth having: not a smaller sawtooth,
# but a filesystem whose usage tracks the WORKLOAD instead of the deploy count.
#
# `crictl rmi --prune` DELETES IMAGES NO CONTAINER REFERENCES, which is a narrower thing than it
# sounds and is what makes this safe to run unattended. A running pod's image is referenced and
# survives; so does the image of a container that has exited but not been removed, which is what
# `kubectl logs --previous` reads. What it does NOT protect is an image pulled seconds ago that has
# no container yet -- a deploy landing inside the collection's window loses its layers and the
# kubelet pulls them again. That costs one re-pull and cannot lose anything, which is why the timer
# is daily and jittered rather than frequent and precise.
#
# AND IT PUBLISHES WHETHER IT RAN, for ./nix-gc.nix's reason exactly: a collector that silently
# stopped is indistinguishable from a node with room to spare, and the alert that says otherwise
# (`ContainerImageGarbageCollectionStale`, in files/monitoring/rules/k3s.rules) is a comparison
# against the timestamp below and cannot fire without it.
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.containerImageGc;
  textfileDir = config.fleet.observability.textfileDirectory;

  script = pkgs.writeShellApplication {
    name = "fleet-container-image-gc";
    runtimeInputs = [ pkgs.coreutils pkgs.jq config.services.k3s.package ];
    text = ''
      metrics_file="''${METRICS_FILE:?METRICS_FILE is not set}"

      # `k3s crictl` RATHER THAN A STANDALONE crictl, because the endpoint is the argument. k3s runs
      # its own containerd on a socket of its own (/run/k3s/containerd/containerd.sock) and a
      # crictl from nixpkgs defaults to the socket a standalone containerd would use -- which does
      # not exist here, so it fails with a connection error that reads like containerd being down.
      # The k3s binary's built-in crictl already knows where its own runtime is.
      before=$(k3s crictl images -q | wc -l)

      k3s crictl rmi --prune >/dev/null

      after=$(k3s crictl images -q | wc -l)

      # BYTES AS REPORTED BY THE RUNTIME, not `du` on the containerd directory. The two disagree --
      # containerd stores each layer once and shares it between images, so summing image sizes
      # double-counts what a `du` would show -- and this is the number that answers "what is the
      # image store costing", which is what somebody reading it after an alert wants. `du` on 70G
      # of snapshots also takes minutes; this is one API call.
      bytes=$(k3s crictl images -o json | jq '[.images[].size | tonumber] | add // 0')

      # WRITTEN LAST AND THROUGH A TEMPORARY, and only on the success path -- ./nix-gc.nix says why
      # at length: node_exporter does not skip a truncated textfile, it fails to parse it and drops
      # every series in the file, and a timestamp written before the work means "it started".
      tmp="''${metrics_file}.tmp"
      {
        echo "# HELP fleet_container_image_gc_last_success_timestamp_seconds When this host last completed a container image collection."
        echo "# TYPE fleet_container_image_gc_last_success_timestamp_seconds gauge"
        echo "fleet_container_image_gc_last_success_timestamp_seconds $(date +%s)"
        echo "# HELP fleet_container_image_gc_images Container images remaining after the collection."
        echo "# TYPE fleet_container_image_gc_images gauge"
        echo "fleet_container_image_gc_images ''${after}"
        echo "# HELP fleet_container_image_gc_images_deleted Container images removed by the last collection."
        echo "# TYPE fleet_container_image_gc_images_deleted gauge"
        echo "fleet_container_image_gc_images_deleted $(( before - after ))"
        echo "# HELP fleet_container_image_gc_bytes Apparent size of the images remaining, as the runtime reports them."
        echo "# TYPE fleet_container_image_gc_bytes gauge"
        echo "fleet_container_image_gc_bytes ''${bytes}"
      } > "$tmp"
      mv "$tmp" "$metrics_file"
      chmod 0644 "$metrics_file"
    '';
  };
in
{
  options.fleet.containerImageGc = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = config.services.k3s.enable;
      defaultText = "config.services.k3s.enable";
      description = ''
        DEFAULTED TO "THIS HOST RUNS k3s" RATHER THAN TO `true`, which is the one difference from
        ./nix-gc.nix's default and the reason this is a separate option at all. Every host has a
        Nix store; only the k3s hosts have an image store, and a collector that shells out to
        `k3s crictl` on mongo-1 would fail every night about a runtime that is not there.

        IT FOLLOWS THE ROLE, so it covers the server and the agent alike and needs no edit when a
        second worker appears -- which is the case that matters, because a new node starts empty
        and looks fine for exactly as long as it takes to accumulate a few hundred images.
      '';
    };

    dates = lib.mkOption {
      type = lib.types.str;
      default = "daily";
      description = ''
        systemd calendar expression for the collection.

        DAILY AGAINST ROUGHLY 5 GB A DAY OF NEW IMAGES on the busiest node, so a single missed run
        is a rounding error against a 150G disk rather than the beginning of a problem. More often
        would buy nothing and would widen the one window in which this can cost anything -- a
        deploy whose image is pulled but not yet running (see the header).
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    # THE ONE FORGIVENESS THIS MODULE ASKS OF ./auto-apply.nix, and it is worth writing the
    # sentence that option's documentation asks for: a nightly, idle-scheduled collection being
    # stopped and started at an arbitrary moment costs nothing at all. There is no session to
    # interrupt and no state to lose -- the next timer firing does the same work.
    #
    # WITHOUT IT EVERY FUTURE EDIT TO THIS COLLECTOR STRANDS THE WHOLE QUEUE. The gate is
    # default-deny and refuses the ENTIRE switch when any one unit would be disturbed, so a change
    # to this script would silently hold back every other merge staged onto these two hosts until
    # somebody applied by hand. That is a disproportionate price for bouncing a garbage collector.
    #
    # IT DOES NOT COVER THE FIRST APPLY. `nixos-auto-apply` reads the allow-list from the closure it
    # is RUNNING, not from the one it is considering, so the switch that introduces these units is
    # judged by a host that has never heard of them and is refused. That one is deployed by hand
    # (`colmena apply --on @k3s`); everything after it is covered.
    fleet.autoApply.restartableUnits = [
      "fleet-container-image-gc.service"
      "fleet-container-image-gc.timer"
    ];

    systemd.services.fleet-container-image-gc = {
      description = "Collect unused container images from the k3s image store";

      # AFTER k3s, AND ONLY WANTED BY THE TIMER. `crictl` talks to the runtime through a socket
      # k3s creates, so a collection racing a boot finds no socket and fails -- which would publish
      # nothing and read, correctly but uselessly, as a stopped collector.
      after = [ "k3s.service" ];
      requires = [ "k3s.service" ];

      serviceConfig = {
        Type = "oneshot";
        User = "root";
        # Generous for the same reason ./nix-gc.nix is: the FIRST run on a neglected node is the
        # expensive one -- 200 images and 70G of snapshots to unlink on the node that produced this
        # module -- and a collection killed part way leaves the store consistent but the metric
        # unwritten, which reads as "never succeeded".
        TimeoutStartSec = "1h";
        Environment = [ "METRICS_FILE=${textfileDir}/fleet-container-image-gc.prom" ];
        ExecStart = lib.getExe script;
        # Unlinking layers is I/O heavy and never urgent. Losing the race to anything this node
        # actually serves is the correct outcome, every time.
        IOSchedulingClass = "idle";
        CPUSchedulingPolicy = "idle";
      };
    };

    systemd.timers.fleet-container-image-gc = {
      description = "Collect unused container images from the k3s image store";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = cfg.dates;
        # JITTER, and an hour of it, for ./nix-gc.nix's reason -- both k3s hosts are in fsn1 and may
        # share a hypervisor -- plus one of this module's own: the window this can cost a re-pull in
        # is the window it overlaps a deploy, and a fixed minute would overlap the same one daily.
        RandomizedDelaySec = "1h";
        # PERSISTENT, so a node that missed its window collects at boot and a node that has never
        # collected does so shortly after taking this module. On k3s-worker-1 that first run IS the
        # repair rather than merely the schedule starting.
        Persistent = true;
      };
    };
  };
}
