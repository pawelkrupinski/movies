# What every host does when it runs short of memory: compressed swap, how eagerly to use it, and --
# on the k3s nodes -- how much of the machine the kubelet keeps back from pods.
#
# THE NUMBERS ARE IN ../../files/host-memory.json, NOT HERE, because NodeMemoryBudgetSpec (worker/
# src/test/scala/deploy/) has to read the same kubelet reserves: they shrink k3s-worker-1's
# allocatable memory, and that spec is the ledger that proves every kinowo pod plus the web tier's
# rolling-surge pod still fits on it. One file, read by both, so the two cannot disagree.
# infra/test/test_host_memory.sh asserts what each host's evaluated config renders from it.
#
# BEFORE THIS (2026-10-23) NO HOST HAD SWAP AND NO k3s NODE HAD A RESERVE. Two 4 GB cx23s (mongo-1,
# monitoring-1) and a 16 GB cx43 whose pods REQUEST ~80% of it and are LIMITED to ~87%, with every
# JVM pinned at -Xms=-Xmx. A host that ran out had exactly one answer, the kernel OOM killer, and
# nothing told the scheduler that caddy, the registry, vector and k3s itself live on that memory too.
#
# ------------------------------------------------------------------------------------------------
# zram, AND WHY LOW SWAPPINESS
# ------------------------------------------------------------------------------------------------
#
# zram is swap in compressed RAM: no disk, no reboot (systemd's zram-generator creates the device
# on activation), and zstd gets ~3:1 on idle anonymous pages. It is here as a CUSHION in front of
# the OOM killer, not as more memory, so swappiness is low: the kernel reaches for it under pressure
# rather than as a habit.
#
#   mongo-1    swappiness 1. MongoDB's production notes (checked 2026-10-23, the 9.0 manual; the 8.x
#              text is the same): "assign swap space ... and configure the kernel to only permit
#              swapping under high memory load", and set vm.swappiness to 1 on a host that also runs
#              other software -- this one runs the exporter, vector and the CI read account. 1 is
#              "swap only to avoid an OOM", which is the whole point: mongod paging is slow, mongod
#              SIGKILLed is an outage.
#   k3s nodes  swappiness 10. PODS CANNOT SWAP AT ALL: Kubernetes 1.35's default
#              `memorySwap.swapBehavior` is NoSwap (pinned below rather than inherited), which gives
#              every container cgroup `memory.swap.max = 0`, and k3s already runs the kubelet with
#              `failSwapOn: false`. So a fixed-heap JVM never has its heap paged out -- the GC pause
#              that would cause is the reason JVMs and swap have a bad name -- and the zram device
#              only absorbs what lives OUTSIDE the pods: Prometheus/Grafana/VictoriaLogs on
#              monitoring-1, caddy/registry/vector on k3s-worker-1, and k3s itself.
#
# 50% on the 4 GB hosts and 25% on the 16 GB one is the device's UNCOMPRESSED ceiling (2 GB and
# 4 GB); at ~3:1 a full device costs a sixth / a twelfth of RAM.
#
# ------------------------------------------------------------------------------------------------
# THE KUBELET RESERVES
# ------------------------------------------------------------------------------------------------
#
# allocatable = capacity - systemReserved - kubeReserved - evictionHard. The scheduler admits on
# allocatable, and the kubelet caps the kubepods cgroup at capacity - systemReserved - kubeReserved,
# so a pod storm meets that cgroup's limit before the host's daemons meet the OOM killer.
#
# EVICTION WAS NOT EVEN THE UPSTREAM 100Mi BEFORE THIS. k3s's own default kubelet config sets
# `evictionHard` to imagefs/nodefs only, and a map given at all REPLACES the kubelet's defaults --
# so memory.available had no threshold, and k3s-worker-1's allocatable equalled its capacity
# (15616Mi). The three keys are restated together below for the same reason: whichever way the
# kubelet merges its config drop-ins, the result is the same map.
#
# THROUGH extraKubeletConfig, NOT `--kubelet-arg=eviction-hard=...` in extraFlags: those land in
# the unit's ExecStart, where systemd reads `%` as a specifier and `nodefs.available<5%` stops being
# what was written. The nixpkgs module already hands k3s a kubelet config file
# (`--kubelet-arg=config=...`, for gracefulNodeShutdown); k3s installs it as a drop-in after its
# own defaults, so these keys override them.
{ config, lib, ... }:

let
  budgets = builtins.fromJSON (builtins.readFile ../../files/host-memory.json);
  host = config.networking.hostName;
  budget = budgets.${host} or null;
  isK3sNode = config.services.k3s.enable;
  mib = n: "${toString n}Mi";
in
{
  config = lib.mkMerge [
    {
      assertions = [
        {
          assertion = budget != null;
          message = ''
            ${host} has no entry in infra/nix/files/host-memory.json. Every host gets zram and a
            swappiness from that file; add one rather than letting this host run without swap.
          '';
        }
        {
          assertion = !isK3sNode || (budget != null && budget ? kubelet);
          message = ''
            ${host} runs k3s but host-memory.json declares no `kubelet` reserves for it, so the
            scheduler would hand pods the memory caddy, vector and k3s itself live on.
          '';
        }
      ];
    }

    (lib.mkIf (budget != null) {
      zramSwap = {
        enable = true;
        algorithm = "zstd";
        memoryPercent = budget.zramMemoryPercent;
      };
      boot.kernel.sysctl."vm.swappiness" = budget.swappiness;
    })

    (lib.mkIf (isK3sNode && budget != null && budget ? kubelet) {
      services.k3s.extraKubeletConfig = {
        systemReserved.memory = mib budget.kubelet.systemReservedMi;
        kubeReserved.memory = mib budget.kubelet.kubeReservedMi;
        evictionHard = {
          "memory.available" = mib budget.kubelet.evictionHardMi;
          # k3s's own defaults, restated because this map replaces them.
          "nodefs.available" = "5%";
          "imagefs.available" = "5%";
        };
        # The upstream default today, pinned: the reasoning above (no JVM heap in swap) depends on it.
        memorySwap.swapBehavior = "NoSwap";
      };
    })
  ];
}
