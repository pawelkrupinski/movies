# A k3s SERVER -- the Kubernetes control plane, on monitoring-1, beside Prometheus and Grafana.
#
# PORTED FROM bitcashier's nix/modules/roles/k3s-server.nix. The reasoning that carried across
# unchanged, because it is about k3s rather than about that fleet: k3s is a single binary behind a
# config file and a set of flags, which is what a NixOS module expresses completely -- the
# alternative (kubeadm) keeps its state in a directory no configuration describes, so a rebuilt
# master is a restore rather than a rebuild.
#
# WHAT IS DIFFERENT HERE, and each difference is a decision rather than a simplification:
#
#   * NO CONSUL, so no service registration and no `--resolv-conf` pointing at a fleet resolver.
#     bitcashier's version points CoreDNS at each node's own unbound because `.service.consul` has
#     to resolve from inside a pod. This fleet has no such namespace. SEE THE HAZARD NOTE BELOW
#     ANYWAY -- the absence of that flag is safe here only for as long as this host's
#     /etc/resolv.conf is a real resolver rather than a systemd-resolved stub.
#   * NO host-gw FLANNEL BACKEND, and this is the one worth reading twice. bitcashier switches to
#     host-gw because it declares pod-network routes in the Hetzner network, which makes a pod
#     address reachable from its HAProxy edges. THIS FLEET DECLARES NO SUCH ROUTES. host-gw without
#     them is a cluster whose pods cannot talk across nodes -- and it fails silently, one direction
#     at a time. So the default VXLAN overlay stands, UDP 8472 is opened for it, and the day
#     something outside the cluster needs to reach a pod directly, BOTH halves change together.
#   * IT SHARES A BOX WITH THE FLEET'S ALARM. See the CPUWeight note at the bottom.
#
# THE CLUSTER CARRIES NOTHING TODAY. That is deliberate: stand it up, watch it, break it on
# purpose, and only then let anything depend on it. Nothing in the kinowo stack is scheduled here.
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.k3sServer;
in
{
  options.fleet.k3sServer = {
    enable = lib.mkEnableOption "a k3s server (Kubernetes control plane)";

    clusterInit = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Whether this node BOOTSTRAPS the cluster's embedded etcd rather than joining one.

        TRUE HERE BECAUSE THERE IS EXACTLY ONE SERVER, which also makes the usual trap harmless
        today and worth stating for the day it is not: `clusterInit` is a property of a MOMENT, not
        of a machine. On a multi-server cluster, leaving it true on a node that is later rebuilt
        asks it to initialise a SECOND cluster with the same token, and the result is not an error
        -- it is an empty control plane that looks healthy. The assertion below refuses it
        alongside `serverAddr` for that reason.
      '';
    };

    serverAddr = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = ''
        The server this node joins, for any server that is not the one bootstrapping, as
        `https://<address>:6443`.

        NO EXAMPLE ADDRESS IS WRITTEN HERE. A cloud provider eventually hands a decommissioned
        machine's address to the next one built, so an address in a manifest -- even an
        illustrative one -- is a thing somebody copies after it has started pointing somewhere
        else.

        AN ADDRESS, NOT A NAME: this is read on a freshly built node before any of the fleet's own
        DNS is necessarily answering, and a join that fails for want of a resolver is diagnosed as
        a token or a firewall problem for far longer than it deserves.
      '';
    };

    tokenFile = lib.mkOption {
      type = lib.types.str;
      default = config.sops.secrets."k3s/cluster-token".path;
      defaultText = ''config.sops.secrets."k3s/cluster-token".path'';
      description = ''
        Path to the shared cluster token, from sops-nix.

        A PATH, NEVER A VALUE. k3s reads it at start; putting the token in the unit's environment
        would publish it to `systemctl show` and to the journal, and it is the credential that lets
        a machine join the control plane.
      '';
    };

    schedulable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Whether ordinary workload may be scheduled onto this control-plane node.

        FALSE, AND ON THIS BOX THAT IS NOT THE USUAL "KEEP ETCD AWAY FROM WORKLOAD" ARGUMENT. This
        machine is the fleet's MONITORING host: Prometheus, Alertmanager and Grafana live here, and
        a pod scheduled beside them competes for the CPU and the disk that the alarm needs in
        exactly the conditions the alarm exists for. k3s-worker-1 is a cx43 with nothing else on
        it; that is where workload goes.

        Setting it false applies k3s's control-plane taint. Note that this does NOT mean nothing
        runs here -- CoreDNS and the other control-plane pods still do, which is why the DNS hazard
        note in the header applies to this host and not only to the agents.
      '';
    };

    clusterCidr = lib.mkOption {
      type = lib.types.str;
      default = "10.42.0.0/16";
      description = ''
        The pod network. k3s's own default, kept deliberately.

        IT MUST NOT COLLIDE WITH THE PRIVATE NETWORK (10.20.0.0/24) and it does not. A collision
        here is not a failure to start: it is a node that runs and cannot reach part of the fleet,
        intermittently, depending on which address a pod happens to be given -- which is diagnosed
        as a flaky network for a long time before anyone suspects an overlap.
      '';
    };

    serviceCidr = lib.mkOption {
      type = lib.types.str;
      default = "10.43.0.0/16";
      description = "The service network. k3s's default, kept for the same reason as clusterCidr.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = !(cfg.clusterInit && cfg.serverAddr != "");
        message = ''
          fleet.k3sServer: clusterInit and serverAddr are mutually exclusive. A node either
          bootstraps a cluster or joins one; asking for both produces a second empty control plane
          that looks healthy rather than an error.
        '';
      }
      {
        assertion = cfg.clusterInit || cfg.serverAddr != "";
        message = ''
          fleet.k3sServer: set either clusterInit (on exactly one node, for its first boot) or
          serverAddr. Without either, k3s starts a cluster of one and nothing says so.
        '';
      }
    ];

    sops.secrets."k3s/cluster-token" = { mode = "0400"; };

    services.k3s = {
      enable = true;
      role = "server";
      inherit (cfg) clusterInit tokenFile serverAddr;

      # WHAT IS TURNED OFF, AND WHY EACH ONE. k3s ships a small distribution of its own; every
      # piece of it that overlaps something this fleet already has is a second answer to a question
      # that already has one, running on a 4 GB box that is also the fleet's alarm.
      #
      #   traefik        -- nothing is fronted by this cluster yet, and an ingress controller that
      #                     nothing routes to is a listener on 80/443 of the monitoring host. When
      #                     something does need ingress, that is a decision to make deliberately
      #                     rather than a default to inherit.
      #   servicelb      -- klipper-lb allocates HOST ports for LoadBalancer services, on a host
      #                     whose ports are the monitoring stack's. A LoadBalancer service created
      #                     by accident would bind one.
      #   metrics-server -- THE ONE GENUINELY ARGUABLE ENTRY. It backs `kubectl top` and any HPA,
      #                     and nothing else provides it. It is off because this fleet's answer to
      #                     "how much CPU is that using?" is Prometheus, which is on this very box
      #                     and keeps history, whereas metrics-server keeps a 15-second window in
      #                     memory and costs RAM on the machine that can least spare it. TURN IT
      #                     BACK ON the moment anything wants an HPA -- an HPA with no metrics
      #                     server does not error, it just never scales.
      #
      # local-storage is LEFT ON: nothing else here provides a PersistentVolume, and a PVC that
      # cannot bind leaves a pod Pending with a message nobody reads until they go looking.
      disable = [ "traefik" "servicelb" "metrics-server" ];

      extraFlags = [
        "--cluster-cidr=${cfg.clusterCidr}"
        "--service-cidr=${cfg.serviceCidr}"

        # BIND AND ADVERTISE THE PRIVATE ADDRESS. Left to itself k3s picks the address on the
        # default route -- which on a Hetzner cloud server is the PUBLIC one. The API server and
        # etcd would then advertise themselves across the internet, and the firewall below (which
        # opens these ports on the private interface only) would refuse the traffic. The symptom is
        # a cluster that bootstraps fine and then cannot add a second node.
        "--node-ip=${config.fleet.privateAddress}"
        "--advertise-address=${config.fleet.privateAddress}"
        "--bind-address=${config.fleet.privateAddress}"
        "--flannel-iface=${config.fleet.privateInterface}"

        # NO `--flannel-backend`, so the default VXLAN stands. See the header: host-gw would be
        # faster and would make pod addresses routable, and it requires network routes this fleet
        # has not declared. Changing this line alone produces a cluster whose cross-node pod
        # traffic silently goes nowhere.
        #
        # NO `--resolv-conf` either, which bitcashier's version does set. THE HAZARD IT GUARDS
        # AGAINST STILL EXISTS: if this host ever resolves through systemd-resolved's stub at
        # 127.0.0.53, CoreDNS inherits that address from /etc/resolv.conf and forwards to it -- and
        # 127.0.0.53 inside a pod's network namespace names the POD, not the host. Every external
        # lookup from every pod then returns NXDOMAIN. It is left unset because this fleet has no
        # resolver of its own to point at; if systemd-resolved is enabled on this host, this flag
        # has to come back with a real nameserver behind it.
      ] ++ lib.optionals (!cfg.schedulable) [
        # k3s's own control-plane taint, applied explicitly rather than relying on a default that
        # differs between k3s and upstream Kubernetes.
        "--node-taint=node-role.kubernetes.io/control-plane:NoSchedule"
      ];

      # A NODE THAT IS GOING DOWN SHOULD SAY SO. Without this, a reboot looks to the API server
      # like a node that stopped answering, and its pods are evicted on the node-monitor timeout
      # rather than drained.
      gracefulNodeShutdown.enable = true;
    };

    # FORWARDING, WHICH NO CNI WORKS WITHOUT. A packet to a pod is DNATed and then has to traverse
    # FORWARD; with forwarding off the kernel drops it after the DNAT with no RST and no counter
    # anywhere, which is about the least diagnosable failure available.
    boot.kernel.sysctl = {
      "net.ipv4.ip_forward" = 1;
      "net.bridge.bridge-nf-call-iptables" = 1;
    };

    # THIS UNIT YIELDS TO THE MONITORING STACK. roles/prometheus.nix sets CPUWeight 400/300 and an
    # elevated I/O class on Prometheus and Alertmanager, for the reason its header gives: the
    # process that RECORDS a starvation has to survive it, or the history stops exactly where the
    # evidence would have been. This is the other side of that decision, written here as well so
    # that neither file is the only place it exists.
    #
    # 100 IS SYSTEMD'S DEFAULT, stated explicitly rather than left implicit -- the point is not to
    # penalise k3s (it is a real workload and starving it helps nobody), it is that the ordering is
    # DECLARED. If somebody later raises this, they should have to notice the note in
    # roles/prometheus.nix saying why it was where it was.
    systemd.services.k3s.serviceConfig.CPUWeight = 100;

    # NO FIREWALL RULES HERE. `fleet.firewall.k3sServer = true;` in the host file opens 6443,
    # 10250, 8472/udp and 51820/udp on the private interface; modules/fleet/firewall.nix holds the
    # numbers and the reasoning for each, including why 8472 being UDP is the one that silently
    # breaks cross-node pod traffic when it is missed.
    #
    # NOTHING ANYWHERE OPENS 6443 PUBLICLY, and that is worth restating from this end: `kubectl`
    # from a laptop reaches this cluster over the same ssh path as everything else private here.

    # kubectl on the host, so an operator on the machine can ask it what it thinks. `k3s kubectl`
    # works without this; having the real binary means a runbook reads the same here as anywhere.
    environment.systemPackages = [ pkgs.kubectl ];
  };
}
