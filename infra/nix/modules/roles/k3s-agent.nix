# A k3s AGENT -- k3s-worker-1, the cx43 in fsn1. The half of the cluster meant to carry workload.
#
# DELIBERATELY A SEPARATE ROLE FROM k3s-server.nix RATHER THAN A FLAG ON IT, which is bitcashier's
# reasoning and holds here unchanged: the two differ in what they are allowed to LOSE. A server
# holds the cluster's etcd; an agent holds nothing and is replaced by rebuilding it. Folding them
# into one module with a `role` option makes that distinction a value in a config file -- the kind
# of thing that gets flipped in a hurry, and the failure is silent, because a machine promoted to
# server by accident joins the control plane and looks like a healthy node.
#
# WHAT AN AGENT DOES NOT HAVE, each absence being the point: no etcd, no API server, no `disable`
# list (the add-ons are the server's), no `clusterInit`. What it needs is a server address, the
# cluster token, and the same networking decisions the server made -- because a node that disagrees
# with its cluster about the pod CIDR does not fail to join. It joins, and then its pods cannot be
# reached.
#
# ------------------------------------------------------------------------------------------------
# THIS NODE AND ITS SERVER ARE NOW IN THE SAME LOCATION, AND THAT WAS NOT ALWAYS TRUE
# ------------------------------------------------------------------------------------------------
#
# Both are in fsn1 as of 2026-09-01. Until then this machine was in hel1 and monitoring-1 in nbg1,
# roughly 24ms apart, and this note recorded why that was acceptable: the traffic between an agent
# and its API server is kubelet heartbeats plus the VXLAN overlay, and both tolerate a link measured
# in milliseconds rather than microseconds. That reasoning was sound and is the reason the split was
# left alone for as long as it was -- what moved the machines was the app pods' MONGO traffic, which
# is on the request path and does not tolerate it (see terraform/server.k3s-worker.tf).
#
# WHAT STILL MUST NOT FOLLOW is a second SERVER placed far away "for redundancy". That would put
# etcd's raft quorum across a long link, where every write waits on it and a brief blip becomes a
# leader election. Distance stays cheap for an agent and expensive for a quorum, whatever the
# current layout happens to be.
#
# THE NODE IS IDLE TODAY, on purpose: the cluster carries no kinowo workload yet. Standing it up
# empty is what makes it possible to break it deliberately before anything depends on it.
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.k3sAgent;
in
{
  options.fleet.k3sAgent = {
    enable = lib.mkEnableOption "a k3s agent (Kubernetes worker)";

    serverAddr = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = ''
        The server this node joins, as `https://<address>:6443`.

        NO DEFAULT AND NO EXAMPLE ADDRESS. It is expected to be read in flake.nix off the SERVER's
        own `fleet.privateAddress`, so that the two cannot drift and so that no literal address
        exists here to be copied after it has stopped pointing at the right machine.

        AN ADDRESS, NOT A NAME: this is read on a freshly built node before the fleet's own DNS is
        necessarily answering, and a join that failed for want of a resolver is diagnosed as a
        token or a firewall problem for far longer than it deserves.
      '';
    };

    tokenFile = lib.mkOption {
      type = lib.types.str;
      default = config.sops.secrets."k3s/cluster-token".path;
      defaultText = ''config.sops.secrets."k3s/cluster-token".path'';
      description = ''
        Path to the shared cluster token, from sops-nix. THE SAME SECRET NAME THE SERVER USES --
        that is what "shared" means, and it is why both roles default to one path rather than each
        host naming its own.

        A PATH, NEVER A VALUE: this token is what lets a machine join the cluster, and putting it
        in the unit's environment would publish it to `systemctl show` and to the journal.
      '';
    };

    nodeLabels = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      example = [ "kinowo.pl/location=fsn1" ];
      description = ''
        Labels this node carries, as `key=value`.

        EMPTY TODAY, AND THAT IS HONEST RATHER THAN LAZY: with one worker there is nothing to
        select between, and a label invented before there is a scheduling decision to make is a
        label that will be wrong by the time there is one. The option exists because the moment a
        SECOND worker appears -- especially one in another location -- affinity stops being
        theoretical.
      '';
    };

    clusterCidr = lib.mkOption {
      type = lib.types.str;
      default = "10.42.0.0/16";
      description = ''
        The pod network, WHICH MUST MATCH THE SERVER'S.

        A disagreement does not fail to join. It joins, and this node's pods are unreachable from
        everywhere else -- which reads as a CNI fault rather than as a typo. Defaulted to the same
        value as the server role so that agreeing is what happens by default and disagreeing takes
        an edit in two places.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.serverAddr != "";
        message = ''
          fleet.k3sAgent.serverAddr is required: an agent has no cluster of its own to fall back
          on, and k3s with no server address does not refuse to start -- it WAITS, which looks like
          a slow boot rather than a missing setting.
        '';
      }
    ];

    sops.secrets."k3s/cluster-token" = { mode = "0400"; };

    services.k3s = {
      enable = true;
      role = "agent";
      inherit (cfg) serverAddr tokenFile;

      extraFlags = [
        # THE PRIVATE ADDRESS, for the reason k3s-server.nix states at more length: left alone k3s
        # takes the default route's address, which on a Hetzner cloud server is the PUBLIC one. On
        # an agent the symptom is worse than on a server, because the join SUCCEEDS and then every
        # pod here is advertised at an address the rest of the cluster reaches over the internet,
        # if at all.
        "--node-ip=${config.fleet.privateAddress}"
        "--flannel-iface=${config.fleet.privateInterface}"

        # NO `--flannel-backend` HERE, AND ITS ABSENCE IS LOAD-BEARING RATHER THAN AN OVERSIGHT.
        # That flag is SERVER-ONLY: an agent given it exits immediately with
        # `flag provided but not defined: -flannel-backend`, taking the whole `nixos-rebuild
        # switch` down with it. The backend is a property of the CLUSTER, chosen once on the server
        # and inherited by every agent. `--flannel-iface` IS an agent flag and stays.
        #
        # NO `--node-taint`. An idle cluster's only worker refusing pods is indistinguishable from
        # a broken one at the moment somebody finally schedules something; the way this node stays
        # empty is that nothing is deployed to it, which is visible, rather than a taint, which is
        # not.
      ] ++ map (l: "--node-label=${l}") cfg.nodeLabels;

      # A NODE THAT IS GOING DOWN SHOULD SAY SO -- pods drained rather than evicted on the
      # node-monitor timeout. It matters more here than on the server, because this is where
      # workload is meant to land, and a job that must not run twice is exactly the kind of thing
      # an ungraceful eviction plus a rescheduled replacement can overlap.
      gracefulNodeShutdown.enable = true;
    };

    # FORWARDING, which no CNI works without: a DNATed packet has to traverse FORWARD, and with
    # forwarding off the kernel drops it with no RST and no counter anywhere.
    boot.kernel.sysctl = {
      "net.ipv4.ip_forward" = 1;
      "net.bridge.bridge-nf-call-iptables" = 1;
    };

    # NO FIREWALL RULES HERE. `fleet.firewall.k3sAgent = true;` in the host file opens the kubelet
    # (10250) and flannel's VXLAN (8472/udp) on the private interface. See modules/fleet/
    # firewall.nix, whose comment on that option spells out the VXLAN failure: pods come up, the
    # node reports Ready, and traffic between pods on DIFFERENT nodes silently goes nowhere -- which
    # with one server and one worker is exactly the traffic this cluster exists to carry.

    # NO NodePort RANGE IS OPENED, and no Consul registration exists to open one for. bitcashier's
    # agent role registers each declared NodePort into Consul so its HAProxy edges can reach a
    # service; this fleet has neither, and nothing outside the cluster reaches a pod today. When
    # something does, the ports to open are the PINNED NodePorts named in whatever declares them --
    # never the whole 30000-32767 range, which would additionally admit every port a future
    # manifest happens to take.

    environment.systemPackages = [ pkgs.kubectl ];
  };
}
