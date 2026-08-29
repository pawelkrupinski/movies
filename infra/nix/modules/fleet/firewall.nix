# The IN-HOST firewall, which states the same intent as Hetzner's edge firewall INDEPENDENTLY.
#
# WHY BOTH, WHEN ONE WOULD DO. infra/terraform/firewall.tf sits ahead of the public NIC and is
# invisible from inside the machine: a host attached to it shows a correct ruleset, a listening
# socket and a green systemd unit while dropping the traffic it exists to serve, and no
# `nixos-rebuild` and no in-host check can see that. The converse is worse. A Hetzner firewall is
# attached PER SERVER, so a machine created outside terraform, or one whose attachment was dropped
# during a `convert-host` run, is COMPLETELY UNFILTERED on the public internet and nothing about
# the machine looks different. This file is what that host still has.
#
# It is not a copy of the edge rules and must not become one. The edge decides what may arrive on
# the public NIC; this decides what this host answers on either NIC. THE PRIVATE NETWORK IS
# COVERED HERE AND NOWHERE ELSE -- Hetzner Cloud firewalls do not filter ens10 at all, so every
# private-side rule below is the only rule there is.
#
# NARROW BY CONSTRUCTION rather than by purging: NixOS's firewall denies by default, so this file
# only ever opens things. Nothing on the host got there except through this file, so there is
# nothing to purge.
#
# ONE LESSON CARRIED OVER VERBATIM FROM bitcashier, about FORWARD. Its monitoring node needed
# `forwarding_ipv4: true` because a published container port is a ROUTED path -- the packet is
# DNATed and then has to traverse FORWARD, and with forwarding off the kernel drops it after the
# DNAT with no RST and no counter anywhere. THAT APPLIES HERE THE DAY k3s DOES ANYTHING: a CNI
# turns forwarding on for itself, and a NodePort or a pod-to-pod hop across the flannel VXLAN below
# is exactly that routed path. k3s-worker-1 is idle today, so nothing here turns forwarding on and
# nothing here needs to; this note is so that the person who first schedules a workload knows the
# shape of the failure before they meet it.
{ config, lib, ... }:

let
  cfg = config.fleet.firewall;
in
{
  options.fleet.firewall = {
    # -----------------------------------------------------------------------------------------
    # WHAT A HOST DECLARES IT NEEDS
    # -----------------------------------------------------------------------------------------
    #
    # Named intents rather than port numbers at the call site, and the reason is that a port number
    # in a host file is unreviewable: `27017` is either the database or a typo, and only one of
    # those is visible in a diff. `fleet.firewall.mongo = true;` in hosts/mongo-1 says what the
    # machine is for, and the number it implies lives here beside every other number, where they
    # can be read as one ruleset.
    #
    # Every one of these opens on the PRIVATE interface only. There is no option here that opens a
    # service publicly, deliberately: the public surface of this fleet is 22 and icmp, it is
    # written out below, and widening it should require editing THIS file rather than setting a
    # flag in a host file.

    mongo = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        MongoDB on 27017, from the private network.

        THE PORT BEING OPEN IS NOT WHAT MAKES THE DATABASE PRIVATE -- the bind address is. mongod
        binds `fleet.privateAddress`, Hetzner's edge firewall never names 27017, and this rule
        admits it only on ens10 from a /24 that only three machines are on. All three have to hold;
        this is the one of the three that survives a mongod misconfigured to bind 0.0.0.0.
      '';
    };

    monitoring = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Prometheus (9090), Alertmanager (9093) and Grafana (3000), from the private network.

        GRAFANA IS THE UNCOMFORTABLE ONE and it is worth being explicit about: 3000 is a human's
        web UI, and it is open on a network no human's laptop is on. That is deliberate -- reaching
        it means an ssh tunnel to monitoring-1, which costs one command and keeps a login page with
        its own user database off the public internet entirely. If a public Grafana is ever wanted,
        it needs a rule in infra/terraform/firewall.tf as well as here, and it should get TLS and a
        reverse proxy in the same change rather than an extra port.
      '';
    };

    k3sServer = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        The k3s control plane: apiserver on 6443/tcp, kubelet on 10250/tcp, flannel's VXLAN on
        8472/udp and WireGuard on 51820/udp.

        A SERVER IS ALSO A NODE, so this implies everything `k3sAgent` opens. Written as two
        options rather than one because monitoring-1 is both and k3s-worker-1 is only the second,
        and a single flag would have made the worker open an apiserver port with no apiserver
        behind it -- an open port with no listener is not harmless, it is a rule nobody can explain
        later.
      '';
    };

    k3sAgent = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        A k3s node: kubelet on 10250/tcp (the server scrapes it and `kubectl logs`/`exec` go
        through it), flannel VXLAN on 8472/udp, WireGuard on 51820/udp.

        8472 IS UDP AND IS EASY TO GET WRONG. VXLAN failing is not a connection error -- pods come
        up, the node reports Ready, and traffic between pods on DIFFERENT nodes silently goes
        nowhere while everything single-node works. With one server and one worker, that is exactly
        the traffic this cluster exists to carry.
      '';
    };

    # -----------------------------------------------------------------------------------------
    # The escape hatch
    # -----------------------------------------------------------------------------------------

    privateTCPPorts = lib.mkOption {
      type = lib.types.listOf lib.types.port;
      default = [ ];
      description = ''
        Extra TCP ports a host or role opens on the private interface, for something that does not
        yet deserve a named option above.

        ROLES APPEND HERE RATHER THAN WRITING `networking.firewall.interfaces.<name>` THEMSELVES,
        and that is the whole reason this option exists: the interface name is
        `fleet.privateInterface`, and a role that spelled it out would silently open NOTHING on a
        host whose private NIC is called something else -- iptables accepts a rule naming an
        interface that does not exist, installs it, prints it correctly in `iptables -S`, and never
        matches it.

        The fleet-wide ports below are NOT in here. They are what every host gets whatever it does,
        and a role must not be able to close them.
      '';
    };

    privateUDPPorts = lib.mkOption {
      type = lib.types.listOf lib.types.port;
      default = [ ];
      description = "Extra UDP ports a host or role opens on the private interface.";
    };
  };

  config.networking.firewall = {
    enable = true;

    # ---------------------------------------------------------------------------------------------
    # THE PUBLIC SURFACE OF THIS FLEET, IN THREE LINES
    # ---------------------------------------------------------------------------------------------
    #
    # UNQUALIFIED BY INTERFACE, AND THAT IS THE SAFE DIRECTION HERE. bitcashier scopes its one
    # public rule to an ADDRESS because it has hosts whose primary path is private and whose public
    # 22 is a deliberate exception. Here the public NIC is the ONLY administrative path -- colmena,
    # nixos-anywhere, the GitHub Actions staging job and the operator all arrive on it -- so a rule
    # that stopped matching would lock the fleet out with no console-free way back. An unqualified
    # `allowedTCPPorts` cannot stop matching. What narrows port 22 is key-only authentication
    # (fleet/default.nix), not a source address this estate does not have a stable set of.
    #
    # These agree, deliberately and independently, with the rules in infra/terraform/firewall.tf.
    # If you add one there, add it here and say why in both.
    allowedTCPPorts = [
      22 # ssh -- the only administrative path onto any of these machines
    ];

    # EMPTY, AND THAT IS THE WHOLE POINT. It briefly carried 51820 for mongo-1's tunnel into Fly's
    # 6PN, on the assumption that Fly dials in. It does not: `fly wireguard create` hands back a peer
    # configuration naming Fly's gateway as the `Endpoint`, so THIS side initiates, and
    # roles/wireguard-fly.nix sets no `listenPort` precisely so the source port stays ephemeral.
    # Return traffic therefore belongs to an already-established flow, which conntrack passes without
    # any rule naming it -- so the port opened nothing except a listener for the internet to find.
    #
    # IF THE DIRECTION EVER REVERSES it comes back here AND in terraform/firewall.tf, in ONE change,
    # alongside the `listenPort` that would make it meaningful. Split across two commits the tunnel
    # simply never establishes, and the symptom is a database the Fly apps cannot reach.
    allowedUDPPorts = [ ];

    # ICMP is admitted by `networking.firewall.allowPing`, which nixpkgs defaults to TRUE, and it is
    # left at that default rather than restated. Said out loud only because the edge firewall names
    # icmp explicitly and a reader comparing the two files would otherwise read the absence as a
    # disagreement: a host that does not answer ping is a host whose reachability nobody can test
    # from outside during the incident where that is the only question.

    # ---------------------------------------------------------------------------------------------
    # THE PRIVATE INTERFACE -- everything this fleet actually does
    # ---------------------------------------------------------------------------------------------
    #
    # Scoped to `fleet.privateInterface` rather than to the 10.20.0.0/24 SOURCE, and the difference
    # is worth stating because the source form looks stricter. It is not, on this hardware: ens10 is
    # attached to exactly one Hetzner cloud network, nothing else can put a packet on it, and a
    # source-address rule additionally admits a SPOOFED 10.20.0.x arriving on the public NIC unless
    # something else stops it. The interface is the boundary; the address range is a description of
    # what is on the far side of it.
    interfaces.${config.fleet.privateInterface} = {
      allowedTCPPorts =
        [
          # FLEET-WIDE, on every host whatever it runs.
          22 # ssh again -- so a host stays reachable from its siblings when the public path is
          # what has broken. This is the only redundant administrative path this fleet has.
          config.fleet.nodeExporterPort # 9100; monitoring-1 scrapes every host here
        ]
        ++ lib.optional cfg.mongo 27017
        ++ lib.optionals cfg.monitoring [
          9090 # prometheus
          9093 # alertmanager
          3000 # grafana
        ]
        ++ lib.optional cfg.k3sServer 6443 # apiserver
        # 10250 for BOTH roles, hence the `||`: a server runs a kubelet of its own, so writing this
        # under `k3sServer` alone would leave the control-plane node unscrapeable by its own
        # cluster, and writing it twice would be two lists to keep in step.
        ++ lib.optional (cfg.k3sServer || cfg.k3sAgent) 10250 # kubelet
        ++ cfg.privateTCPPorts;

      allowedUDPPorts =
        lib.optionals (cfg.k3sServer || cfg.k3sAgent) [
          8472 # flannel VXLAN -- see the k3sAgent option for how this fails when it is missing
          51820 # k3s's own WireGuard backend, if flannel is ever switched to it
        ]
        ++ cfg.privateUDPPorts;
    };

    # ---------------------------------------------------------------------------------------------
    # A PACKET ADDRESSED TO LOOPBACK THAT DID NOT ARRIVE ON LOOPBACK
    # ---------------------------------------------------------------------------------------------
    #
    # Ported from bitcashier, which ported it from a Puppet rule older than either. WHAT IT
    # PROTECTS HERE: anything bound to 127.0.0.1 is unauthenticated BECAUSE it is bound to
    # 127.0.0.1 -- and on this fleet that is a real list, not a hypothetical one. Grafana's
    # datasource reaches Prometheus that way, k3s's kubelet talks to its own components that way,
    # and a mongod bound to loopback for a maintenance window is the standard way to run one. A
    # packet arriving on ens10 carrying a 127.0.0.0/8 destination is asking for exactly those, and
    # there is no legitimate sender of one.
    #
    # WHAT IS ALREADY STOPPING THEM, stated because a rule sold as the defence when it is the
    # second line is a rule nobody re-checks: the kernel refuses a martian destination in its own
    # route lookup unless `net.ipv4.conf.<if>.route_localnet` is 1, which is 0 by default and which
    # nothing in this tree sets. So on today's hosts this rule fires NEVER, and that is the honest
    # reading of what it buys.
    #
    # WHY HAVE IT ANYWAY. `route_localnet` is not an exotic switch, it is the one a container
    # runtime flips -- Docker sets it on its bridge to make a port published to a loopback address
    # routable, and k3s-worker-1 exists to run containers. This is the rule that stops the first
    # workload scheduled there turning a loopback-only service into something the private network
    # can reach. One rule installed before the reason for it exists is the cheap half of that trade.
    #
    # WHY `-I nixos-fw 1` AND NOT `-A`, WHICH IS WHAT MAKES IT WORK RATHER THAN MERELY READ
    # CORRECTLY. `extraCommands` is spliced into firewall-start immediately before the terminal
    # `-A nixos-fw -j nixos-fw-log-refuse`, and therefore AFTER every accept the module emits.
    # Those accepts match on interface and port only -- `-A nixos-fw -p tcp --dport 9090 -j
    # nixos-fw-accept -i ens10` -- so a spoofed packet to 127.0.0.1:9090 arriving on ens10 would be
    # ACCEPTED several rules before an appended reject could see it. Appending installs a rule that
    # reads correctly in `iptables -S` and never fires. Position 1 is also independent of whatever
    # any role appends later, and cannot accumulate: firewall-start flushes and recreates
    # `nixos-fw` on every start, so there is exactly one of these and it is first.
    #
    # WHY IT IS SAFE AHEAD OF THE LOOPBACK ACCEPT. nixpkgs sets `trustedInterfaces = [ "lo" ]`
    # unconditionally when the firewall is on, which emits `-A nixos-fw -i lo -j nixos-fw-accept`
    # as the chain's first rule. Position 1 puts this ahead of it, so the `! -i lo` is what keeps
    # loopback traffic working. Note that naming `lo` does NOT reopen the nonexistent-interface
    # trap the private block above avoids: `lo` is present under that name on every Linux host
    # whatever image built it, and more to the point the NEGATED form fails SAFE -- were it somehow
    # absent, `! -i lo` matches everything and loopback services break immediately and loudly,
    # rather than the rule silently never matching.
    extraCommands = lib.mkMerge [
      ''
        iptables -I nixos-fw 1 ! -i lo -d 127.0.0.0/8 -j nixos-fw-log-refuse
      ''

      # The IPv6 counterpart. GUARDED ON `enableIPv6` because firewall-start runs under `sh -e`: on
      # a host with IPv6 off, `ip6tables` would fail, abort the start script, and take the WHOLE
      # firewall down with it rather than just this rule -- which fails in the direction of an
      # unfiltered public NIC.
      (lib.mkIf config.networking.enableIPv6 ''
        ip6tables -I nixos-fw 1 ! -i lo -d ::1/128 -j nixos-fw-log-refuse
      '')
    ];
  };

  # A CHANGE TO ANY RULE ABOVE MUST RESTART THE FIREWALL, NOT RELOAD IT.
  #
  # `extraCommands` is emitted into firewall-START only, and nixpkgs ships the unit with
  # `reloadIfChanged = true`, so a switch that touches only those rules completes cleanly, reports
  # "reloading firewall.service", and applies NONE of them. Measured on bitcashier on 2026-08-23:
  # after a successful switch the generated firewall-start held 15 rules, firewall-reload held
  # none, and the live chain had none until the unit was restarted by hand. That is the exact shape
  # of failure this whole file is written against -- a ruleset that reads correctly and is not
  # installed.
  #
  # WHAT IT COSTS, stated because it is a real trade and not a free win: a reload closes the door
  # first (firewall-reload inserts a `nixos-drop` jump before re-running the start script), whereas
  # a restart runs ExecStop -- which deletes `-A INPUT -j nixos-fw` -- and leaves the host open for
  # the moment before ExecStart re-adds it. A sub-second open window is the lesser harm than a
  # deploy that silently applies no rules at all.
  #
  # It also means auto-apply will not touch a firewall change unattended: ./auto-apply.nix's
  # gate refuses any closure that would restart a unit, and this makes every rule change exactly
  # that. Deliberate. A firewall edit is one of the few things on this fleet worth a person
  # watching the switch.
  config.systemd.services.firewall.reloadIfChanged = lib.mkForce false;

  # sshd's OWN PORT IS NOT LEFT TO nixpkgs. `services.openssh.openFirewall` defaults to TRUE and
  # would emit an unscoped `--dport 22 -j nixos-fw-accept`, which happens to be what the public
  # rule above wants -- so leaving it on would be harmless TODAY and would silently outlive any
  # future decision to narrow port 22. Turning it off means the only rules admitting ssh are the
  # two written above, both visible in this file, and neither of them arriving from a module
  # default nobody read.
  config.services.openssh.openFirewall = false;
}
