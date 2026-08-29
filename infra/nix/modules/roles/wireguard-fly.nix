# THE TUNNEL THAT KEEPS THE DATABASE OFF THE INTERNET.
#
# This host joins Fly.io's private 6PN network as a WireGuard peer -- the configuration that
# `fly wireguard create` hands back: our own private key, Fly's gateway public key and endpoint, and
# an address out of Fly's `fdaa:` ULA range. From the applications' side nothing about reaching
# MongoDB changes: kinowo and kinowo-worker still open a connection over 6PN, exactly as they do to
# the Fly-hosted mongod today. THE CONNECTION URI CHANGES HOST AND NOTHING ELSE.
#
# WHY THIS SHAPE RATHER THAN THE OBVIOUS ALTERNATIVES, since every alternative is worse in a way
# that is easy to underrate:
#
#   * MONGOD ON THE PUBLIC INTERNET WITH A GOOD PASSWORD. Internet-facing database ports are
#     scanned continuously; the exposure is not the password, it is every authentication bypass and
#     pre-auth parser bug the server will ever have, for as long as it is exposed. This is the
#     option the whole role exists to avoid, and the assertion in mongodb.nix refuses it
#     structurally rather than by review.
#   * AN IP ALLOW-LIST OF FLY'S EGRESS ADDRESSES. Those are a shared, changing pool; the list is
#     wrong the day Fly reschedules a machine, and it grants everyone else on the pool anyway.
#   * TLS + client certificates over the public internet. Defensible, and a lot more moving parts:
#     a CA, a rotation story, and a mongod whose port is still reachable by anyone who wants to
#     probe it. The tunnel takes the port off the internet entirely, which is a stronger property
#     than authenticating everyone who reaches it.
#
# ------------------------------------------------------------------------------------------------
# THE HONEST TRADE-OFF: THIS IS ONE TUNNEL, AND THEREFORE ONE POINT OF FAILURE
# ------------------------------------------------------------------------------------------------
#
# A single WireGuard peer sits between every Fly-hosted process and the database. If it goes down --
# Fly's gateway is restarted or moved, the peer is removed on Fly's side with `fly wireguard
# remove`, the key is rotated on one end only, a Hetzner network event drops the UDP flow for long
# enough -- then the apps cannot reach Mongo at all.
#
# WHAT THAT LOOKS LIKE FROM OUTSIDE, which is the part worth knowing in advance because it does NOT
# look like an outage: the web tier keeps serving. It answers from its projected read model in
# memory, health checks stay green, pages render. What stops is UPDATING -- the change stream is
# gone, the worker's writes fail, and the site quietly serves an ageing corpus. The same silent
# shape mongodb.nix's header describes for a missing replica set, arrived at from a different
# direction, and the reason the handshake-age metric published below is not decoration: the
# handshake going stale is the ONLY early signal, because nothing else about the machine changes.
#
# THE MITIGATION, IF IT EVER MATTERS, IS A SECOND PEER: `fly wireguard create` again, a second
# `wg1` here with its own key and Fly gateway, and both addresses in mongod's bind list. That is
# deliberately NOT done today -- it doubles the moving parts for a fleet that has not yet measured
# a single tunnel failure, and the failure is degradation rather than an outage. This paragraph is
# the record that it was considered and declined, so that the next person hitting a tunnel drop
# knows what the intended answer is rather than inventing a different one under pressure.
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.wireguardFly;

  # PUBLISH THE HANDSHAKE AGE. WireGuard is silent by design: a dead peer is indistinguishable from
  # an idle one from the interface's point of view -- `ip link` shows wg0 UP either way, because
  # the interface is a virtual device that exists whether or not anything is on the other end. The
  # last handshake time is the only thing on this machine that knows the difference.
  #
  # node_exporter HAS NO WIREGUARD COLLECTOR, so this is published through its textfile collector,
  # the same route mongodb.nix's dump timer uses. `latest-handshakes` prints 0 for a peer that has
  # NEVER completed one, which is a different fault from a peer that has gone stale (a wrong key
  # versus a lost path) -- the rule file keeps them apart on that basis, so this script must NOT
  # helpfully normalise the zero away.
  handshakeScript = pkgs.writeShellScript "wireguard-fly-handshake-metrics" ''
    set -euo pipefail

    tmp="${cfg.textfileDirectory}/wireguard-fly.prom.$$"
    {
      echo "# HELP kinowo_wireguard_latest_handshake_timestamp_seconds Unix time of the last completed handshake with the Fly gateway, or 0 if there has never been one."
      echo "# TYPE kinowo_wireguard_latest_handshake_timestamp_seconds gauge"
      ${pkgs.wireguard-tools}/bin/wg show ${cfg.interface} latest-handshakes \
        | while read -r peer handshake; do
            echo "kinowo_wireguard_latest_handshake_timestamp_seconds{interface=\"${cfg.interface}\",peer=\"$peer\"} $handshake"
          done
    } > "$tmp"
    # Renamed rather than written in place: node_exporter reads this directory on every scrape and
    # a half-written .prom discards the whole file as a parse error.
    ${pkgs.coreutils}/bin/mv "$tmp" ${cfg.textfileDirectory}/wireguard-fly.prom
  '';
in
{
  options.fleet.wireguardFly = {
    enable = lib.mkEnableOption "a WireGuard peer on Fly.io's private 6PN network";

    interface = lib.mkOption {
      type = lib.types.str;
      default = "wg0";
      description = ''
        The tunnel interface. Named in the firewall rules and in the handshake metric's `interface`
        label, so renaming it is a rename in three places and in every alert annotation.
      '';
    };

    address = lib.mkOption {
      type = lib.types.str;
      example = "fdaa:0:0:a7b:0:1::2/120";
      description = ''
        THIS PEER'S ADDRESS ON 6PN, WITH ITS PREFIX LENGTH, exactly as `fly wireguard create`
        printed it. Not invented and not tidied: the prefix length is what decides which addresses
        the kernel considers on-link, and a `/128` where Fly issued a `/120` produces a tunnel that
        comes up and then cannot reach the gateway's own address.

        THE APPLICATIONS' CONNECTION URI NAMES THIS ADDRESS. It is the one value in this file that
        another system depends on, so moving it is a coordinated change: mongod's bind list here,
        MONGODB_URI on every Fly app there. Fly's `.internal` DNS covers app instances; whether a
        `fly wireguard create` peer is resolvable by name from an app has NOT been verified here,
        so the URI carries the literal address. If somebody confirms a peer name resolves, prefer
        the name -- an address in a connection string is the same trap the address literals in this
        repository's other modules are careful about.
      '';
    };

    privateKeyFile = lib.mkOption {
      type = lib.types.str;
      default = config.sops.secrets."wireguard-fly/private-key".path;
      defaultText = ''config.sops.secrets."wireguard-fly/private-key".path'';
      description = ''
        Path to this peer's WireGuard private key, from sops-nix.

        A PATH, NEVER A VALUE. `networking.wireguard` reads it at runtime, so the key never enters
        the world-readable Nix store -- the same rule the bitcashier wireguard role states for its
        own gateway key, and it matters more here: this key is the whole of the authentication
        between Fly and the database.

        IT IS MOVED, NEVER ROTATED IN ISOLATION. Fly holds the matching PUBLIC key against this
        peer; changing one end and not the other does not error, it simply stops handshaking, which
        presents as the silent degradation described in the module header.
      '';
    };

    peerPublicKey = lib.mkOption {
      type = lib.types.str;
      description = "Fly's gateway public key, from the `fly wireguard create` output. Public by name and nature.";
    };

    peerEndpoint = lib.mkOption {
      type = lib.types.str;
      example = "fra1.gateway.6pn.dev:51820";
      description = ''
        The Fly gateway's `host:port`, as `fly wireguard create` printed it.

        A NAME HERE, DELIBERATELY, WHICH IS THE OPPOSITE OF THIS REPOSITORY'S USUAL RULE ABOUT
        ADDRESSES: it is Fly's endpoint, not ours, and Fly is entitled to move it behind that name.
        WireGuard re-resolves an endpoint name when a handshake fails, so a moved gateway recovers
        on its own; a literal address pinned here would not.
      '';
    };

    allowedIPs = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ "fdaa::/48" ];
      description = ''
        WHAT MAY COME OUT OF THE TUNNEL, AND -- BECAUSE `allowedIPsAsRoutes` IS LEFT ON -- WHAT IS
        ROUTED INTO IT. WireGuard has no separate access-control list: this one list is both the
        cryptographic routing table and the filter that drops any packet arriving from the peer
        with a source outside it.

        `fdaa::/48` IS FLY'S ULA RANGE AND NOTHING ELSE. It deliberately does not include a default
        route: this machine must not send its ordinary traffic through Fly, and a `::/0` here --
        which is what a copied-in client config from `fly wireguard create` may well contain if it
        was generated for a laptop -- would do exactly that, quietly, on the next rebuild.
      '';
    };

    persistentKeepalive = lib.mkOption {
      type = lib.types.int;
      default = 25;
      description = ''
        SECONDS BETWEEN KEEPALIVES, AND IT IS NOT OPTIONAL HERE.

        This host is the INITIATOR: it dials Fly's gateway over UDP, and the return path exists
        only for as long as the conntrack entry for that flow does. Linux's UDP conntrack timeout
        is 180 seconds at its longest and 30 at its shortest (an unreplied flow), so a tunnel that
        is idle in the outbound direction stops being reachable INBOUND -- which is the direction
        that matters, because the applications are the ones opening connections to the database.
        25s is comfortably inside every one of those timeouts and is WireGuard's own recommended
        value for a peer behind stateful middleboxes.

        Setting this to 0 does not break the tunnel in a way that shows up in testing: traffic from
        this side revives it instantly, so an interactive check from the machine always succeeds
        while the apps see intermittent connection failures.
      '';
    };

    textfileDirectory = lib.mkOption {
      type = lib.types.str;
      default = config.fleet.observability.textfileDirectory;
      defaultText = "config.fleet.observability.textfileDirectory";
      description = ''
        Where the handshake metric is published for node_exporter to pick up.

        READ FROM THE FLEET OPTION, not restated: modules/fleet/observability.nix both creates this
        directory and passes it to node_exporter as `--collector.textfile.directory`, so writer and
        reader cannot drift apart. The rule file still alerts on the series being ABSENT, because
        the timer failing is a different fault from the directory being wrong and both end as
        silence.
      '';
    };

    mongoPort = lib.mkOption {
      type = lib.types.port;
      default = 27017;
      description = ''
        The port opened to Fly on this interface. Defaulted rather than read from
        `config.fleet.mongodb.port` so this role stays usable on a host that runs no database --
        the assertion below is what keeps the two in step when both are enabled.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = !config.fleet.mongodb.enable || cfg.mongoPort == config.fleet.mongodb.port;
        message = ''
          fleet.wireguardFly.mongoPort (${toString cfg.mongoPort}) does not match
          fleet.mongodb.port (${toString config.fleet.mongodb.port}). The tunnel would then be open
          on a port nothing listens on, and mongod would be listening on a port the tunnel drops --
          which reads as a Fly networking fault rather than as a one-line disagreement here.
        '';
      }
      {
        assertion = !(lib.elem "::/0" cfg.allowedIPs);
        message = ''
          fleet.wireguardFly.allowedIPs contains ::/0. That routes this machine's ENTIRE IPv6
          traffic through Fly's gateway -- the default in a `fly wireguard create` config generated
          for a laptop, and never what a server peer wants. Name Fly's own range (fdaa::/48).
        '';
      }
    ];

    # 0400 root: `networking.wireguard`'s setup script runs as root and reads the key directly.
    # Widening this is not a small mistake -- the key IS the authentication between Fly and the
    # database, and there is no second factor behind it.
    sops.secrets."wireguard-fly/private-key" = { mode = "0400"; };

    networking.wireguard.enable = true;
    networking.wireguard.interfaces.${cfg.interface} = {
      ips = [ cfg.address ];
      privateKeyFile = cfg.privateKeyFile;

      # NO `listenPort`. As the initiator this peer wants an EPHEMERAL source port: a fixed one
      # would be a port to firewall and a port to collide with, and buys nothing, because nothing
      # on the internet ever needs to open a handshake TO this machine. Fly's gateway only ever
      # replies on the flow this side started -- which is also why nothing below opens a UDP port
      # to the public interface, and why the keepalive above is what keeps that flow alive.

      peers = [{
        publicKey = cfg.peerPublicKey;
        endpoint = cfg.peerEndpoint;
        allowedIPs = cfg.allowedIPs;
        persistentKeepalive = cfg.persistentKeepalive;

        # `allowedIPsAsRoutes` IS LEFT AT ITS DEFAULT OF TRUE, which is what installs the
        # `fdaa::/48 dev wg0` route -- the second half of the tunnel and the half that is easy to
        # forget, because without it the interface comes up, handshakes, and carries nothing: every
        # packet for an fdaa: address goes out the default route to a gateway that drops it. Named
        # here rather than left implicit precisely because setting it false is a one-word change
        # that produces a tunnel which looks healthy in `wg show` and moves no traffic.
      }];
    };

    # BINDING AN ADDRESS BEFORE ITS INTERFACE EXISTS, ON PURPOSE.
    #
    # mongod binds the tunnel address (see fleet.mongodb.bindAddresses). Without this sysctl that
    # bind FAILS while wg0 is absent -- at boot, during a `wg-quick` restart, or for as long as
    # Fly's gateway is unreachable -- and mongod exits. The consequence would be that a tunnel
    # problem takes the database down for EVERYONE, including the private network and the local
    # dump timer, which is a far larger blast radius than the tunnel's own.
    #
    # With it, mongod starts and listens on all three addresses regardless; the tunnel address is
    # simply not reachable until wg0 comes up, at which point it starts working with no restart.
    #
    # THE COST, STATED: a socket bound to an address that routes nowhere looks healthy locally. That
    # is exactly the blindness the handshake metric below exists to cover, and it is why that timer
    # is part of this role rather than an optional extra -- the sysctl trades a loud failure for a
    # quiet one, so the quiet one has to be watched.
    boot.kernel.sysctl."net.ipv6.ip_nonlocal_bind" = 1;

    # ORDERING ONLY, NEVER A REQUIREMENT. mongod should come up AFTER the tunnel when both start in
    # the same transaction, so that the common case needs no retry -- but it must not be gated on
    # it: `requires` would mean a failed tunnel keeps the database from starting at all, which is
    # the coupling the sysctl above exists to break.
    systemd.services.mongodb = lib.mkIf config.fleet.mongodb.enable {
      after = [ "wireguard-${cfg.interface}.service" ];
    };

    systemd.services.wireguard-fly-metrics = {
      description = "Publish Fly WireGuard handshake age for node_exporter";
      serviceConfig = {
        Type = "oneshot";
        # root: `wg show` reads the interface through a netlink socket that needs CAP_NET_ADMIN.
        # Only `latest-handshakes` is asked for -- deliberately not `wg show all dump`, which
        # includes the PRIVATE KEY in its first column and would put it in the journal on any error.
        ExecStart = handshakeScript;
        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectHome = true;
        ProtectSystem = "strict";
        ReadWritePaths = [ cfg.textfileDirectory ];
      };
    };

    systemd.timers.wireguard-fly-metrics = {
      description = "Publish Fly WireGuard handshake age every minute";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnBootSec = "1m";
        # ONE MINUTE, against a rule that fires on a handshake older than five. WireGuard
        # re-handshakes about every two minutes while traffic flows, so a sampling interval near
        # that period would alias -- a healthy tunnel would occasionally read as stale and a
        # marginal one as healthy.
        OnUnitActiveSec = "1m";
      };
    };

    # THE DATABASE PORT, ON THE TUNNEL. THE ONE FIREWALL RULE ANY ROLE IN THIS TREE WRITES DIRECTLY,
    # and the exception is narrow rather than a lapse: every option in `fleet.firewall` is scoped to
    # `fleet.privateInterface`, and wg0 is not that interface. The objection its header raises
    # against a role naming an interface does not apply either -- the name here is this role's own
    # `interface` option, the same value that created the device, so the two cannot disagree.
    #
    # This is the WHOLE of what Fly is granted on this machine. Nothing is forwarded: this host is
    # an ENDPOINT on 6PN, not a gateway into the Hetzner private network, so a compromised Fly app
    # reaches mongod and stops there.
    #
    # A DISAGREEMENT WORTH RESOLVING RATHER THAN INHERITING: modules/fleet/firewall.nix opens
    # 51820/udp INBOUND on all three hosts, with a comment that "Fly DIALS IN from its gateway
    # rather than us dialling out". THIS ROLE IS BUILT THE OTHER WAY ROUND -- `fly wireguard create`
    # hands back a peer configuration with an Endpoint, so this side initiates, which is why no
    # `listenPort` is set above and why the keepalive matters. If this side initiates, that inbound
    # opening is inert here (nothing listens on 51820) and harmless. If Fly really must dial in,
    # then this role needs a FIXED `listenPort` and that rule is what carries it -- one of the two
    # files is wrong, and finding out which is a five-minute check with `wg show` after the first
    # handshake, not a thing to leave as two comments that disagree.
    networking.firewall.interfaces.${cfg.interface}.allowedTCPPorts = [ cfg.mongoPort ];

    # THE MTU IS LEFT AT WIREGUARD'S DEFAULT (1420), which is right for a 1500-byte path and is a
    # guess about a path nobody here has measured. If large responses from Mongo ever stall while
    # small ones succeed -- the classic path-MTU black hole, and it presents as "the database is
    # slow" rather than as a network fault -- lowering this is the first thing to try. Stated as a
    # risk, not as a history: it has not happened here.

    environment.systemPackages = [ pkgs.wireguard-tools ];
  };
}
