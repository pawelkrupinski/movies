# What every host on this fleet gets, whoever it is.
#
# PORTED FROM bitcashier's `nix/modules/fleet/default.nix`, INCLUDING ITS CENTRAL ARGUMENT: this is
# taken as ONE module rather than as pieces a host picks from. That repository learned the cost of
# the alternative on a host (`borg-ovh-1`) that named four pieces explicitly and then missed every
# fleet-wide guarantee added after it was built -- four separate consequences, all of them the same
# consequence. Three hosts is exactly the size at which that mistake is cheapest to make and least
# visible, so the shape is kept.
#
# WHAT WAS DELIBERATELY DROPPED IN THE PORT, so that a reader does not go looking for it:
#
#   * CONSUL and VAULT -- no service mesh and no secret broker here. Service discovery is three
#     static private addresses in a /24, and secrets are sops-nix files decrypted at activation by
#     a key derived from the host's own ssh host key. Both of those are simpler than the thing they
#     replace, and both stop being simpler the moment a fourth host with a floating address exists.
#   * PUPPET -- there is no second, Puppet-managed half of this estate to keep parity with, so
#     every "matches bitcashier::common" accommodation (the vendored metric scripts, gai.conf,
#     hidepid, the locale pin argued as a pin) went with it. Where one of those is kept below it is
#     kept on its own merits and says so.
#   * TAILSCALE and the identity/LDAP module -- operator access here is an ssh key in
#     ./accounts.nix and nothing else.
#   * VICTORIALOGS / promtail -- nothing ships logs off these boxes yet. `services.journald.storage
#     = "persistent"` below is the whole of the log retention story, and it is deliberately the
#     first thing to revisit when an incident here needs evidence from a host that has rebooted.
#   * BITCOIN and everything downstream of it. This fleet runs a MongoDB, a Prometheus/Grafana and
#     an idle k3s. Nothing here holds money.
{ config, lib, pkgs, ... }:

{
  imports = [
    ./accounts.nix
    ./firewall.nix
    ./nix-gc.nix
    ./observability.nix
    ./deploy-staging.nix
    ./auto-apply.nix
  ];

  # -----------------------------------------------------------------------------------------------
  # What a host has to say about itself
  # -----------------------------------------------------------------------------------------------

  options.fleet = {
    role = lib.mkOption {
      type = lib.types.str;
      example = "mongo";
      description = ''
        What this machine is for, in one word. It must AGREE WITH THE `role` LABEL
        infra/terraform/server.*.tf puts on the same server -- the label is what
        `hcloud_firewall_attachment`, the ansible inventory and any future service discovery select
        on, and this is what the host publishes about itself in the `machine` inventory gauge. Two
        answers to "what is this box" is how a machine ends up excluded from a sweep that was
        supposed to cover it and included in one that was not.
      '';
    };

    environment = lib.mkOption {
      type = lib.types.str;
      default = "prod";
      description = ''
        Matching the `env` label in terraform. Every host in this fleet is `prod` today, which is
        precisely why it is an option rather than a literal: the first non-prod machine must be
        able to say so without editing a module every other host also reads.
      '';
    };

    privateAddress = lib.mkOption {
      type = lib.types.str;
      example = "10.20.0.10";
      description = ''
        This host's address on the 10.20.0.0/24 private network, and the value colmena uses as
        `deployment.targetHost`.

        IT IS THE ADMINISTRATIVE PATH AND THE DATA PATH BOTH. Mongo listens here, Prometheus
        scrapes here, k3s joins here, and node_exporter binds here and nowhere else. Hetzner's edge
        firewall does not filter this interface at all (see infra/terraform/firewall.tf), so
        "unreachable from the internet" for those services is a property of the BIND ADDRESS, not
        of a rule -- which is why this option exists rather than each service naming an address of
        its own and one of them getting it wrong.

        NO DEFAULT, DELIBERATELY. A host that forgets it must fail to build, because the failure
        mode of a wrong or missing value here is a node_exporter that cannot bind (it exits, it
        does not retry) and a mongod listening somewhere nobody expects.
      '';
    };

    publicAddress = lib.mkOption {
      type = lib.types.str;
      default = "";
      example = "2.28.56.140";
      description = ''
        The host's public IPv4, as pinned in infra/terraform/primary_ips.tf.

        IT EXISTS BECAUSE THERE IS NO MACHINE INSIDE THE PRIVATE NETWORK THAT CAN DEPLOY. bitcashier
        stages closures from a self-hosted runner on 10.0.0.0/8 and therefore never needed a public
        address in the flake at all. Here CI is `ubuntu-latest` on GitHub's own infrastructure, so
        `bin/stage-nixos-closures` reaches every host over this address -- see that file, which
        reads exactly this attribute rather than carrying a roster of its own.

        Empty means "this host cannot be staged onto", and the staging tool reports that as a
        FAILURE rather than skipping it. That is the right direction: a host nobody can copy a
        closure to is a host that will silently stop tracking main.
      '';
    };

    privateInterface = lib.mkOption {
      type = lib.types.str;
      default = "enp7s0";
      description = ''
        The NIC carrying `privateAddress`. `enp7s0` is what these hosts actually present the second,
        network-attached card as -- read off k3s-worker-1 on 2026-08-29, running NixOS 26.05 with
        kernel 6.18.

        IT WAS `ens10` UNTIL THAT WAS CHECKED, AND THE COMMENT BELOW IS WHY THAT MATTERED. Every
        private rule on the first converted host was installed against an interface that does not
        exist: `iptables -S` listed `-A nixos-fw -i ens10 --dport 9100 -j nixos-fw-accept` and read
        perfectly, while the default policy quietly dropped every Prometheus scrape and every k3s
        join. Nothing failed; it simply did not work. Older Hetzner images do present ens3/ens10,
        which is where the wrong value came from -- so do not assume either name, CHECK
        (`ip -brief addr`), and note that ./firewall.nix now installs a unit that checks for you.

        NAMED HERE RATHER THAN IN EACH RULE, and the reason is a trap bitcashier paid for twice:
        iptables ACCEPTS a rule naming an interface that does not exist. It installs, it reads
        correctly in `iptables -S`, and it never matches -- so a firewall scoped to a misspelled or
        renamed interface opens nothing and reports nothing. One option, read by ./firewall.nix, is
        one place for that to be wrong.
      '';
    };

    publicInterface = lib.mkOption {
      type = lib.types.str;
      default = "enp1s0";
      description = ''
        The NIC carrying the public address. Recorded for completeness and for the same
        rename-safety reason as `privateInterface`; note that ./firewall.nix deliberately scopes
        nothing by this name -- see the note there about why the public rules are unqualified.
      '';
    };

    nodeExporterPort = lib.mkOption {
      type = lib.types.port;
      default = 9100;
      description = ''
        node_exporter's own upstream default, and this fleet keeps it.

        WORTH A SENTENCE ONLY BECAUSE THE PORT THIS WAS PORTED FROM IS NOT 9100. bitcashier runs it
        on 9101 and has an incident behind that number: Prometheus there discovers targets through
        the Hetzner API and relabels every server to `<private ipv4>:9101`, so a host on 9100 was
        not a missing target but a permanently-down one, and it scraped `up == 0` for weeks while
        looking configured. Here Prometheus is a static scrape config on monitoring-1 and the port
        is written in exactly two places -- this option and that config -- so 9100 is safe and is
        what infra/terraform/firewall.tf already documents as the private-network port.
      '';
    };
  };

  config = {

    # ---------------------------------------------------------------------------------------------
    # Boot and platform
    # ---------------------------------------------------------------------------------------------

    # GRUB IN BIOS MODE, because Hetzner Cloud servers boot BIOS. systemd-boot is UEFI-only and
    # simply never runs on this hardware, which fails as a machine that installs cleanly and then
    # never boots -- the most expensive shape of failure available during a bootstrap, because the
    # only tool left is the Hetzner console.
    #
    # The device list is NOT set here. disko contributes it per host, and `devices` is a list that
    # MERGES: a fleet-wide literal plus a host's own entry gives grub the same disk twice and trips
    # its own "duplicated devices in mirroredBoots" assertion. bitcashier resolves this with a
    # `mkForce` reading a `fleet.osDisks` option; with three homogeneous cloud servers and no bare
    # metal, saying nothing here and letting each host's disko config speak is simpler and has the
    # same effect.
    boot.loader.systemd-boot.enable = false;
    boot.loader.grub = {
      enable = true;
      efiSupport = lib.mkDefault false;
    };

    # Hetzner Cloud presents its disks over virtio-scsi. Without these in the initrd the machine
    # installs cleanly and then cannot find its root device.
    boot.initrd.availableKernelModules =
      [ "ahci" "xhci_pci" "virtio_pci" "virtio_scsi" "sd_mod" "sr_mod" ];

    services.qemuGuest.enable = true;

    # PERSISTENT JOURNAL, so the next failed boot leaves evidence. A volatile journal dies with the
    # boot that would have explained itself, and with no log shipping on this fleet (see the header)
    # this is the ONLY place an incident's evidence survives a reboot.
    services.journald.storage = "persistent";

    # ---------------------------------------------------------------------------------------------
    # Networking
    # ---------------------------------------------------------------------------------------------
    #
    # DHCP on both interfaces, which is what Hetzner Cloud serves on the public NIC and on the
    # private one alike. The private address is HANDED OUT by the cloud network (terraform pins it
    # per server), so `fleet.privateAddress` describes what DHCP will deliver rather than
    # configuring it -- deliberately, because a hand-written static address here that disagreed
    # with terraform would come up unreachable and there is no console-free way back from that.
    networking.useNetworkd = true;
    networking.useDHCP = lib.mkDefault true;
    services.resolved.enable = true;

    time.timeZone = "UTC";

    # UTC AND en_US.UTF-8 ARE BOTH STATED RATHER THAN INHERITED, and neither line changes anything
    # today: nixpkgs already defaults `i18n.defaultLocale` to en_US.UTF-8. The point of writing
    # them down is that a nixpkgs bump moving either becomes a DIFF rather than a discovery.
    #
    # It is not cosmetic. The locale decides collation -- so `sort` and every pipeline built on it
    # order differently under C than under en_US -- decimal separators in tool output, date
    # rendering, and the language of program messages, which is what makes a log line match or stop
    # matching a grep somebody wrote against a different machine. Three hosts that agree with each
    # other and with the operator's laptop is worth more than which of the two is chosen.
    i18n.defaultLocale = "en_US.UTF-8";

    # ---------------------------------------------------------------------------------------------
    # SSH
    # ---------------------------------------------------------------------------------------------
    #
    # ssh.SERVICE, not ssh.socket, and `startWhenNeeded = false` is what pins that. Socket
    # activation is what produced a fleet-wide lockout on bitcashier's Ubuntu 24.04 hosts: a
    # restart under an active socket unit left the daemon unable to re-acquire its runtime
    # directory, on machines with no console. NixOS activates it as a plain service by default and
    # this states the default we are relying on.
    #
    # WHERE THIS DIVERGES FROM THE PORT, DELIBERATELY: bitcashier binds sshd's ListenAddress to the
    # PRIVATE address only, because it has an administrative path on the private network and a
    # self-hosted runner sitting on it. This fleet has neither. nixos-anywhere, colmena, the GitHub
    # Actions staging job and the operator all arrive over the public NIC, so sshd binds the
    # wildcard and the narrowing is done by key-only authentication plus the Hetzner edge firewall
    # (infra/terraform/firewall.tf, port 22 from anywhere, by explicit decision recorded there).
    # If a private administrative path ever exists here, this is the line to reconsider first.
    services.openssh = {
      enable = true;
      startWhenNeeded = false;
      settings = {
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
        # `prohibit-password`, NOT `no`. Root by KEY is how nixos-anywhere installs this machine and
        # how colmena activates on it; root by PASSWORD is impossible anyway, since ./accounts.nix
        # sets `hashedPassword = "!"`. See that file for why the break-glass path has to exist.
        PermitRootLogin = lib.mkDefault "prohibit-password";
        X11Forwarding = false;
      };
    };

    # ---------------------------------------------------------------------------------------------
    # Nix itself
    # ---------------------------------------------------------------------------------------------

    nix.settings = {
      # Flakes, because the whole tree is one. Both features are needed: `flakes` without
      # `nix-command` gives you a flake nothing can evaluate from the command line, which is how a
      # host ends up debuggable only through a rebuild.
      experimental-features = [ "nix-command" "flakes" ];

      # WHO MAY INSERT ARBITRARY PATHS INTO THIS STORE, and the CI account is deliberately not in
      # the list. This is the load-bearing half of the staging design in ./deploy-staging.nix: a
      # trusted nix user can add ANY store path, unsigned, which is a route to root the moment
      # anything activates it. `nixdeploy` is therefore untrusted, and what lets its paths in
      # instead is that CI SIGNS them and the host lists the public half in `trusted-public-keys`.
      # So the key can deliver exactly the closures CI built and nothing it invents.
      #
      # `@wheel` is this fleet's admin group and those accounts already have passwordless sudo
      # (./accounts.nix), so naming it grants nobody anything they did not already have.
      trusted-users = [ "root" "@wheel" ];
    };

    # ---------------------------------------------------------------------------------------------
    # Packages present on every host
    # ---------------------------------------------------------------------------------------------
    #
    # Deliberately short: what an operator reaches for on a machine that has ALREADY gone wrong.
    # Anything a service needs belongs to that service's module, where it is visible as a
    # dependency rather than as an ambient convenience.
    #
    # `python3` IS NOT A CONVENIENCE. It is what lets anything Ansible-shaped manage this host at
    # all -- every Ansible module except `raw` needs an interpreter on the target, and NixOS has
    # none by default: no /usr/bin/python3, and nothing called python3 on PATH unless it is in this
    # list. bitcashier lost an afternoon to that, because the error Ansible prints for a missing
    # interpreter is the same sentence it prints for a module whose output would not parse, so it
    # reads as a broken play rather than a missing binary.
    environment.systemPackages = with pkgs; [
      curl dnsutils file git htop jq lsof python3 rsync sysstat tcpdump tree vim
    ];

    # UNATTENDED UPGRADES HAVE NO COUNTERPART AND THAT IS THE POINT. Patching happens by moving the
    # nixpkgs pin in this repository and letting ./auto-apply.nix activate what CI staged -- which
    # is reviewable, revertible and gated on disturbing nothing. None of those three is true of a
    # machine that rewrites itself at 06:00.
    system.autoUpgrade.enable = false;

    # ---------------------------------------------------------------------------------------------
    # Staging and auto-apply
    # ---------------------------------------------------------------------------------------------
    #
    # ACTIVATING WHAT CI STAGED IS ON BY DEFAULT, and it is set here rather than per host for the
    # reason the mechanism exists: a roster somebody has to remember to add a machine to is exactly
    # the failure being fixed, and it would be a poor thing to reintroduce in the wiring.
    #
    # bitcashier had this per host and the per-host list is what went wrong: eleven of thirteen
    # hosts enabled it, one of the two that did not was a deliberate decision recorded in a commit
    # message, and the other was an omission created by a branch that ran in parallel with the one
    # turning it on. Nothing in the tree distinguished the decision from the accident. A default is
    # what makes the accident impossible and forces the decision to be written down.
    #
    # `mkDefault`, so a host that genuinely must not activate unattended says
    # `fleet.autoApply.enable = false;` in its OWN file, beside `fleet.autoApply.excludedBecause`,
    # where somebody reading that machine's description sees both the exemption and the sentence
    # justifying it.
    fleet.autoApply.enable = lib.mkDefault true;

    # WHICH ENTRY OF `nixosConfigurations` THIS HOST IS, defaulted from its own hostname rather than
    # restated per host -- because on this fleet the three names are the same string BY
    # CONSTRUCTION: the directory under nix/hosts/, the `networking.hostName` set inside it, and the
    # attribute `flake.nix` binds it to are all `mongo-1` / `monitoring-1` / `k3s-worker-1`, and
    # flake.nix derives the last two from the first (see `hostModules`).
    #
    # STATING IT THREE TIMES WOULD NOT MAKE IT SAFER, IT WOULD MAKE IT BREAKABLE. The failure this
    # avoids is a host whose `attribute` names a DIFFERENT host: auto-apply would then pull that
    # other machine's staged closure every 30 minutes and activate it, which on this fleet means the
    # k3s worker quietly becoming a second monitoring node. A default that cannot disagree with the
    # hostname cannot express that.
    #
    # `mkDefault`, so a host that genuinely needs to diverge -- a rename mid-migration, where the
    # machine still answers to the old name while the flake already carries the new one -- can say
    # so in its own file, which is the one place a reader would look for it.
    fleet.autoApply.attribute = lib.mkDefault config.networking.hostName;

    # THE CI KEYPAIRS, minted 2026-08-29. Both halves below are PUBLIC and belong in the repository;
    # their private halves live in the repo-root .env.local and as GitHub Actions secrets, and
    # nowhere else.
    #
    # WHY THERE ARE TWO OF THEM, because one would look like enough: they answer different
    # questions, and collapsing them would give CI a power it must not have.
    #
    #   authorizedKey    WHO MAY CONNECT. The public half of an ssh keypair CI holds. It is pinned
    #                    to a forced command in deploy-staging.nix, so a holder of the private half
    #                    can push a closure into the pin directory and do NOTHING else -- no shell,
    #                    no activation, no other command. That restriction is the whole security
    #                    model of letting a GitHub runner reach production at all.
    #
    #   trustedPublicKeys  WHAT MAY BE INSTALLED. The public half of a nix signing keypair. A host
    #                    refuses to activate a store path this key has not signed, so even an
    #                    attacker who obtained the ssh key above could only stage closures the host
    #                    would then decline to run.
    #
    # ROTATING EITHER IS A TWO-STEP: add the new public half here and deploy it to every host FIRST,
    # then switch CI to the new private half. Doing it in the other order locks CI out of a fleet
    # that no longer trusts it, and the recovery is a manual colmena deploy to all three hosts.
    fleet.deployStaging.authorizedKey = lib.mkDefault
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINFG71ktZWOCBDbipR7AYcr+bWTJ09qwB0chXDLP9Cx9 nixdeploy@kinowo-ci";

    fleet.deployStaging.trustedPublicKeys = lib.mkDefault [
      "kinowo-ci-1:Zn/H+cOSVYLLJRSvBVJLbr86cjWnaMujLr0hm9rEl1Y="
    ];

    # THE FLEET'S DISTURBANCE POLICY, and it is the CONSERVATIVE end of the range the ported module
    # supports. Both allow-lists are left at their defaults -- EMPTY -- so a staged closure that
    # would stop, start, restart or reload a single unit is refused and left for a person. What
    # auto-apply takes on its own here is changing files and nothing else.
    #
    # WHY NOT bitcashier's `[ "*" ]` FOR BOTH. That fleet widened to `*` because its gate had gone
    # inert: essentially every closure there touched consul, unbound, promtail or an exporter, so
    # nothing ever passed and six of ten hosts drifted 171 commits behind while the gate reported
    # itself working. That is a real failure mode and this fleet may well meet it -- three hosts
    # running one service each churn far fewer units, but a nixpkgs bump still restarts half the
    # machine. THE RIGHT MOMENT TO WIDEN IS WHEN THE METRIC SAYS SO: watch
    # `nixos_auto_apply_blocked_reason{reason="units_would_change"}` for a fortnight, and widen to
    # the units it actually names rather than to `*` on the strength of this paragraph.
    #
    # A plain list rather than `mkDefault` for the deny-list below, so a host's own definition
    # MERGES with this one by concatenation instead of replacing it: raising the floor is one line
    # in a host file, and LOWERING it takes `lib.mkForce` and is therefore visible in review. That
    # asymmetry is the right direction of travel for the only list here whose failure mode is an
    # outage rather than an inconvenience.
    # WITHOUT THIS, AUTO-APPLY NEVER ACTIVATES ANYTHING, AND IT LOOKS HEALTHY WHILE NOT DOING SO.
    #
    # `dbus-broker.service.d/overrides.conf` differs between ANY two closures on this fleet -- it
    # carries an X-Restart-Triggers value derived from the system closure, so it changes whenever
    # anything else does. With both allow-lists empty (default deny) that one unit made every pass
    # answer `blocked: units_would_change`, on all three hosts, for every commit. The timer ran, the
    # verdict was published, the metric said "blocked" rather than "failed" -- so nothing was red,
    # and the fleet would simply have stopped tracking main for ever.
    #
    # A RELOAD, NOT A RESTART, WHICH IS WHY IT IS ON THIS LIST AND NOT THE OTHER ONE. Every
    # `nixos-rebuild switch` run against these hosts on 2026-08-29 reported exactly
    # "reloading the following units: dbus-broker.service" and left zero failed units behind, across
    # more than a dozen deploys. dbus-broker re-reads its configuration in place; existing
    # connections are not dropped.
    #
    # IT IS ONE NAMED UNIT, NOT `[ "*" ]`. bitcashier arrived at the wildcard; that is a much larger
    # claim -- that no unit on the host is worth pausing for -- and nothing here has earned it. The
    # evidence above covers dbus-broker and dbus-broker alone.
    fleet.autoApply.reloadableUnits = [ "dbus-broker.service" ];

    fleet.autoApply.neverDisturbUnits = [
      # THE RULE THESE FOLLOW: a unit may be restarted unattended UNLESS a restart is visible to
      # somebody outside this fleet, or unless losing it removes our own way back in. Everything
      # NOT named here stays restartable the day the allow-lists are widened -- prometheus,
      # grafana, alertmanager, node_exporter, the timers. Bouncing those costs a gap in a graph
      # that nobody outside sees, and forgiving them is most of the drift problem solved.
      "sshd.service" # the way back in; a restart that refuses every key still "succeeds"
      "mongodb.service" # the read model's only durable store, and every worker write
      "k3s.service" # the server on monitoring-1; a bounce takes the cluster's API with it
    ];

    services.chrony.enable = true;

    # ---------------------------------------------------------------------------------------------
    # WHICH MACHINE AM I ON
    # ---------------------------------------------------------------------------------------------
    #
    # Three hosts is few enough to hold in your head and exactly few enough to confuse: two of them
    # are `cx23` in nbg1 with adjacent private addresses. An operator who cannot tell two shells
    # apart is how a `systemctl stop` lands on the wrong machine.
    users.motd = ''

      role:        ${config.fleet.role}
      environment: ${config.fleet.environment}
      private ip:  ${config.fleet.privateAddress}
      os:          NixOS -- this machine is declared in infra/nix, not configured by hand

    '';

    # MAKE THE RUNNING HOSTNAME MATCH THE CONFIGURED ONE, ON EVERY SWITCH AND NOT ONLY ON BOOT.
    #
    # `networking.hostName` writes /etc/hostname and the /etc/hosts entry, but a `nixos-rebuild
    # switch` does NOT change the live kernel hostname -- systemd applies that at boot. So between
    # a deploy and the next reboot the machine answers to whatever image it came up on while every
    # file on it says otherwise.
    #
    # THAT IS NOT COSMETIC. On bitcashier a host converted from a generic image kept the running
    # name `nixos-agent-unconfigured`, which appears in /etc/hosts under NO address at all -- only
    # the configured name is mapped -- and a JVM service died on startup inside
    # `InetAddress.getLocalHost()` with `UnknownHostException`. Anything that resolves its own name
    # (a metric label, a cluster member id, a log field) fails the same way, and it fails while the
    # process reports itself healthy, which is the shape that takes longest to attribute. k3s and
    # mongod both do this.
    #
    # Idempotent by construction, and deliberately NOT a reboot: a hostname is not worth restarting
    # a machine over.
    system.activationScripts.fleetHostname = ''
      current=$(${pkgs.nettools}/bin/hostname)
      if [ "$current" != "${config.networking.hostName}" ]; then
        echo "fleet: running hostname is '$current', configuration says " \
             "'${config.networking.hostName}' -- setting it"
        ${pkgs.nettools}/bin/hostname "${config.networking.hostName}"
      fi
    '';

    # `nixos-version` and the closure are the record of what this machine is. Pinned to the release
    # these hosts were first built against (nixos-26.05); changing it is a MIGRATION with its own
    # release notes to read, not a version bump that rides along with a nixpkgs update.
    system.stateVersion = "26.05";
  };
}
