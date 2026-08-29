{ config, ... }:

let
  # Allocated by Terraform; read it back with
  # `terraform -chdir=infra/terraform output -json hosts | jq -r '."monitoring-1".volumes."monitoring-data"'`.
  # See the long note in ../mongo-1/default.nix for why a wrong id here is worse than a build
  # failure -- the same reasoning applies, with Prometheus filling the root disk in mongod's place.
  volumeDevice = "/dev/disk/by-id/scsi-0HC_Volume_106735107";
in
{
  imports = [
    ./disko.nix
    ../../modules/roles/public-proxy.nix
    ../../modules/roles/k8s-deploy.nix
    ../../modules/roles/wireguard-fly.nix
    ../../modules/roles/prometheus.nix
    ../../modules/roles/grafana.nix
    ../../modules/roles/k3s-server.nix
    ../../modules/roles/victoria-logs.nix

    # THE SHIPPER, IMPORTED PER HOST -- WHICH IS THE WRONG PLACE FOR IT, AND IS SAID OUT LOUD HERE
    # RATHER THAN LEFT TO BE DISCOVERED. It lives under modules/fleet/ because it is true of every
    # machine, and on bitcashier the equivalent file sits in fleet/default.nix's `imports` so that
    # a host CANNOT be built without it. That list could not be edited in the change that added
    # this. The consequence is concrete: a fourth host added to this fleet gets a firewall, a
    # node_exporter and a persistent journal automatically, and ships its logs NOWHERE until
    # somebody remembers this line. Moving `./logs.nix` into modules/fleet/default.nix's imports and
    # deleting the three copies of it is the follow-up.
    ../../modules/fleet/logs.nix
  ];

  networking.hostName = "monitoring-1";

  fleet = {
    role = "monitoring";
    privateAddress = "10.20.0.11";

    # THE ADDRESS CI COPIES A CLOSURE TO, and the reason it is stated rather than inferred: this
    # fleet has no jump host, so every path onto these machines -- colmena, nixos-anywhere, the
    # staging workflow -- arrives on the public NIC. bin/stage-nixos-closures treats an empty
    # `publicAddress` as `unreachable-by-declaration` and FAILS rather than skipping, which is the
    # right direction (a host nobody can stage to silently stops tracking main) and is exactly what
    # it did on the first run after this landed on main.
    #
    # Stable because terraform/primary_ips.tf pins it with `auto_delete = false`; it is
    # `monitoring_1_ipv4` there.
    publicAddress = "2.28.52.210";
  };

  # No `nofail`, for the reason in the assertion above: a monitoring host that boots without its
  # metrics disk is worse than one that does not boot, because it looks fine.
  fileSystems."/var/lib/monitoring" = {
    device = volumeDevice;
    fsType = "ext4";
    options = [ "defaults" ];
  };

  # PROMETHEUS AND ALERTMANAGER. `nodeTargets` is written out rather than derived from the Hetzner
  # API on purpose: a scrape list that discovers its own targets cannot tell "this host was
  # decommissioned" from "this host is unreachable", and the second is the case monitoring exists
  # for. A host that disappears from this list does so because somebody edited it.
  fleet.prometheus = {
    enable = true;
    nodeTargets = [
      { address = "10.20.0.10"; host = "mongo-1"; role = "mongodb"; }
      { address = "10.20.0.11"; host = "monitoring-1"; role = "monitoring"; }
      { address = "10.20.0.12"; host = "k3s-worker-1"; role = "k3s-worker"; }
    ];
  };

  fleet.grafana = {
    enable = true;

    # MUST MATCH THE PUBLIC NAME. Grafana builds redirects, OAuth callbacks and the links in alert
    # notifications from root_url, so behind a proxy a wrong value here does not fail loudly -- it
    # sends people to http://localhost:3000, from an email, and looks like the alert is broken.
    rootUrl = "https://grafana.kinowo.net/";
  };

  # PUBLIC HTTPS FOR GRAFANA ONLY. See roles/public-proxy.nix for why nothing else is published:
  # Grafana is the only service here that authenticates its own users, and its login IS the
  # security boundary -- the proxy adds TLS and a name, not authentication.
  fleet.publicProxy = {
    enable = true;
    acmeEmail = "pawel@bitcashier.io";
    vhosts = {
      # A NAME THIS PROJECT OWNS, at last. The sslip.io spelling below was only ever a stand-in for
      # not having a domain (see roles/public-proxy.nix), and it carried a real cost: sslip.io is a
      # SHARED registered domain, so Let's Encrypt's per-domain rate limit is consumed by everyone
      # using it and an issuance here could fail for reasons that have nothing to do with this fleet.
      "grafana.kinowo.net".upstream = "10.20.0.11:3000";

      # KEPT SERVING, not redirected, and deliberately so. It is what every existing bookmark and
      # every alert notification sent before this change points at, and making it a redirect would
      # couple THIS host's only public service to a DNS record that may not have propagated yet --
      # if grafana.kinowo.net does not resolve, a redirect takes Grafana away, and the monitoring
      # box is exactly the thing you need reachable when something is wrong. Turn it into a
      # `redirectTo` once the new name has been answering for a while.
      "grafana.2-28-52-210.sslip.io".upstream = "10.20.0.11:3000";
    };
  };

  # THE FLEET'S LOG STORE, beside the metrics store and for the same reason: this is the box you go
  # to when you want to know what happened, and there is no second one. It writes to the monitoring
  # volume (/var/lib/monitoring/victoria-logs) alongside the TSDB and Grafana's sqlite -- see
  # roles/victoria-logs.nix for the arithmetic that lets three things share a 40GB disk, and for why
  # its bound is 10GiB rather than "whatever is left".
  fleet.victoriaLogs.enable = true;

  # AND IT SHIPS ITS OWN JOURNAL TO ITSELF. Not redundant: without this, the one host whose logs
  # would explain a monitoring outage is the one host missing from the store, and every query that
  # sweeps the fleet would silently cover two machines out of three.
  #
  # NO POD LOGS HERE. This node runs the k3s control plane with `schedulable = false`, so the only
  # containers on it are the control plane's own -- and k3s logs those through its own systemd
  # units, which the journal source already covers.
  fleet.logs = {
    enable = true;

    # ITSELF, read from its own declaration above rather than written out again -- a second literal
    # is a second thing to get wrong on the day this address changes. The other two hosts cannot do
    # this and carry the literal; see the option's own note for the flake.nix wiring that would fix
    # all three at once.
    serverAddress = config.fleet.privateAddress;
  };

  # THE k3s CONTROL PLANE, ON THE SAME BOX, per the fleet design. `clusterInit` because this is the
  # first and only server; a second would join against it rather than repeat this.
  #
  # `schedulable = false` so the control plane does not also run workloads. On a 2-core box shared
  # with Prometheus and Grafana that is not a stylistic preference: a pod that pins both cores would
  # otherwise starve the apiserver AND the metrics that would explain why. k3s-worker-1 is a cx43
  # with eight cores and nothing else on it, and is where work belongs.
  fleet.k3sServer = {
    enable = true;
    clusterInit = true;
    schedulable = false;
  };

  # A SECOND PEER INTO FLY'S 6PN, WHOSE ONE JOB NO LONGER EXISTS.
  #
  # THE PROBLEM IT SOLVED. `kinowo` and its `/metrics` used to run on Fly, and the obvious way to
  # see them -- Fly's managed Prometheus -- is unavailable: both read-only tokens for it are
  # revoked, and the only remaining Fly token is org-wide and deploy-capable, which must not sit on
  # the host that also runs the k3s control plane. So `fleet.prometheus.scrapeFly` is off and the
  # Fly panels have no data. The way round it needed no Fly token at all: the app published its own
  # `/metrics` on port 9000 over Fly's private network, so this peer scraped it DIRECTLY, on a
  # WireGuard key this fleet holds rather than a token somebody else issues.
  #
  # WHAT IS TRUE SINCE THE 2026-08-29 CUTOVER. The web tier moved to k3s beside the workers
  # (infra/kubernetes/web/, docs/domain-cutover.md), Prometheus now reaches BOTH tiers over
  # NodePorts on the Hetzner private network, and the 6PN DNS-discovery job that resolved
  # `kinowo.internal` through this tunnel was deleted with the move -- see
  # infra/nix/files/monitoring/scrape-kinowo-apps.yaml. Nothing of the product runs on Fly any more,
  # so nothing on this host routinely sends a packet down this tunnel.
  #
  # WHICH MAKES THE PEER A CANDIDATE FOR REMOVAL, left in place deliberately rather than by
  # oversight: it is the only pre-built route from the monitoring box into Fly's private network
  # while the Fly org is being wound down, and it costs one interface and one keypair. If nothing
  # has needed it by the time the org goes, delete it here and revoke it with `fly wireguard
  # remove` -- and note that the wireguard-fly alert rules then have one fewer host to watch.
  #
  # SEPARATE PEER FROM mongo-1's, deliberately: `fly wireguard create` mints one keypair per peer,
  # and sharing one across two hosts would mean two machines using one identity -- so revoking
  # either means revoking both, and Fly's peer list would name a machine rather than a role.
  fleet.wireguardFly = {
    enable = true;
    address = "fdaa:74:b6b5:a7b:35c:20a7:c070:5c02/120";
    peerPublicKey = "tyYPi0DmwNDs3YEhnm4CeNy5I9m2QSsdry4H46Zfr3M=";
    peerEndpoint = "arn1.gateway.6pn.dev:51820";
    allowedIPs = [ "fdaa:74:b6b5::/48" ];
  };

  fleet.firewall.monitoring = true;
  # HOW CI ROLLS THE WORKER OUT. A key pinned to a forced command that accepts one validated image
  # reference and updates one container -- see roles/k8s-deploy.nix for why CI is not simply given a
  # kubeconfig (k3s writes exactly one, and it is cluster-admin).
  #
  # The public half is here; the private half is a GitHub Actions secret and is in .env.local.
  fleet.k8sDeploy = {
    enable = true;
    authorizedKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIg+/1urv1bMUdFd3yRyrb6SgrOz5f7cjJdM7H4sDUuQ k8sdeploy@kinowo-ci";
  };

  fleet.firewall.k3sServer = true;

  sops.defaultSopsFile = ../../secrets/monitoring-1.yaml;

  system.stateVersion = "26.05";
}
