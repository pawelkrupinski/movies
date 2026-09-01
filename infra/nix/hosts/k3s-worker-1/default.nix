{ ... }:
{
  imports = [
    ./disko.nix
    ../../modules/roles/k3s-agent.nix
    ../../modules/roles/public-proxy.nix

    # See the note on the same import in hosts/monitoring-1: this belongs in
    # modules/fleet/default.nix's `imports`, and is repeated per host only because that list could
    # not be edited in the change that added it.
    ../../modules/fleet/logs.nix
  ];

  networking.hostName = "k3s-worker-1";

  fleet = {
    role = "k3s-worker";
    privateAddress = "10.20.0.12";

    # THE ADDRESS CI COPIES A CLOSURE TO, and the reason it is stated rather than inferred: this
    # fleet has no jump host, so every path onto these machines -- colmena, nixos-anywhere, the
    # staging workflow -- arrives on the public NIC. bin/stage-nixos-closures treats an empty
    # `publicAddress` as `unreachable-by-declaration` and FAILS rather than skipping, which is the
    # right direction (a host nobody can stage to silently stops tracking main) and is exactly what
    # it did on the first run after this landed on main.
    #
    # Stable because terraform/primary_ips.tf pins it with `auto_delete = false`; it is
    # `k3s_worker_1_ipv4` there.
    publicAddress = "2.28.47.31";
  };

  # THE ONLY HOST IN fsn1, and the only one with no `fileSystems` entry beyond disko's root -- see
  # ./disko.nix for why it holds no state. It is also the only host with Hetzner's daily backups
  # switched OFF (in ../../../terraform/server.k3s-worker.tf), which is the same decision stated in
  # the other direction.

  # JOINS THE CONTROL PLANE ON monitoring-1, over the private network. Both hosts are in Hetzner's
  # `eu-central` network zone despite sitting in different datacentres (fsn1 here, nbg1 there), so
  # 10.20.0.11 is directly reachable with no peering and no routes -- see terraform/network.tf. The
  # ~4.5ms between Falkenstein and Nuremberg is paid by kubelet heartbeats, image pulls and every
  # mongo-1 query the app pods make on the request path -- which is why this machine was moved out
  # of hel1, where the same link measured 24ms. See the note in terraform/server.k3s-worker.tf.
  fleet.k3sAgent = {
    enable = true;
    serverAddr = "https://10.20.0.11:6443";
  };

  fleet.firewall.k3sAgent = true;

  # SHIP THE JOURNAL *AND* THE POD LOGS TO monitoring-1. The second half is the point on this host:
  # containerd writes container stdout/stderr to /var/log/pods and NOT to the journal, so a shipper
  # with only the journal source would send k3s's own units and nothing the cluster runs -- which is
  # the whole reason this machine exists. bitcashier hit exactly that gap when its workloads moved
  # to Kubernetes and nothing replaced the old alloc-log scrape; nothing alerted, because a stream
  # that has stopped looks the same as one nobody has asked about.
  #
  # This host holds no state (see ./disko.nix) -- vector's checkpoint and buffer under /var/lib are
  # the one exception, and both are disposable: losing them costs a re-ship, not data.
  #
  # See the note in hosts/mongo-1 for why the address is a literal here rather than read off
  # monitoring-1's own declaration.
  fleet.logs = {
    enable = true;
    serverAddress = "10.20.0.11";
    kubernetesPodLogs.enable = true;
  };

  # THE PUBLIC FACE OF THE PRODUCT, and the reason this host — not monitoring-1 — terminates it: the
  # web pods run HERE (nodeSelector pins them to this node), so every vhost below proxies to a
  # NodePort on loopback. Putting these on monitoring-1's Caddy instead would send every user request
  # nbg1 → hel1 across the private network and make the monitoring box a single point of failure for
  # the product, which is precisely backwards.
  #
  # WHY NOT AN INGRESS CONTROLLER. k3s ships with traefik and servelb disabled here
  # (roles/k3s-server.nix), so a Kubernetes-native answer means adding an ingress controller AND
  # cert-manager AND a LoadBalancer story, to do what Caddy already does on this host with automatic
  # ACME and no renewal timer to forget.
  #
  # EVERY NAME BELOW MUST RESOLVE TO 2.28.47.31 BEFORE A DEPLOY, or Let's Encrypt's HTTP-01
  # challenge fails and Caddy serves a self-signed certificate — which browsers reject outright, so
  # the failure mode is a hard TLS error rather than a degraded page. The A records live at OVH.
  fleet.publicProxy = {
    enable = true;
    acmeEmail = "pawel@bitcashier.io";
    vhosts = {
      # Poland, on its own domain and its own brand. Mounted at the root, so its URLs are the ones
      # it has always served, byte for byte.
      "kinowo.net".upstream = "127.0.0.1:30910";
      "www.kinowo.net".redirectTo = "kinowo.net";

      # THE SHOWTIMES COUNTRIES SHARE ONE DOMAIN AND ARE TOLD APART BY A PATH SEGMENT.
      #
      # They used to have a subdomain each (uk./de./us.showtimes.cc). Those names now serve
      # NOTHING — no vhost, no certificate, no redirect, deliberately: a redirect map is a second
      # source of truth for where each country lives, and the apps are store-release-gated anyway,
      # so the cut is clean rather than half-migrated.
      #
      # Each prefix still reaches its OWN pod against its OWN database (KINOWO_COUNTRY per overlay
      # in infra/kubernetes/web/overlays); one pod serving four countries would mean one process
      # against four databases. The app mounts itself at the matching prefix
      # (`play.http.context`, derived from models.Country.mountPath) so every URL it emits —
      # reverse routes, canonical link, og:url, sitemap, cookie paths — carries the segment, and
      # nothing here rewrites paths.
      #
      # THE BARE APEX IS THE BRAND FRONT DOOR, not a deployment of its own: the app renders a
      # country picker (Poland included) when the request Host is the apex AND the deployment
      # answering is mounted at `/` — see models.Country.servesApex. That is why the fallback
      # points at POLAND's pod rather than the UK's: it is the only one whose `/` is not already a
      # country's own landing. The picker is rendered in English regardless of which deployment
      # serves it, so nothing about that pick is Polish.
      #
      # The fallback also carries the apex ROOT files a crawler and the mobile OSes only ever fetch
      # from a host's root — /robots.txt, /sitemap.xml, /.well-known/* — which the app answers with
      # front-door variants (a sitemap INDEX of the three mounted countries, not Poland's cities).
      "showtimes.cc" = {
        upstream = "127.0.0.1:30910";
        pathUpstreams = {
          "/uk" = "127.0.0.1:30912";
          "/de" = "127.0.0.1:30911";
          "/us" = "127.0.0.1:30913";
        };
      };
      "www.showtimes.cc".redirectTo = "showtimes.cc";
    };
  };

  sops.defaultSopsFile = ../../secrets/k3s-worker-1.yaml;

  system.stateVersion = "26.05";
}
