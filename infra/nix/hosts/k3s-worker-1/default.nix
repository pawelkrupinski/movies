{ config, ... }:
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

  # JOINS THE CONTROL PLANE ON monitoring-1, over the private network -- now the SAME datacentre
  # (both fsn1 since 2026-09-01), so 10.20.0.11 is a local hop. It did not have to be: every host
  # here is in Hetzner's `eu-central` network zone, which is the unit a cloud subnet is scoped to, so
  # this join worked unchanged when the two were 24ms apart in hel1 and nbg1 -- see
  # terraform/network.tf. What is NOT local is mongo-1, still in nbg1 at ~4.5ms, and that link is on
  # the app pods' request path; it is the reason both machines moved here. See the note in
  # terraform/server.k3s-worker.tf.
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
  # WHO GETS 429ed ON THE FACETED LISTINGS, shared by both product domains because the policy is
  # about the crawler rather than the brand.
  #
  # `meta-externalagent` is Meta's AI indexing crawler, and it is here on evidence rather than on
  # principle. Over the retained showtimes.cc access logs it made 404,087 requests, 74.3% of them
  # to `/{cc}/{city}/movies?cast=…` -- a path robots.txt has disallowed the whole time -- and it
  # fetched /robots.txt ZERO times in that window. It was 99.4% of all traffic to the domain.
  #
  # Not `facebookexternalhit`: that is Meta's SHARE-PREVIEW agent, it does read robots.txt, and in
  # the same logs it made 131 requests, all to city pages and og-image assets. Throttling it would
  # break the Facebook/WhatsApp/Messenger link previews the og-image endpoints exist for.
  fleet.publicProxy = let
    facetThrottle = {
      userAgents = [ "meta-externalagent" ];
    };
    # ⚠️ ONLY THE PROXIED NAMES. A Cloudflare Origin certificate is trusted by Cloudflare and by
    # nothing else, so putting one on a name a browser reaches directly hands every visitor a
    # certificate they reject. All four vhosts here are behind Cloudflare; `grafana` and `headlamp`
    # are dns-only and live on monitoring-1, so they are not reachable from this list at all.
    kinowoOrigin = {
      certFile = ../../files/origin-certs/kinowo.net.crt;
      keyFile  = config.sops.secrets."origin-tls/kinowo_net".path;
    };
    showtimesOrigin = {
      certFile = ../../files/origin-certs/showtimes.cc.crt;
      keyFile  = config.sops.secrets."origin-tls/showtimes_cc".path;
    };
  in {
    enable = true;
    acmeEmail = "pawel@bitcashier.io";
    vhosts = {
      # Poland, on its own domain and its own brand. Mounted at the root, so its URLs are the ones
      # it has always served, byte for byte.
      "kinowo.net" = {
        upstream = "127.0.0.1:30910";
        crawlerThrottle = facetThrottle;
        originCertificate = kinowoOrigin;
      };
      "www.kinowo.net" = { redirectTo = "kinowo.net"; originCertificate = kinowoOrigin; };

      # THE SHOWTIMES COUNTRIES SHARE ONE DOMAIN AND ARE TOLD APART BY A PATH SEGMENT.
      #
      # They used to have a subdomain each (uk./de./us.showtimes.cc). Those names now serve
      # NOTHING — no vhost, no certificate, no redirect, deliberately: a redirect map is a second
      # source of truth for where each country lives, and the apps are store-release-gated anyway,
      # so the cut is clean rather than half-migrated.
      #
      # Each prefix still reaches its OWN pod against its OWN database (KINOWO_COUNTRY per overlay
      # in movies-gitops/web/overlays); one pod serving four countries would mean one process
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
          "/es" = "127.0.0.1:30914";
        };
        crawlerThrottle = facetThrottle;
        originCertificate = showtimesOrigin;
      };
      "www.showtimes.cc" = { redirectTo = "showtimes.cc"; originCertificate = showtimesOrigin; };
    };
  };

  sops.defaultSopsFile = ../../secrets/k3s-worker-1.yaml;

  # THE ORIGIN CERTIFICATES' PRIVATE KEYS. Only the keys are secret -- the certificates themselves
  # are public documents and live in the nix store beside this file.
  #
  # `owner = "caddy"` because Caddy reads them as itself at startup, and 0400 because nothing else
  # on this host has any business with them. A wrong owner here does not fail the build: it fails
  # at Caddy start, which on this host means the product is down until somebody reads a journal.
  sops.secrets."origin-tls/kinowo_net"   = { owner = "caddy"; mode = "0400"; };
  sops.secrets."origin-tls/showtimes_cc" = { owner = "caddy"; mode = "0400"; };

  system.stateVersion = "26.05";
}
