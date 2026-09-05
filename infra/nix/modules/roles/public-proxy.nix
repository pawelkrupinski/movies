# THE PUBLIC HTTP SURFACE ON THIS FLEET, and the only deliberate exception to a convention that is
# otherwise absolute here: every other service binds the private subnet, and the only port open at
# the Hetzner edge is 22.
#
# WHAT IT PUBLISHES. Two different things on two different hosts, and the distinction is worth
# keeping straight:
#
#   monitoring-1   Grafana, and nothing else. Of the three things a reverse proxy can do --
#                  terminate TLS, route, and authenticate -- only the first two are useful to a
#                  service with no login of its own. Grafana authenticates its own users, so it is
#                  the sole internal service that can safely stand behind a proxy whose only job is
#                  TLS. Prometheus, Alertmanager, VictoriaLogs, the k3s apiserver, node_exporter and
#                  mongod stay private; publishing them would mean inventing an auth layer here, and
#                  a shared password in front of an unauthenticated admin API is a worse answer than
#                  a tunnel. For those the answer remains
#                  `ssh -N -L <port>:10.20.0.11:<port> root@<host>`, which needs no open port.
#
#   k3s-worker-1   the PRODUCT: kinowo.net, the showtimes.cc apex, and the per-country path
#                  prefixes beneath it. These are meant to be public, they authenticate their own
#                  admin pages, and they proxy to NodePorts on 127.0.0.1 because the pods run on
#                  that same host.
#
# WHY MULTIPLE VHOSTS RATHER THAN ONE. This module used to take a single `hostName`/`upstream`
# pair, which was right while the fleet published exactly one thing. The product needs several
# names on one host, so the option is an attrset keyed by public hostname -- and, since the
# Showtimes countries share a name and are told apart by a path segment, one of those keys fans
# out again over `pathUpstreams`. The alternative -- an ingress
# controller plus cert-manager inside k3s -- buys Kubernetes-native Ingress objects at the cost of
# two more controllers and a LoadBalancer story on a cluster that has servicelb disabled, to do
# what fifteen lines of Caddy already do here.
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.publicProxy;
in
{
  options.fleet.publicProxy = {
    enable = lib.mkEnableOption "a public HTTPS reverse proxy";

    acmeEmail = lib.mkOption {
      type = lib.types.str;
      description = ''
        Where Let's Encrypt sends expiry warnings. Required by the ACME terms; a wrong address here
        means the one notification that a renewal has been failing for weeks goes nowhere.
      '';
    };

    vhosts = lib.mkOption {
      default = { };
      description = ''
        Public hostnames this proxy answers for, keyed by the name itself. Each name is also the
        name a certificate is issued for, so every key must already resolve to this host's public
        address -- ACME's HTTP-01 challenge is served on port 80 of whatever the DNS says, and a
        name pointed elsewhere fails issuance rather than falling back to plain HTTP.
      '';
      type = lib.types.attrsOf (lib.types.submodule {
        options = {
          upstream = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = ''
              host:port to proxy to -- a private-subnet address for a service on another fleet host,
              or 127.0.0.1:<nodePort> for a workload on this host's own k3s node.
            '';
          };

          extraConfig = lib.mkOption {
            type = lib.types.lines;
            default = "";
            description = ''
              Extra Caddy directives for this vhost, emitted before the upstream or redirect.

              Caddy sorts directives into its own canonical order rather than the order written,
              so a `redir` here still takes effect ahead of the `reverse_proxy` below regardless
              of where it appears. That is what makes a partial redirect expressible: send some
              paths elsewhere and proxy the rest.
            '';
          };

          pathUpstreams = lib.mkOption {
            type = lib.types.attrsOf lib.types.str;
            default = { };
            description = ''
              Upstreams for PATH PREFIXES of this vhost, keyed by the prefix (`"/uk"`), for a name
              that fronts several independent deployments instead of one. Each becomes a terminal
              Caddy `handle` matching the prefix and everything under it, and `upstream` /
              `redirectTo` become the fallback for whatever no prefix claimed — wrapped in a
              `handle` of its own so the precedence is written down rather than inferred from
              Caddy's directive order.

              This exists because the Showtimes countries share one domain and are told apart by a
              leading path segment (`showtimes.cc/uk/…`), each still its own pod against its own
              database. The alternative — one pod serving four countries — would mean one process
              against four databases.

              A bare prefix with no trailing slash (`/uk`) is redirected to `/uk/`: the app is
              MOUNTED at `/uk/` (`play.http.context`), so `/uk` matches no route and would 404 on
              the one URL a person is most likely to type.
            '';
            example = { "/uk" = "127.0.0.1:30912"; };
          };

          crawlerThrottle = lib.mkOption {
            default = null;
            description = ''
              Answer 429 to a named crawler on the FACETED LISTING paths, instead of rendering them.

              These paths (`/{city}/movies?cast=…`, and the Polish `/{city}/filmy`) are already
              `Disallow`ed in the app's robots.txt: they are UI state, not content, and the facet
              space is combinatorial -- city x every cast member -- so there is no finite set of
              them to finish crawling. A crawler that honours robots.txt never arrives here and
              never sees this. One that does not, does.

              WHY 429 AND NOT 404. A 404 makes each request cheap, which is most of the win, but it
              says nothing about RATE -- the crawler keeps discovering facet links in our own HTML
              and keeps asking. 429 with `Retry-After` is the signal Meta documents its crawlers as
              backing off from, so it reduces the arrival rate rather than only the cost of each
              arrival.

              WHY AT THE PROXY AND NOT IN THE APP. Two reasons, and the second is the one that is
              easy to miss: the request never reaches the JVM (no read-model query, no multi-MB
              render), AND it never reaches `WebHttpMetrics`, so it does not add a permanent 4xx
              floor to the error-share panel -- the exact confusion web-errors.rules exists to
              explain.

              Matched by USER-AGENT SUBSTRING, so it is deliberately narrow: Meta's share-preview
              agent is `facebookexternalhit`, a different string on different paths, and stays
              untouched. So do the film pages and `og-image`, which are the content we want
              indexed.
            '';
            example = { userAgents = [ "meta-externalagent" ]; };
            type = lib.types.nullOr (lib.types.submodule {
              options = {
                userAgents = lib.mkOption {
                  type = lib.types.listOf lib.types.str;
                  description = ''
                    User-agent substrings to throttle. Each becomes one wildcard `header User-Agent`
                    line in a single matcher, which Caddy ORs together.
                  '';
                };
                listingPaths = lib.mkOption {
                  type = lib.types.listOf lib.types.str;
                  default = [ "movies" "filmy" ];
                  description = ''
                    The final path segment of a faceted listing, in every language this domain
                    serves it under. The country prefixes are NOT listed here -- they are read off
                    `pathUpstreams`, so a new country cannot be onboarded into an unthrottled hole.
                  '';
                };
                retryAfterSeconds = lib.mkOption {
                  type = lib.types.ints.positive;
                  default = 3600;
                  description = ''
                    The `Retry-After` a throttled crawler is handed. An hour: long enough to matter
                    against a crawler running for days, short enough that a mistake here expires on
                    its own rather than needing a deploy to undo.
                  '';
                };
              };
            });
          };

          originCertificate = lib.mkOption {
            type = lib.types.nullOr (lib.types.submodule {
              options = {
                certFile = lib.mkOption { type = lib.types.path; description = "PEM certificate, world-readable."; };
                keyFile  = lib.mkOption { type = lib.types.str;  description = "Path to the PEM key on the host, readable by caddy."; };
              };
            });
            default = null;
            description = ''
              Serve a FIXED certificate for this vhost instead of getting one from Let's Encrypt.

              This exists for one shape: a name that is only ever reached through a CDN. Cloudflare
              in `Full (strict)` mode validates the origin certificate against ITS OWN Origin CA, so
              a Cloudflare Origin certificate is trusted there and lasts fifteen years -- which takes
              renewal off the critical path entirely. That matters more under a CDN than without one:
              a failed ACME renewal in front of `strict` is not a browser warning any more, it is a
              526 and the site is down.

              ⚠️ NEVER ON A NAME BROWSERS REACH DIRECTLY. An Origin CA certificate is trusted by
              Cloudflare and by nothing else, so a visitor arriving at a dns-only name would get a
              certificate their browser rejects outright. On this fleet that means it belongs on the
              product vhosts (which are proxied) and NOT on `grafana` / `headlamp`, which are
              dns-only and live on monitoring-1 in any case.

              ACME still runs for every vhost that does not set this, so the two coexist.
            '';
          };

          redirectTo = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = ''
              Public hostname to 301 to instead of proxying, path and query preserved. This is how
              the `www.` spelling of a name is served: it still needs its own vhost (and so its own
              certificate, because the redirect itself is delivered over TLS -- a browser that
              already knows the HSTS policy will not follow a redirect it cannot validate first),
              but it runs no upstream.
            '';
          };
        };
      });
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = lib.mapAttrsToList (host: v: {
      # Catches the two ways a vhost is silently wrong: neither field set (Caddy would serve an
      # empty 200 for the site, which looks like the app returning a blank page) and both set
      # (the redirect wins and the upstream is dead config nobody notices).
      assertion = (v.upstream == null) != (v.redirectTo == null);
      message = "fleet.publicProxy.vhosts.\"${host}\" must set exactly one of `upstream` or `redirectTo`.";
    }) cfg.vhosts;

    security.acme = {
      acceptTerms = true;
      defaults.email = cfg.acmeEmail;
    };

    services.caddy = {
      enable = true;

      # Caddy rather than nginx, for one reason that matters on a fleet nobody watches daily: it
      # obtains and RENEWS every certificate itself, with no timer to forget, no reload hook to get
      # wrong, and no separate acme.sh state to go stale. The config below is the entire deployment.
      virtualHosts = lib.mapAttrs (host: v:
        let
          # One terminal `handle` per path prefix, plus the bare-prefix redirect that keeps
          # `https://showtimes.cc/uk` (no trailing slash) working against an app mounted at
          # `/uk/`. Emitted in the attrset's own (sorted) order; the prefixes are disjoint, so
          # the order is irrelevant beyond being deterministic.
          pathBlocks = lib.concatStringsSep "\n" (lib.mapAttrsToList (prefix: upstream: ''
            redir ${prefix} ${prefix}/ permanent
            handle ${prefix}/* {
              reverse_proxy ${upstream}
            }
          '') v.pathUpstreams);

          fallback = if v.redirectTo != null
            then ''redir https://${v.redirectTo}{uri} permanent''
            else ''reverse_proxy ${v.upstream}'';

          # THE FACETED-LISTING THROTTLE, emitted as the FIRST `handle` so it wins over the
          # per-country ones. `handle` blocks at one level are mutually exclusive and evaluated in
          # written order, which is the only reason this reads top-to-bottom while everything else
          # in a Caddyfile is sorted into Caddy's own directive order.
          #
          # The country prefixes come off `pathUpstreams` rather than being spelled again, so the
          # regex covers exactly the mounts this vhost actually has. `(?:...)` and not `(...)`:
          # Caddy names its capture groups, and an unnamed capturing group here would be one more
          # thing that has to stay in step for no benefit. The optional prefix is what makes the
          # same expression cover a root-mounted deployment (kinowo.net's `/{city}/filmy`) and a
          # path-mounted one (`showtimes.cc/us/{city}/movies`).
          throttleBlock = lib.optionalString (v.crawlerThrottle != null) (
            let
              t = v.crawlerThrottle;
              prefixes = lib.concatStringsSep "|" (map (p: lib.removePrefix "/" p) (lib.attrNames v.pathUpstreams));
              prefixGroup = lib.optionalString (prefixes != "") "(?:/(?:${prefixes}))?";
              listings = lib.concatStringsSep "|" t.listingPaths;
              agentLines = lib.concatStringsSep "\n              " (map (a: ''header User-Agent *${a}*'') t.userAgents);
            in ''
              @throttledCrawler {
                ${agentLines}
                path_regexp facetListing ^${prefixGroup}/[^/]+/(?:${listings})/?$
              }
              handle @throttledCrawler {
                header Retry-After "${toString t.retryAfterSeconds}"
                respond "Filtered listings are disallowed by robots.txt on this host. The film pages and sitemap are open." 429
              }
            '');
          # A FIXED CERTIFICATE INSTEAD OF ACME, when the vhost asks for one. Emitted FIRST so it is
          # unmistakable when reading the generated Caddyfile which names are not on Let's Encrypt.
          # Caddy takes an explicit `tls <cert> <key>` as "manage nothing for this site", so the
          # ACME machinery simply does not run for it -- there is no renewal to fail.
          tlsBlock = lib.optionalString (v.originCertificate != null)
            "tls ${v.originCertificate.certFile} ${v.originCertificate.keyFile}";
        in {
        extraConfig = ''
          ${tlsBlock}
          ${v.extraConfig}
          ${throttleBlock}
          ${if v.pathUpstreams == { }
            then fallback
            else ''
              ${pathBlocks}
              handle {
                ${fallback}
              }
            ''}

          # HSTS. Deliberately modest -- one week, no preload, no includeSubDomains. Preload is a
          # one-way door (removal takes months and ships with the browser), and includeSubDomains
          # would impose HTTPS-only on every future name under these domains, including ones that
          # do not exist yet.
          header Strict-Transport-Security "max-age=604800"
        '';
      }) cfg.vhosts;
    };

    # 80 AND 443, AND 80 IS NOT OPTIONAL. ACME's HTTP-01 challenge is served on 80, so closing it
    # does not harden anything -- it just makes every certificate fail to renew, silently, sixty
    # days later. Caddy redirects 80 to 443 for everything that is not a challenge.
    networking.firewall.allowedTCPPorts = [ 80 443 ];
  };
}
