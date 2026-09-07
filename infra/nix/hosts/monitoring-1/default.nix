{ config, ... }:

let
  # THE FILESYSTEM'S OWN UUID, NOT THE HETZNER VOLUME'S DEVICE PATH, AND THE DIFFERENCE IS THE WHOLE
  # POINT OF THIS LINE. `/dev/disk/by-id/scsi-0HC_Volume_<id>` names the ENCLOSURE -- which Hetzner
  # volume is bolted to this machine -- and a Hetzner volume cannot leave its location. So when this
  # host moved nbg1 -> fsn1 on 2026-09-01 the data had to be copied onto a NEW volume with a NEW id,
  # and every spelling of the mount that named the old id was, at that moment, a host that boots
  # without its metrics disk. A UUID names the DATA, and the data is what moved: the fsn1 volume was
  # given this exact UUID with `tune2fs -U` after the copy, so the mount below did not have to change
  # at all and the machine came up with its TSDB already where it expected it.
  #
  # A WRONG VALUE HERE IS STILL WORSE THAN A BUILD FAILURE, for the reason ../mongo-1/default.nix
  # spells out at length -- with Prometheus filling the root disk in mongod's place. What changed is
  # only which identity is being named, not how carefully it has to be right. Read it back with
  # `blkid /dev/disk/by-id/scsi-0HC_Volume_$(terraform -chdir=infra/terraform output -json hosts \
  #   | jq -r '."monitoring-1".volumes."monitoring-data"')`.
  volumeDevice = "/dev/disk/by-uuid/1d0ae481-d7d3-42e1-8a77-bbc13164c9ee";
in
{
  imports = [
    ./disko.nix
    ../../modules/roles/public-proxy.nix
    ../../modules/roles/google-sso.nix
    ../../modules/roles/k8s-deploy.nix
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
    publicAddress = "128.140.49.167";
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
      { address = "10.20.0.13"; host = "mongo-1"; role = "mongodb"; }
      { address = "10.20.0.11"; host = "monitoring-1"; role = "monitoring"; }
      { address = "10.20.0.12"; host = "k3s-worker-1"; role = "k3s-worker"; }
    ];
  };

  # GRAFANA AND PROMETHEUS MAY BE BOUNCED BY AN UNATTENDED SWITCH ON THIS HOST, AND NOTHING ELSE MAY.
  #
  # Every dashboard edit rewrites grafana.service -- the provisioning directory's store path is in
  # config.ini, which is in ExecStart -- so without this line the applier refuses the closure and,
  # with it, every unrelated change staged for this machine. It refuses SILENTLY: the timer keeps
  # firing, the unit keeps completing, and only its journal says why. That happened on 2026-08-30
  # and took a manual switch-to-configuration to clear. roles/grafana.nix asserts on the omission
  # now, and this is the line that assertion asks for.
  #
  # THE SENTENCE modules/fleet/default.nix ASKS TO BE WRITTEN OUT FOR EACH ENTRY: a Grafana restart
  # at an arbitrary moment costs a few seconds of the monitoring UI and a gap in no graph at all.
  # Prometheus keeps scraping and Alertmanager keeps notifying throughout, because neither of them
  # is Grafana; the worst case is somebody reloading a page mid-incident. That is a far smaller cost
  # than a fleet whose deploys have quietly stopped, which is what the alternative buys.
  #
  # PROMETHEUS EARNED ITS LINE THE WAY THIS COMMENT PREDICTED IT WOULD. It used to say that
  # prometheus.service "would be equally cheap to bounce, but nothing has yet needed it to be";
  # cinema-scrape.rules is what needed it. Every alert-rule edit rewrites prometheus.service the
  # same way a dashboard edit rewrites grafana.service -- roles/prometheus.nix puts each rule file
  # in `restartTriggers` deliberately, because a rule written into /etc and never re-read is silent
  # in precisely the way an unloaded rule file is -- so without this line the applier would refuse
  # the closure and every unrelated change staged for this machine along with it, SILENTLY, exactly
  # as the missing Grafana entry did on 2026-08-30.
  #
  # THE SENTENCE modules/fleet/default.nix ASKS TO BE WRITTEN OUT FOR EACH ENTRY: a Prometheus
  # restart at an arbitrary moment costs a few seconds of scrape gap and loses no data at all --
  # the TSDB is on disk and the WAL is replayed on start (roles/prometheus.nix says so at the
  # `restartTriggers` it sets). Nothing outside this fleet can see it. The one visible cost is that
  # `for:` timers restart with the process, so an alert part-way through its pending window waits
  # again; on rules whose shortest `for:` is 5m that is not a real risk of missing anything.
  #
  # STILL NAMED UNITS, NOT `[ "*" ]`, for the reason modules/fleet/default.nix gives about
  # `reloadableUnits`: a wildcard forgives every unit nobody has thought about. The fleet's
  # neverDisturbUnits floor (sshd, mongodb, k3s) is checked first and still applies.
  #
  # ALERTMANAGER JOINED THE LIST ON 2026-09-05, which is the case this comment used to describe as
  # hypothetical ("equally cheap to bounce, but nothing has yet needed it to be"). Something needed
  # it: alertmanager.yaml gained an email receiver, and roles/prometheus.nix sets `restartTriggers`
  # on that file precisely so a changed route is READ rather than silently written -- so every
  # future edit to a route or a receiver disturbs this unit. Without it here, each of those edits
  # would refuse the whole switch and hold back every other merge staged onto this host.
  #
  # THE COST IS SMALL AND BOUNDED. A bounce loses no alert state that matters: silences and the
  # notification log live under `--storage.path` on disk, and an alert mid-`group_wait` is re-sent
  # by the next evaluation. What it does lose is in-flight grouping, which is seconds.
  #
  # AND IT DOES NOT COVER ITS OWN INTRODUCTION. `nixos-auto-apply` reads this list from the closure
  # it is RUNNING, not the one it is judging, so the switch that ADDS a name here is weighed
  # without it -- the same caveat container-image-gc.nix documents. Expect this one to want a hand
  # apply; `AutoApplyBlocked` in nixos-deploy.rules says so within a day if it does.
  fleet.autoApply.restartableUnits = [ "alertmanager.service" "grafana.service" "prometheus.service" ];

  # THIS API SERVER BELIEVES ONLY ITS OWN CERTIFICATES AGAIN. `fleet.k3sServer.oidc` stood here
  # until 2026-09-07, so that Headlamp could log a person in with Google and present that token to
  # the API as a Kubernetes subject in their own right. Sign-in has moved out to the fleet's
  # oauth2-proxy door, which admits people to a UI rather than to Kubernetes, so nothing presents
  # a Google token here any more and trusting the issuer would widen the box that holds etcd for
  # no remaining caller.
  #
  # ⚠️ THIS IS THE HALF THAT HAS TO COME BACK FIRST if per-person cluster identity is ever wanted
  # again. Restoring the `User` subject in movies-gitops/headlamp/rbac.yaml achieves nothing on its
  # own -- with no trusted issuer the name matches nobody. The k3s option and its `oidc:` username
  # prefix went with it; see docs/headlamp-serviceaccount-subject.md in the app repo.

  # CADDY RELOADS RATHER THAN RESTARTS, which is why it is here and not in the list above.
  # `caddy.service` reports `CanReload=yes` with an `ExecReload` of `caddy reload --force`, so a
  # vhost added or changed is picked up without dropping a connection -- including the TLS
  # session somebody is reading Grafana over at that moment.
  #
  # WITHOUT IT THE APPLIER REFUSES THE WHOLE CLOSURE, SILENTLY -- the same failure the paragraph
  # above describes for the missing Grafana entry on 2026-08-30, and it was about to happen
  # again: publishing Headlamp adds a vhost, which changes this unit, and every unrelated change
  # staged for this machine would have sat unapplied with nothing saying why.
  #
  # THE SENTENCE WRITTEN OUT: a graceful config reload of the reverse proxy at an arbitrary
  # moment is a cost this host accepts -- it drops no connection and no request. A RESTART is
  # deliberately NOT accepted and stays refused, so if the Caddy package itself changes and
  # switch-to-configuration wants a bounce, a person takes that brief 502 knowingly.
  fleet.autoApply.reloadableUnits = [ "caddy.service" ];

  # WHO MAY OPEN ANYTHING THIS HOST PUBLISHES -- and since 2026-09-07 that is the WHOLE of the
  # answer, for all three published names. See roles/google-sso.nix for why the sign-in lives on one
  # name and one callback.
  #
  # THE CLIENT ID IS SPELLED OUT HERE rather than read from `fleet.k3sServer.oidc`, which used to
  # hold it: the API server no longer trusts Google, so that option is gone and this is the last
  # consumer of the client. It is the same Google client either way -- not a secret, a client id
  # travels in every authorisation URL, and the secret half is in this host's sops file under
  # `google-sso/client-secret`.
  fleet.googleSso = {
    enable = true;
    clientId = "283216110679-ur705ei6rk5hlaukm13rrfioe45ltite.apps.googleusercontent.com";
    authHostName = "auth.kinowo.net";
    cookieDomain = ".kinowo.net";
    allowedEmails = [ "pawel.krupinski@gmail.com" ];
  };

  fleet.grafana = {
    enable = true;

    # MUST MATCH THE PUBLIC NAME. Grafana builds redirects, OAuth callbacks and the links in alert
    # notifications from root_url, so behind a proxy a wrong value here does not fail loudly -- it
    # sends people to http://localhost:3000, from an email, and looks like the alert is broken.
    rootUrl = "https://grafana.kinowo.net/";

    # SIGNED IN ALREADY, BY THE TIME GRAFANA SEES THE REQUEST. Caddy admits nobody to this vhost
    # without a Google session (`requireGoogleLogin` below), and hands Grafana the address that
    # session belongs to, so asking for a second password here would be theatre.
    #
    # ⚠️ THIS IS A HEADER GRAFANA BELIEVES, which is only safe because of where it will accept it
    # from. `proxyWhitelist` is the one address Caddy speaks from; a request arriving at
    # 10.20.0.11:3000 from anywhere else -- and every pod k3s schedules on these two machines can
    # reach that port -- is NOT admitted by the header, no matter what it claims. Without that
    # bound this option is a way to become admin by asking.
    proxyAuth = {
      enable = true;
      headerName = "X-Auth-Request-Email";
      whitelist = config.fleet.privateAddress;
    };
  };

  # PUBLIC HTTPS FOR THE THREE THINGS A PERSON OPENS IN A BROWSER, and since 2026-09-07 ONE DOOR IN
  # FRONT OF ALL OF THEM. Every published name below carries `requireGoogleLogin`, so the proxy --
  # not the application behind it -- is what decides whether a request belongs to somebody.
  #
  # THAT WAS NOT ALWAYS THE RULE, and the older comments here argued the opposite: that the proxy
  # adds TLS and a name, and a service authenticating its own users needs no door. Grafana's own
  # login was a shared password, Headlamp's was Google OIDC, VictoriaLogs had none at all -- three
  # answers to one question, each revoked in a different place. They are one answer now:
  # `fleet.googleSso.allowedEmails` above, enforced at `auth.kinowo.net`.
  #
  # WHAT THE DOOR DOES NOT DO is decide what a person may then SEE. That still differs per service
  # and cannot be unified away: Grafana takes the forwarded email as a per-user account, Headlamp
  # answers everyone with one read-only ServiceAccount, and VictoriaLogs has no notion of a user, so
  # its bound is the `/select`-only path list. See roles/public-proxy.nix for why nothing else here
  # is published at all.
  fleet.publicProxy = {
    enable = true;
    acmeEmail = "pawel@bitcashier.io";
    vhosts = {
      # A NAME THIS PROJECT OWNS, at last. The sslip.io spelling below was only ever a stand-in for
      # not having a domain (see roles/public-proxy.nix), and it carried a real cost: sslip.io is a
      # SHARED registered domain, so Let's Encrypt's per-domain rate limit is consumed by everyone
      # using it and an issuance here could fail for reasons that have nothing to do with this fleet.
      "grafana.kinowo.net" = {
        upstream = "10.20.0.11:3000";
        # Grafana authenticates its own users and could stand behind a proxy that only does TLS --
        # but its login is a shared password too, and this one asks WHO rather than WHAT.
        requireGoogleLogin = true;
      };

      # THE SIGN-IN ITSELF, and the only redirect URI registered with Google. It carries no login
      # of its own for the obvious reason: it is what a person is sent to in order to GET one.
      "auth.kinowo.net".upstream = config.fleet.googleSso.listenAddress;

      # THE KUBERNETES UI, and as of 2026-09-07 the vhost where this door stopped being belt-and-
      # braces and became the ONLY thing holding the line. Headlamp used to do its own Google OIDC
      # and hand the person's token to the API server, which made it a Kubernetes subject with
      # read-only RBAC of its own; that was removed in favour of one sign-in for the fleet, so the
      # backend now answers every request with a single ServiceAccount
      # (`-unsafe-use-service-account-token`, movies-gitops/headlamp/deployment.yaml).
      #
      # ⚠️ SO `requireGoogleLogin` HERE IS LOAD-BEARING IN A WAY THE OTHER TWO ARE NOT. Delete it
      # and Headlamp is not merely public -- it serves the whole read-only cluster view to anyone
      # who asks, with no second gate anywhere behind it. Grafana would still demand a login and
      # VictoriaLogs would still only expose `/select`; this one has nothing left underneath.
      #
      # ⚠️ AND IT ONLY GUARDS THE PUBLIC PATH. The upstream is a ClusterIP with no NetworkPolicy in
      # front of it, so any pod k3s schedules can reach Headlamp directly and read the cluster
      # without passing this door at all. That is the accepted cost of the unification, recorded
      # rather than discovered: docs/headlamp-serviceaccount-subject.md in the app repo measures it.
      #
      # THE UPSTREAM IS A CLUSTER SERVICE, not a host port. This machine is a k3s node, so
      # kube-proxy makes the ClusterIP routable from the host and Headlamp needs no NodePort
      # on every node's interfaces. The address is PINNED in
      # movies-gitops/headlamp/deployment.yaml precisely so this line can name it.
      "headlamp.kinowo.net" = {
        upstream = "10.43.165.84:80";
        requireGoogleLogin = true;
      };

      # THE FLEET'S LOGS, IN A BROWSER. VictoriaLogs has no authentication of its own
      # (roles/victoria-logs.nix says so at length, and until this vhost the bind address was the
      # whole of its access control), so the proxy supplies the login and publishes ONLY `/select`,
      # which is vmui and the LogsQL query API.
      # `/insert`, `/delete`, `/internal`, `/metrics` and the flags page answer 404 here; the
      # password guards a surface that can read logs, not one that can write or erase them.
      #
      # The Grafana datasource is unchanged and still the everyday path; this is for VictoriaLogs'
      # own UI -- its query builder, field stats and hit histograms -- without an ssh tunnel.
      "logs.kinowo.net" = {
        # WAS A SHARED PASSWORD, until 2026-09-07. `basicAuth` answered "does this request know the
        # string", which says nothing about who used it, cannot be withdrawn from one person, and
        # never expires. Google answers "is this Paweł's account" -- revocable, second-factored,
        # and logged somewhere other than here.
        requireGoogleLogin = true;
        pathUpstreams."/select" =
          "${config.fleet.victoriaLogs.listenAddress}:${toString config.fleet.victoriaLogs.port}";
        # The store's own `/` is a bare index of links, most of them to paths this vhost 404s.
        extraConfig = "redir / /select/vmui/";
      };

      # THE sslip.io NAME IS GONE, and the move to fsn1 is what settled it rather than a change of
      # mind. `grafana.2-28-52-210.sslip.io` RESOLVES ITS OWN IP OUT OF ITS OWN LABEL -- that is what
      # sslip.io is -- so the address it names is 2.28.52.210, a primary IP that could not follow
      # this host to Falkenstein (Hetzner primary IPs are location-scoped) and was released with the
      # nbg1 machine. Kept as a vhost it would not have degraded quietly: the name would still
      # resolve, to an address this fleet no longer holds, and Caddy would fail the HTTP-01 renewal
      # roughly sixty days later against a domain whose Let's Encrypt rate limit is shared with
      # everyone else using sslip.io. grafana.kinowo.net is the name now, and it is the one the
      # alert notifications have carried since roles/grafana.nix's rootUrl moved to it.
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

  # THE 6PN PEER INTO FLY'S PRIVATE NETWORK IS GONE, 2026-09-04.
  #
  # It existed to scrape `kinowo`'s own `/metrics` on port 9000 over Fly's private network, back
  # when the web tier ran there and Fly's managed Prometheus was unreachable (both read-only tokens
  # revoked; the only remaining Fly token is org-wide and deploy-capable, which must not sit on the
  # host that also runs the k3s control plane). The peer needed no Fly token at all -- a WireGuard
  # key this fleet holds rather than a credential somebody else issues.
  #
  # The 2026-08-29 cutover moved both tiers to k3s, Prometheus reaches them over NodePorts on the
  # Hetzner private network, and the DNS-discovery job that resolved `kinowo.internal` through this
  # tunnel went with it. Nothing sent a packet down it after that. Revoke the peer on Fly's side
  # with `fly wireguard remove` if it is still listed there.
  #
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
