# HOW CI READS PRODUCTION MONGO, and the reason it is not simply "give the runner an ssh key".
#
# .github/workflows/record-scrape-fixtures.yml records each country's real `cinema_scrapes` corpus
# and prod's enrichment coverage of that same repertoire, nightly, from a GitHub-hosted runner.
# Until the database moved off Fly it reached it with `flyctl proxy --app kinowo-mongo`; that app is
# stopped, and mongod here listens on 127.0.0.1, on 10.20.0.13 (a private subnet no runner can
# route to) and on a Fly 6PN address (how the deployed apps arrive). So the job needs a route in,
# and the only question is how wide it has to be.
#
# The two easy answers are both refused, for the same reason nix/modules/fleet/deploy-staging.nix
# and roles/k8s-deploy.nix refuse theirs -- the least managed machine involved is an
# `ubuntu-latest` runner nobody owns, so whatever it holds must be worth losing:
#
#   * A NORMAL SSH KEY on this host is a shell on the database. That is not "read access to a
#     collection", it is root's dbPath, the sops-decrypted secrets under /run, the keyfile, and
#     every other host this machine's WireGuard peer can reach.
#   * BINDING MONGOD PUBLICLY trades the whole argument in hosts/mongo-1/default.nix -- the bind
#     list there is three addresses and roles/mongodb.nix asserts one of them -- for the
#     convenience of one nightly job, and leaves the database exposed for the 23h59m it is not
#     running.
#
# SO: the same shape as the other two, one forced command, and the capability it grants is
# deliberately smaller than a port forward. `ssh -L` lets the CLIENT name the host and port it
# wants opened and leaves the server to police the request; this endpoint relays its own stdin and
# stdout to an address written HERE, in the closure. The client cannot name a destination, cannot
# ask for a second one, and cannot reach the private subnet -- `restrict` denies forwarding
# outright, exactly as k8s-deploy.nix's comment insists it must.
#
# WHAT BOUNDS THE READ ITSELF is not this key but the Mongo user in the credential it hands out:
# `read` on the three country databases and nothing else. The key opens a pipe to mongod; the
# credential decides what may be said down it. Both halves have to be wrong for CI to be able to
# write to production.
#
# DISABLED UNTIL A KEY IS SET. With `authorizedKey` empty -- the default -- no account is created,
# no secret is decrypted, and nothing about this host changes.
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.mongoCiRead;

  endpoint = pkgs.writeShellScript "mongo-ci-read-endpoint" ''
    set -euo pipefail

    # The client's requested command lands here as a STRING and is never executed -- that is what a
    # forced command buys, and it is why this may be compared but must never be interpolated into
    # one. Two verbs, both read-only; anything else is refused rather than interpreted.
    case "''${SSH_ORIGINAL_COMMAND:-connect}" in

      # THE CREDENTIAL, so that no production database password is stored in GitHub. This is the
      # part worth arguing with rather than accepting: it collapses two secrets into one, and the
      # ssh key alone is then enough to read production.
      #
      # It is the right trade here because the second secret was never independent -- both halves
      # would have lived in the same GitHub Actions store, handed to the same runner, in the same
      # job. What it buys is that the password lives in sops on this host and nowhere else, so
      # rotating it is one `sops` edit and one deploy, with no repository secret that silently goes
      # on working with the old value until someone remembers it exists.
      #
      # The username is printed WITH it because the runner should not carry a second thing that has
      # to agree with this file; if they ever disagree the symptom is an authentication failure in
      # the middle of the night, which is a poor way to learn about a copy-paste.
      credential)
        printf '%s:%s\n' ${lib.escapeShellArg cfg.username} "$(${pkgs.coreutils}/bin/tr -d '\n' < ${cfg.passwordFile})"
        ;;

      # THE PIPE. One relay per TCP connection the runner makes, because the Mongo driver wants a
      # real socket per pooled connection and a dump would not do: the recording is a driver-side
      # keyset scan plus an aggregation, not a `mongodump` (see worker/src/test/scala/scripts/
      # RecordCorpusFixture.scala, which the CI side cannot restructure into one).
      #
      # LOOPBACK IS THE ONLY FAR-SIDE ADDRESS THAT WORKS FROM INSIDE AN SSH SESSION here, and
      # roles/mongodb.nix already asserts mongod binds it because the backup timer depends on the
      # same thing -- so this relay cannot be left pointing at an address mongod stopped serving
      # without that assertion failing first.
      connect)
        exec ${pkgs.coreutils}/bin/timeout ${cfg.sessionTimeout} \
          ${pkgs.socat}/bin/socat STDIO "TCP4:127.0.0.1:${toString cfg.mongoPort}"
        ;;

      *)
        echo "mongo-ci-read: unknown request. Send 'connect' or 'credential' as the ssh command." >&2
        exit 2
        ;;
    esac
  '';
in
{
  options.fleet.mongoCiRead = {
    enable = lib.mkEnableOption "a forced-command ssh endpoint that relays one connection to mongod";

    authorizedKey = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = ''
        CI's public key, as one authorized_keys line WITHOUT options -- the forced command and
        `restrict` are added here so that they cannot be left off at the call site. Empty means the
        account is not created at all, so a host that has never been given one does not carry a
        dormant reader.
      '';
    };

    username = lib.mkOption {
      type = lib.types.str;
      default = "kinowo-ci-corpus";
      description = ''
        The Mongo user the endpoint hands out, which should hold `read` on the country databases
        and NOTHING else.

        A SEPARATE USER FROM THE APPLICATION'S AND FROM THE BACKUP'S, on the same reasoning
        roles/mongodb.nix gives for `backup.username`: the application's credential can write, and
        a job that records fixtures has no business holding one that can.

        THE USER IS NOT CREATED BY THIS ROLE, again matching that role: creating it means a
        `db.createUser()` against a running mongod, which is a one-time build step and not
        something to attempt on every activation. On mongo-1, as an admin:

          db.getSiblingDB("admin").createUser({
            user: "kinowo-ci-corpus",
            pwd: "<the sops value>",
            roles: [ { role: "read", db: "kinowo"    },
                     { role: "read", db: "kinowo_uk" },
                     { role: "read", db: "kinowo_de" },
                     { role: "read", db: "kinowo_us" },
                     { role: "read", db: "kinowo_es" } ] })

        ONE ROLE PER COUNTRY, and the list has to grow with `Country.all`. It did not when
        the United States was added, and the failure is quiet in the worst way: the other
        three countries record fine, the US leg dies with `not authorized on kinowo_us`,
        and the recorder — correctly — refuses to write a fixture from a read that failed,
        so what you see is "read came back empty across all 5031 catalogue cinemas". An
        already-created user is amended rather than recreated:

          db.getSiblingDB("admin").grantRolesToUser(
            "kinowo-ci-corpus", [ { role: "read", db: "kinowo_us" } ])

        `ConvergenceLegWiringSpec` pins this list against `Country.all` so a missing grant
        fails a test rather than a nightly job.
      '';
    };

    passwordFile = lib.mkOption {
      type = lib.types.str;
      default = config.sops.secrets."mongodb/ci-corpus-password".path;
      defaultText = ''config.sops.secrets."mongodb/ci-corpus-password".path'';
      description = ''
        The read-only user's password, decrypted into /run by sops-nix.

        KEEP IT ALPHANUMERIC. The runner splices this straight into a Mongo URI's userinfo, so a
        `@`, `/`, `:` or `%` in it does not fail authentication -- it makes the URI parse as a
        different host and the failure names something unrelated. Percent-encoding it here would
        hide the same hazard one layer deeper.
      '';
    };

    mongoPort = lib.mkOption {
      type = lib.types.port;
      default = config.fleet.mongodb.port;
      defaultText = "config.fleet.mongodb.port";
      description = ''
        Read off the mongodb role rather than restated, so a port change on this host cannot leave
        the relay pointing at a closed one.
      '';
    };

    sessionTimeout = lib.mkOption {
      type = lib.types.str;
      default = "1h";
      description = ''
        How long a single relayed connection may live. This bounds a LEAKED relay -- a runner that
        vanished mid-job leaves its half of the socket open, and without this the ssh session sits
        here until something else notices.

        Comfortably above the workflow's own 30-minute timeout on purpose: set below it and a
        corpus scan is cut off mid-read, which the recorder then refuses to write a fixture from
        (correctly) while reporting an empty read rather than a severed pipe.
      '';
    };
  };

  config = lib.mkIf (cfg.enable && cfg.authorizedKey != "") {
    users.groups.mongociread = { };

    users.users.mongociread = {
      isSystemUser = true;
      group = "mongociread";
      home = "/var/lib/mongociread";
      createHome = true;
      # A shell is required for a forced command to run at all; it is never reached interactively,
      # because `restrict` denies a pty and the forced command replaces whatever was asked for.
      shell = pkgs.bashInteractive;

      # `command=` IS THE SECURITY BOUNDARY, and `restrict` is what stops the shape being worked
      # around: no port forwarding, no agent forwarding, no pty, no X11. Dropping `restrict` here
      # would be worse than on the other two endpoints -- a forwarding-capable key on THIS host is
      # a route onto 10.20.0.13 and down the Fly tunnel.
      openssh.authorizedKeys.keys = [
        ''command="${endpoint}",restrict ${cfg.authorizedKey}''
      ];
    };

    # DECLARED HERE, NOT IN THE HOST FILE, for the reason roles/mongodb.nix gives for its own two:
    # enabling the role is then the whole of what it takes to have the secret, and a role whose
    # secrets live somewhere else half-starts on a host that forgot them.
    #
    # Owned by the reader because the endpoint runs AS the reader; 0400 because nothing else on
    # this box has any business with a credential that can read the whole corpus.
    sops.secrets."mongodb/ci-corpus-password" = {
      owner = "mongociread";
      mode = "0400";
    };
  };
}
