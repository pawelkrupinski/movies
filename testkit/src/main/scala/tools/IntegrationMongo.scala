package tools

/**
 * Refuses to let the `it/` layer run against a REAL MongoDB cluster.
 *
 * The integration specs write sentinel rows into `movies` / `tasks` / … and purge
 * them in `afterAll`. They resolve their target through `Env` — and a `MONGODB_URI` at
 * `127.0.0.1:27017` is PROD via an ssh tunnel to the prod Mongo host (the old `.env.local`
 * carried exactly that). So `sbt itAll` with a tunnel open silently wrote
 * sentinels into the production corpus, and a run killed before `afterAll` stranded
 * them there. That is not hypothetical — prod still carried "Dotted (1902)" rows,
 * and `MovieRepositoryIntegrationSpec` + `ci.yml` both grew purge code for them.
 *
 * Host is NOT the discriminator: the prod tunnel is `127.0.0.1` too, and CI runs a
 * throwaway docker Mongo on `127.0.0.1:27017` under the database name `kinowo`, so
 * neither the host nor the db name separates the two. CREDENTIALS do — a throwaway
 * `mongod` has no auth, while every real cluster this project talks to is reached
 * either with a `user:pass@` userinfo or over `mongodb+srv://`:
 *
 *   mongodb://127.0.0.1:27017/kinowo?directConnection=true        CI docker    ALLOW
 *   mongodb://127.0.0.1:28017/?directConnection=true              local RS     ALLOW
 *   mongodb://kinowo_app_2:secret@127.0.0.1:27017/kinowo          PROD tunnel  REFUSE
 *   mongodb+srv://user:secret@cluster.mongodb.net/kinowo          Atlas        REFUSE
 *
 * Set `KINOWO_ALLOW_REMOTE_IT=1` to override, for the rare deliberate run against a
 * throwaway remote. The override is loud by design — you have to name it.
 */
object IntegrationMongo {

  /** Env var that waives the guard for a deliberate run against a remote cluster. */
  final val OverrideVar = "KINOWO_ALLOW_REMOTE_IT"

  /** True when `uri` names a real, credentialed cluster rather than a throwaway
   *  local `mongod`. Pure — the whole guard decision lives here so it can be
   *  tested without an env or a server. */
  def isProtectedCluster(uri: String): Boolean = {
    val trimmed = uri.trim
    val isSrv   = trimmed.toLowerCase(java.util.Locale.ROOT).startsWith("mongodb+srv://")
    // Userinfo is the segment between the scheme and the first '/' that follows,
    // and only counts when it actually carries a '@' (mongodb://host/db has none).
    val afterScheme = trimmed.dropWhile(_ != '/').dropWhile(_ == '/')
    val authority   = afterScheme.takeWhile(c => c != '/' && c != '?')
    val hasUserInfo = authority.contains('@')
    isSrv || hasUserInfo
  }

  /** Throw unless `uri` is a throwaway Mongo (or the override is set). Called by
   *  every `it/` spec before it opens a client. */
  def requireThrowaway(uri: String, overrideSet: Boolean): Unit =
    if (isProtectedCluster(uri) && !overrideSet)
      throw new IllegalStateException(
        s"""Refusing to run integration tests against what looks like a REAL MongoDB cluster.
           |
           |  MONGODB_URI resolved to: ${redact(uri)}
           |
           |These specs write and delete sentinel rows. Against prod that strands data —
           |it already has ("Dotted (1902)"). A MONGODB_URI on 127.0.0.1:27017 is the
           |prod tunnel (scripts/with-prod-mongo.sh exports exactly that), so don't run
           |the it/ layer under it.
           |
           |Run against a throwaway instead — a mongod of the run's own:
           |  scripts/it-local.sh
           |
           |Deliberately targeting a throwaway REMOTE? Set $OverrideVar=1.""".stripMargin)

  /** The guard as the specs call it, over the configuration the spec resolved: its cluster and
   *  override, when it names a cluster at all. */
  def requireThrowaway(configuration: settings.ProcessConfiguration): Unit =
    IntegrationMongoTarget.from(configuration).foreach(_.requireThrowaway())

  /** Hide the password before the URI reaches a test log / CI transcript. */
  def redact(uri: String): String =
    uri.replaceAll("://([^:/?#@]+):([^@/?#]+)@", "://$1:***@")
}
