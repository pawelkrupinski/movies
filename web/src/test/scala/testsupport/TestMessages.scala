package testsupport

import com.typesafe.config.ConfigFactory
import play.api.http.HttpConfiguration
import play.api.i18n._
import play.api.{Configuration, Environment, Mode}

/**
 * A real `MessagesApi` for tests, built off the checked-in `conf/messages`
 * (Polish) + `messages.en` bundles — no running Play app needed. View specs and
 * page snapshots `import testsupport.TestMessages.given` to get the deployment's
 * Polish `Messages` (what production renders under the default Poland country),
 * so their assertions/snapshots see the same copy prod does.
 */
object TestMessages {
  private val env         = Environment.simple(mode = Mode.Test)
  private val config      = Configuration(ConfigFactory.load(env.classLoader))
  private val langs       = new DefaultLangsProvider(config).get
  private val httpConfig  = HttpConfiguration.fromConfiguration(config, env)
  val messagesApi: MessagesApi = new DefaultMessagesApiProvider(env, config, langs, httpConfig).get

  /** The deployment's Polish `Messages` (default country). */
  given deployment: Messages = MessagesImpl(Lang("pl"), messagesApi)

  def forLang(code: String): Messages = MessagesImpl(Lang(code), messagesApi)
}
