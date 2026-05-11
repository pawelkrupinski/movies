package modules

import com.google.inject.AbstractModule
import services.ShowtimeCache

class CacheModule extends AbstractModule {
  override def configure(): Unit =
    bind(classOf[ShowtimeCache]).asEagerSingleton()
}
