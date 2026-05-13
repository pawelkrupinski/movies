package modules

import com.google.inject.AbstractModule
import services.{Keepalive, ShowtimeCache}

class CacheModule extends AbstractModule {
  override def configure(): Unit = {
    bind(classOf[ShowtimeCache]).asEagerSingleton()
    bind(classOf[Keepalive]).asEagerSingleton()
  }
}
