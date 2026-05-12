package modules

import clients.HeliosClient
import com.google.inject.AbstractModule
import services.ShowtimeCache

class ClientsModule extends AbstractModule {
  override def configure(): Unit =
    bind(classOf[HeliosClient]).toInstance(new HeliosClient())
}
