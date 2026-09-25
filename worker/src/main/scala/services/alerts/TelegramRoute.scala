package services.alerts

/** Where a Telegram alerter posts: the bot, the chat, and the optional forum topic. */
final case class TelegramRoute(token: String, chatId: Long, topicId: Option[Long])

object TelegramRoute {
  val TokenKey = "TELEGRAM_BOT_TOKEN"

  /** The route read through `read`, or the keys whose absence leaves the alerter unrouted.
   *  `chatKeys` are alternatives, the first one set winning; when none is set they are named
   *  together (`A or B`). A chat id that is set but not a number counts as missing — it used
   *  to switch the alerter off exactly as silently as an absent one. The topic stays optional. */
  def resolve(read: String => Option[String], chatKeys: Seq[String], topicKey: String): Either[Seq[String], TelegramRoute] = {
    val token = read(TokenKey)
    val chat: Either[String, Long] = chatKeys.iterator.map(key => key -> read(key)).collectFirst { case (key, Some(raw)) => key -> raw } match {
      case None             => Left(chatKeys.mkString(" or "))
      case Some((key, raw)) => raw.trim.toLongOption.toRight(s"$key (not a number)")
    }
    val missing = token.fold(Seq(TokenKey))(_ => Nil) ++ chat.left.toSeq
    (token, chat) match {
      case (Some(t), Right(chatId)) => Right(TelegramRoute(t, chatId, read(topicKey).flatMap(_.trim.toLongOption)))
      case _                        => Left(missing)
    }
  }
}
