package services

import models.{CreateItemRequest, Item, UpdateItemRequest}
import repositories.ItemRepository

import javax.inject.{Inject, Singleton}
import scala.concurrent.Future

/** Business-logic layer sitting between the controller and the repository.
  * Add validation, enrichment, or cross-cutting concerns here.
  */
@Singleton
class ItemService @Inject() (repo: ItemRepository) {

  def listAll(): Future[Seq[Item]] = repo.findAll()

  def getById(id: String): Future[Option[Item]] = repo.findById(id)

  def create(req: CreateItemRequest): Future[Either[String, Item]] = {
    if (req.name.isBlank)
      Future.successful(Left("name must not be blank"))
    else if (req.price < 0)
      Future.successful(Left("price must be >= 0"))
    else {
      val item = Item(None, req.name.trim, req.description.trim, req.price, req.tags)
      repo.insert(item).map(Right(_))
    }
  }

  def update(id: String, req: UpdateItemRequest): Future[Option[Item]] =
    repo.update(id, req)

  def delete(id: String): Future[Boolean] = repo.delete(id)
}
