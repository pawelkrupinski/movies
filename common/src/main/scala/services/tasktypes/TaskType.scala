package services.tasktypes

/** The kinds of deferred work the queue carries. Extensible — add a case and a
 *  handler. The `name` is the on-disk discriminator, so keep it stable. A type may be
 *  deleted while rows of it are still queued: `MongoTaskQueue.claim` drops a row whose
 *  name no longer decodes. */
sealed trait TaskType { def name: String }

object TaskType {
  case object ScrapeCinema  extends TaskType { val name = "ScrapeCinema"  }
  case object EnrichDetails extends TaskType { val name = "EnrichDetails" }
  // A venue detail page read into venue_pages for a listing no film row holds yet (a cut-over country).
  case object ReadVenuePage extends TaskType { val name = "ReadVenuePage" }
  case object ResolveImdbId extends TaskType { val name = "ResolveImdbId" }
  case object ImdbRating    extends TaskType { val name = "ImdbRating"    }
  case object FilmwebRating extends TaskType { val name = "FilmwebRating" }
  case object RtRating      extends TaskType { val name = "RtRating"      }
  case object McRating      extends TaskType { val name = "McRating"      }

  // Operator-triggered, corpus-wide refresh runs (the `/tasks` page buttons).
  // Each is a global singleton: a constant dedup key collapses repeat clicks
  // while one is active, and the worker handler runs the corresponding existing
  // `refreshAll`.
  case object RefreshAllImdb       extends TaskType { val name = "RefreshAllImdb"       }
  case object RefreshAllFilmweb    extends TaskType { val name = "RefreshAllFilmweb"    }
  case object RefreshAllMetacritic extends TaskType { val name = "RefreshAllMetacritic" }
  case object RefreshAllRt         extends TaskType { val name = "RefreshAllRt"         }
  case object RefreshAllOmdb       extends TaskType { val name = "RefreshAllOmdb"       }
  // Operator-triggered identity projection, the same work the periodic SettleReaper
  // schedules, on demand from the `/tasks` page's "settle" button.
  case object SettleNow            extends TaskType { val name = "SettleNow"            }
  // Operator-triggered: one stored external source (the payload's `source`: TMDB films, a rating source) fetched again
  // for the payload's keys, and what moved kicked downstream (`services.tasks.RefreshableSource`): an identity
  // re-resolve, a row's projection — now, rather than when the periodic paths reach it.
  case object RefreshSource        extends TaskType { val name = "RefreshSource"        }

  // Chunked (map-reduce) scrape: a cinema whose listing fans out over many
  // independent units (per-day pages, per-event pages) is scraped as one
  // ScrapeChunk task PER unit (the map — each stores a keyed slice of the
  // showtimes), gathered by `ChunkScrapeCoordinator` and aggregated by one
  // ScrapeChunkReduce task (the reduce — publishes the full listing). The whole
  // run is namespaced by a unique runId so a re-scrape can't mix data with an
  // in-flight one. See ChunkedCinemaScraper / ChunkScrapeStore.
  case object ScrapeChunk       extends TaskType { val name = "ScrapeChunk"       }
  case object ScrapeChunkReduce extends TaskType { val name = "ScrapeChunkReduce" }

  // Film share cards (the worker renders them to a directory Caddy serves — see
  // `services.sharecards`). A render per (film, language, input hash); the daily prune and the
  // frequent budget pass; the end of a first-publish hold. (Facebook re-scrapes are not tasks: see `FacebookRescrapeDrain`.)
  case object RenderShareCard      extends TaskType { val name = "RenderShareCard"      }
  case object PruneShareCards      extends TaskType { val name = "PruneShareCards"      }
  case object ReleaseShareCardHold extends TaskType { val name = "ReleaseShareCardHold" }
  // A city's share card, re-rendered weekly at its own phase (`services.sharecards.CityCardService`).
  case object RenderCityCard       extends TaskType { val name = "RenderCityCard"       }

  // Runtime invariants checked by sample (see `services.tasks.RecheckedAudit`): an hourly sample
  // and, fifteen minutes later, the re-check of whatever it found — the same type, the ids in its
  // payload. Read-model content against a fresh projection; share-card pointers against the disk.
  case object AuditReadModelContent extends TaskType { val name = "AuditReadModelContent" }
  case object AuditShareCards       extends TaskType { val name = "AuditShareCards"       }
  /** An identity agreement question — a family's search, record or programme, TMDB's find of an agreed IMDb id or a
   *  record read again, a poster to hash, a catalogue page or a batch of catalogue ids — named by its id
   *  (`services.identity.Question.id`) and asked by `services.identityadapters.AgreementAsker`. */
  case object AskQuestion           extends TaskType { val name = "AskQuestion"           }
  // The four agreement task types AskQuestion replaced, each its own payload, asked as AskQuestion is
  // (`AgreementQuestions.questionsOf`). Old queued tasks only; remove after 2026-11-07.
  case object AgreementQuestion     extends TaskType { val name = "AgreementQuestion"     }
  case object AgreementFind         extends TaskType { val name = "AgreementFind"         }
  case object AgreementPoster       extends TaskType { val name = "AgreementPoster"       }
  case object AgreementCatalogue    extends TaskType { val name = "AgreementCatalogue"    }

  val all: Seq[TaskType] =
    Seq(ScrapeCinema, EnrichDetails, ReadVenuePage, ResolveImdbId, ImdbRating, FilmwebRating, RtRating, McRating,
        RefreshAllImdb, RefreshAllFilmweb, RefreshAllMetacritic, RefreshAllRt, RefreshAllOmdb, SettleNow, RefreshSource,
        ScrapeChunk, ScrapeChunkReduce,
        RenderShareCard, PruneShareCards, ReleaseShareCardHold, RenderCityCard,
        AuditReadModelContent, AuditShareCards, AskQuestion, AgreementQuestion, AgreementFind, AgreementPoster, AgreementCatalogue)

  def byName(s: String): Option[TaskType] = all.find(_.name == s)
}
