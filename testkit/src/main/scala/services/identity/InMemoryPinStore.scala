package services.identity

/** Pins held in memory, in one keyed store over memory's documents, each written as [[MongoPinStore]] stores it -- for tests. */
final class InMemoryPinStore extends KeyedPinStore(Some(new tools.InMemoryStoreDocuments()))
