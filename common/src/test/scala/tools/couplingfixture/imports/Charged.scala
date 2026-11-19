package tools.couplingfixture.imports

import tools.couplingfixture.split.{Listing, Venue}

// CouplingSpec's fixture: three classes of one file, each import used by its own class. Zinc charges both imports to
// the file's LAST class, Tally, besides recording each real use; Tally's class file names only Listing.

final class Booking { def venue: String = Venue.default }

final class Ledger { def rows(n: Int): Seq[Listing] = Seq(Listing(n)) }

final class Tally { def first(listing: Listing): Int = listing.size }
