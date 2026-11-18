package tools.couplingfixture.split

// CouplingSpec's fixture: the same Schedule as tools.couplingfixture.together's, its collaborators in files of their own.

final class Schedule(showtimes: Showtimes) {
  def first: Int = showtimes.count
  def sizes(rows: Seq[Int]): Seq[Listing] = rows.map(n => new Listing(n))
  def later: Runnable = new Runnable { def run(): Unit = { Venue.default; () } }
}
