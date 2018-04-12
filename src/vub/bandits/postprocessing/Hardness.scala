package vub.bandits.postprocessing


//Functions to compute best-arm hardness
object Hardness {
  def deltas(mus: Seq[Double]) = {
    val sorted_mus = mus.sorted.reverse

    val best_mu = sorted_mus(0)

    //we compute deltas for all but the best arm
    //(whose mu is in the first entry of sorted_mus)
    val deltas:Seq[Double] = sorted_mus.slice(1, sorted_mus.length).map(mu => best_mu - mu)

    //delta of the best arm delta_i*
    val best_delta = deltas.min

    best_delta +: deltas
  }

  def H1(mus: Seq[Double]) = deltas(mus).map(d => 1/(d*d)).sum
  def H2(mus: IndexedSeq[Double]) = {
    val s =
      for (d <- deltas(mus); i <- 1 to mus.length)
        yield i/(d*d)
    s.max
  }
}
