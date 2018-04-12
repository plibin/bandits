package vub.bandits.rand

class StudentsT(degrees: Int) extends Rand[Double] {
  override def sample(rng: RNG): Double = {
    val g = new breeze.stats.distributions.StudentsT(degrees)(rng.rand)
    g.sample()
  }
}
