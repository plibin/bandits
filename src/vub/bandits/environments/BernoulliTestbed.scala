package vub.bandits.environments

import vub.bandits.{Action, Bandit}
import vub.bandits.rand.{Bernoulli, DiscreteUniform, RNG}

object BernoulliTestbed {
  //TODO should be a Bandit[Boolean]
  class ChapelleBandit (val bandit: Bandit[Double], val bestArm: Int)

  /**
    * Bernoulli bandit as defined in Chapelle's NIPS paper [1],
    * the number of arms can be configured (k).
    * The best arm has a reward probability of 0.5,
    * the other (k-1) arms have a reward probability of 0.5 - epsilon.
    *
    * [1] Chapelle, Olivier, and Lihong Li. "An empirical evaluation of thompson sampling."
    *     Advances in neural information processing systems. 2011.
    */
  def chapelleBernoulliBandit(bestArm: Int, k: Int, epsilon: Double, rng: RNG) = {
    def arm(p: Double) = new Action[Double](() => if (new Bernoulli(p = p).sample(rng)) 1.0 else 0.0)
    val arms = Seq.fill(bestArm)(arm(0.5 - epsilon)) ++ Seq(arm(0.5)) ++ Seq.fill(k - bestArm - 1)(arm(0.5 - epsilon))
    val b = new Bandit[Double](arms)
    new ChapelleBandit(b, bestArm)
  }
}
