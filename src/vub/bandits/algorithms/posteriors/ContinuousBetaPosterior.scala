package vub.bandits.algorithms.posteriors

import vub.bandits.rand.{Bernoulli, Beta, RNG}

/**
  * TODO: ref Agrawal paper [1]
  * [1] Agrawal, Shipra, and Navin Goyal.
  *     "Analysis of Thompson Sampling for the Multi-armed Bandit Problem." COLT. 2012.
  */
class ContinuousBetaPosterior extends Posterior {
  var successes: Int = 0
  var failures: Int = 0

  def update(rng: RNG, reward: Double) = {
    val b = new Bernoulli(p = reward).sample(rng)
    if (b) successes += 1 else failures += 1
  }
  def sample(rng: RNG): Double =
    new Beta(successes + 1, failures + 1).sample(rng)

  def quantile(p: Double) = error("not implemented")
  def mean = error("not implemented")
  def variance = error("not implemented")
  def samples = error("not implemented")
  def pdf(x: Double) = error("not implemented")
}
