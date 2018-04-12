package vub.bandits.algorithms.posteriors

import vub.bandits.rand.RNG

trait Posterior {
  //TODO: type Posterior (for the reward type)
  //TODO: update should not have an rng as param,
  //        think how this can be fixed
  def update(rng: RNG, reward: Double)
  def sample(rng: RNG): Double
  def quantile(p: Double): Double
  def mean: Double
  def samples: Vector[Double]
  def variance: Double

  def pdf(x: Double): Double
}

