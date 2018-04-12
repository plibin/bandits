package vub.bandits.algorithms.bounds

import breeze.numerics.lgamma
import vub.bandits.{Bandit, utils}
import vub.bandits.algorithms.BayesGap.Bounds
import vub.bandits.algorithms.posteriors.GaussianPosterior

class GaussianBounds extends Bounds[GaussianPosterior] {
  def gamma(x: Double) = org.apache.commons.math3.special.Gamma.gamma(x)
  //bias correction for a normal reward distribution
  //https://en.wikipedia.org/wiki/Unbiased_estimation_of_standard_deviation
  def c4(n: Int): Double =
    math.sqrt(2.0 / (n - 1)) * (gamma(n / 2.0) * gamma((n - 1) / 2.0))

  def variance(posteriors: Vector[GaussianPosterior]) = {
    val variances =
      for {
        p <- posteriors

        v = p.sampleVariance
      } yield v
    utils.mean(variances)
  }

  def beta(bandit: Bandit[Double], budget: Int, variance: Double, hardness: Double) =
    math.sqrt((budget - 3 * bandit.nrArms) / (4 * hardness * variance))
  def U(bandit: Bandit[Double], budget: Int, posterior: GaussianPosterior, beta: Double, t: Int): Double =
    posterior.mean + (beta * posterior.sigma)
  def L(bandit: Bandit[Double], budget: Int, posterior: GaussianPosterior, beta: Double, t: Int): Double =
    posterior.mean - (beta * posterior.sigma)
}
