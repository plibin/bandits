package vub.bandits.algorithms.posteriors

import org.apache.commons.math3.distribution.TDistribution
import org.apache.commons.math3.special.Gamma
import vub.bandits.rand.{RNG, StudentsT}
import vub.bandits.algorithms.{sampleMean, sumOfSquares}

import scala.collection.mutable.ListBuffer

/**
  * Non-standardized t-distribution as presented in [1].
  * The distribution has 2 parameters: mu and sigma.
  * [1] Honda, Junya, and Akimichi Takemura.
  *     "Optimality of Thompson Sampling for Gaussian Bandits Depends on Priors." AISTATS. 2014.
  */
class GaussianPosterior(val alpha: Double) extends Posterior {
  val rewards: ListBuffer[Double] = new ListBuffer()
  var sampleMean: Double = -1
  var sos: Double = -1

  def update(rng: RNG, reward: Double) = {
    rewards += reward
    sampleMean = rewards.sum / rewards.length
    sos = sumOfSquares(rewards, sampleMean)
  }

  private def freedom_(n: Int): Int = n + (2*alpha).toInt - 1
  def freedom = freedom_(rewards.length)

  def mu = sampleMean
  private def sigma_(n: Int): Double = math.sqrt(sos / (n * freedom_(n)))
  def sigma = sigma_(rewards.length)

  def sampleVariance =
    rewards.map(x => (x - mu)*(x - mu)).sum / (rewards.length - 1)
  def sampleStdDev =
    math.sqrt(sampleVariance)

  def mean = mu

  def variance = {
    val n = rewards.length
    val f = freedom_(n)
    val t_variance = f / (f - 2)
    val s = sigma_(n)
    t_variance * (s * s)
  }

  /**
    * Implemented as presented in equation (4) of [1].
    * [1] Honda, Junya, and Akimichi Takemura.
    *     "Optimality of Thompson Sampling for Gaussian Bandits Depends on Priors." AISTATS. 2014.
    */
  def sample(rng: RNG) = {
    val n = rewards.length

    val t = new StudentsT(freedom_(n))
    val sample = t.sample(rng)

    mu + (sample * sigma_(n))
  }

  def quantile(p: Double) = {
    val n = rewards.length

    val t = new TDistribution(freedom_(n))
    val q = t.inverseCumulativeProbability(p)

    mu + (q * sigma_(n))
  }

  def samples = rewards.toVector

  def pdf(x: Double) = {
    val n = rewards.length

    val c = Gamma.gamma((n+1)/2)/(Gamma.gamma((n/2))*math.sqrt(math.Pi*n)*sigma_(n))
    val xx = (x - mu)/sigma_(n)
    c*math.pow(1 + (xx*xx)/n, -(n+1)/2)
  }
}
