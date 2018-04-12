package vub.bandits.experiments.bernoulli

import vub.bandits.algorithms.Report.RunReport
import vub.bandits.algorithms.posteriors.{BernoulliBetaPosterior, ContinuousBetaPosterior}
import vub.bandits.algorithms._
import vub.bandits.environments.{BernoulliTestbed, SuttonTestbed}
import vub.bandits.postprocessing._
import vub.bandits.rand.{DiscreteUniform, RNG}

/**
  * Apply bandit-solving algorithms on  Chapelle Bernoulli bandits.
  */
object ThompsonSamplingExp extends App {
  def values: Vector[ValueFunction] =
    Vector.fill(nrArms)(new ValueFunction(stepSizeFn, 0))

  val rng = RNG.default(10)

  val nrArms = 10
  val nrBandits = 2000
  val steps = 1000

  def chapelleBandit(k: Int, epsilon: Double, rng: RNG) = {
    val bestArm = new DiscreteUniform(low = 0, high = k - 1).sample(rng)
    BernoulliTestbed.chapelleBernoulliBandit(bestArm, k, epsilon, rng)
  }

  val chapelleBandits = Vector.fill(nrBandits)(chapelleBandit(nrArms, 0.1, rng))
  val bandits = chapelleBandits.map(_.bandit)
  val bestArms = chapelleBandits.map(_.bestArm)

  val stepSizeFn = new HarmonicStepSizeFunction

  val experiments = Map[String, Vector[RunReport]] (
    "greedy epsilon=0.1" ->
      bandits.map(EpsilonGreedy.run(_, epsilon = 0.1, values = values, steps = steps, r => false, rng)),
    "thompson" ->
      bandits.map(ThompsonSampling.run(
        _,
        Vector.fill(nrArms)(new BernoulliBetaPosterior),
        budget = steps,
        3,
        rng))
  )

  val bestArmFns = bestArms.map(OptimalActions.bestArmFn)

  val title = "thompson"
  val avg = AverageReward.plot(experiments, steps)
  Plot.show(title, avg)
  val cum_avg = CumulativeAverageReward.plot(experiments, steps)
  Plot.show(title, cum_avg)
  val optimal = OptimalActions.plot(bestArmFns, experiments, steps)
  Plot.show(title, optimal)
  val cum_optimal = CumulativeOptimalActions.plot(bestArmFns, experiments, steps)
  Plot.show(title, cum_optimal)
}
