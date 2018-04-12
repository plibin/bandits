package vub.bandits.experiments.sutton

import vub.bandits.algorithms.Report.RunReport
import vub.bandits.algorithms.posteriors.{ContinuousBetaPosterior, GaussianPosterior}
import vub.bandits.algorithms._
import vub.bandits.environments.SuttonTestbed
import vub.bandits.postprocessing._
import vub.bandits.rand.RNG

/**
  * Apply bandit-solving algorithms on the Sutton testbed.
  * Reference: RL book (Sutton, 1998) section 2.2
  */
object BayesianBanditsExp extends App {
  def values: Vector[ValueFunction] =
    Vector.fill(nrArms)(new ValueFunction(stepSizeFn, 0))

  val rng = RNG.default(10)

  val nrArms = 10
  val nrBandits = 500
  val steps = 1000

  val suttonBandits = Vector.fill(nrBandits)(SuttonTestbed.testBandit(nrArms, rng))
  val bandits = suttonBandits.map(_.bandit)
  val bestArms = suttonBandits.map(_.bestArm)

  val stepSizeFn = new HarmonicStepSizeFunction

  def initializeArms(alpha: Double): Int = math.max(2, 3 - math.floor(2*alpha).toInt)

  val experiments = Map[String, Vector[RunReport]] (
    "greedy epsilon=0.1" ->
      bandits.map(EpsilonGreedy.run(_, epsilon = 0.1, values = values, steps = steps, r => false, rng)),
    "thompson=-.5" ->
      bandits.map(ThompsonSampling.run(
        _,
        Vector.fill(nrArms)(new GaussianPosterior(-.5)),
        budget = steps,
        initializeArms(-.5),
        rng)),
    "thompson=.5" ->
      bandits.map(ThompsonSampling.run(
        _,
        Vector.fill(nrArms)(new GaussianPosterior(.5)),
        budget = steps,
        initializeArms(.5),
        rng)),
    "thompson=-1" ->
      bandits.map(ThompsonSampling.run(
        _,
        Vector.fill(nrArms)(new GaussianPosterior(-1)),
        budget = steps,
        initializeArms(-1),
        rng)),
    "thompson=0" ->
      bandits.map(ThompsonSampling.run(
        _,
        Vector.fill(nrArms)(new GaussianPosterior(0)),
        budget = steps,
        initializeArms(0),
        rng)),
    "bayes-ucb=-0.5" ->
      bandits.map(BayesUCB.run(
        _,
        Vector.fill(nrArms)(new GaussianPosterior(-.5)),
        horizon = steps,
        c = 0,
        (r) => false,
        initializeArms(-.5),
        rng)),
    "bayes-ucb=.5" ->
      bandits.map(BayesUCB.run(
        _,
        Vector.fill(nrArms)(new GaussianPosterior(.5)),
        horizon = steps,
        c = 0,
        (r) => false,
        initializeArms(.5),
        rng)),
    "bayes-ucb=-1" ->
      bandits.map(BayesUCB.run(
        _,
        Vector.fill(nrArms)(new GaussianPosterior(-1)),
        horizon = steps,
        c = 0,
        (r) => false,
        initializeArms(-1),
        rng)),
    "bayes-ucb=0" ->
      bandits.map(BayesUCB.run(
        _,
        Vector.fill(nrArms)(new GaussianPosterior(0)),
        horizon = steps,
        c = 0,
        (r) => false,
        initializeArms(0),
        rng))
  )

  val bestArmFns = bestArms.map(OptimalActions.bestArmFn)

  val title = "bayesian"
  val avg = AverageReward.plot(experiments, steps)
  Plot.show(title, avg)
  val cum_avg = CumulativeAverageReward.plot(experiments, steps)
  Plot.show(title, cum_avg)
  val optimal = OptimalActions.plot(bestArmFns, experiments, steps)
  Plot.show(title, optimal)
  val cum_optimal = CumulativeOptimalActions.plot(bestArmFns, experiments, steps)
  Plot.show(title, cum_optimal)
}
