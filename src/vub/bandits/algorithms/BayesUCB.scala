package vub.bandits.algorithms

import vub.bandits.Bandit
import vub.bandits.algorithms.Report.{RunReport, StepReport}
import vub.bandits.algorithms.posteriors.Posterior
import vub.bandits.rand.RNG

object BayesUCB {
  /**
    * Perform one iteration of the Bayes-UCB algorithm:
      TODO: explain on what it is performed
      TODO: add ref to Bayes-UCB paper
    */
  //TODO: bound iteration?
  def step(bandit: Bandit[Double],
           posteriors: Vector[Posterior],
           iteration: Int,
           horizon: Int,
           c: Double,
           censorReward: (Double) => Boolean,
           rng: RNG): StepReport = {
    val t = iteration + 1
    val p = 1 - (1/(t * math.pow(math.log(horizon), c)))
    val arm = maxIndex[Posterior](posteriors, _.quantile(p))

    val reward = bandit.play(arm)

    if (!censorReward(reward))
      posteriors(arm).update(rng, reward)

    new StepReport(arm, reward)
  }

  /**
    * Perform a number of steps (i.e. iterations) of the Bayes-UCB algorithm:
    * TODO: explain on what
    * TODO: ref Bayes-UCB paper
    */
  def run(bandit: Bandit[Double],
          posteriors: Vector[Posterior],
          horizon: Int,
          c: Double,
          censorReward: (Double) => Boolean,
          initializeArms: Int,
          rng: RNG): RunReport = {
    //initialize posteriors
    for (arm <- 0 to bandit.nrArms-1) {
      for (i <- 1 to initializeArms) {
        val reward = bandit.play(arm)
        posteriors(arm).update(rng, reward)
      }
    }

    val stepResults = for {
      i <- Vector.range(0, horizon)
      result = step(bandit, posteriors, i, horizon, c, censorReward, rng)
    } yield result

    new RunReport(stepResults)
  }
}
