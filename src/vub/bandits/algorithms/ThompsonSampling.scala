package vub.bandits.algorithms

import vub.bandits.{Bandit, utils}
import vub.bandits.algorithms.Report.{RunReport, StepReport}
import vub.bandits.algorithms.posteriors.Posterior
import vub.bandits.rand.{Bernoulli, Beta, RNG, StudentsT}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

//TODO: normalized rewards!!!
object ThompsonSampling {
  def bestArm[T](posteriors: Vector[Posterior], rng: RNG) =
    utils.maxIndex(posteriors, (p: Posterior) => p.sample(rng))

  /**
    * Perform one iteration of the Thompson-Sampling algorithm:
    * - on a bandit
    * - operating on a Posterior vector
    * - at an iteration (0-based)
    * - using step sizes as produced by a stepSizeFunction
    * TODO: add ref (Agrawal, 2012 paper)
    */
  //TODO: bound iteration?
  def step(bandit: Bandit[Double],
           posteriors: Vector[Posterior],
           rng: RNG): StepReport = {
    val arm = bestArm(posteriors, rng)

    val reward = bandit.play(arm)
    posteriors(arm).update(rng, reward)

    new StepReport(arm, reward)
  }
  
  def run(bandit: Bandit[Double],
          posteriors: Vector[Posterior],
          budget: Int,
          initializeArms: Int,
          rng: RNG): RunReport = {
    val steps = mutable.Buffer[StepReport]()

    //initialize posteriors
    for (arm <- 0 to bandit.nrArms-1) {
      for (i <- 1 to initializeArms) {
          val reward = bandit.play(arm)
          posteriors(arm).update(rng, reward)
          steps += new StepReport(arm, reward)
      }
    }

    for (t <- 1 to budget - (bandit.nrArms * initializeArms)) {
      val s = step(bandit, posteriors, rng)
      steps += s
    }

    new RunReport(steps.toVector)
  }
}
