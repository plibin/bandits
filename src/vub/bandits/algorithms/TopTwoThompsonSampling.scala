package vub.bandits.algorithms

import vub.bandits.algorithms.Report.{RunReport, StepReport}
import vub.bandits.algorithms.posteriors.Posterior
import vub.bandits.rand.{Bernoulli, Choice, RNG}
import vub.bandits.{Bandit, utils}

import scala.collection.mutable

object TopTwoThompsonSampling {
  def sampleArm[T](posteriors: Vector[Posterior], rng: RNG) =
    utils.maxIndex(posteriors, (p: Posterior) => p.sample(rng))
  
  def run(bandit: Bandit[Double],
          beta: Double,
          posteriors: Vector[Posterior],
          budget: Int,
          initializeArms: Int,
          rng: RNG): (RunReport, Int) = {
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
      val arm = sampleArm(posteriors, rng)
      val b = new Bernoulli(beta).sample(rng)
      if (b) {
        val reward = bandit.play(arm)
        posteriors(arm).update(rng, reward)
        steps += new StepReport(arm, reward)
      } else {
        def repeat(i: Int, max: Int): Int = {
          if (i == max) {
            Console.err.println("TTTS: exceed maximum sample tries: " + i + "!")
            Choice.sample(0 to bandit.nrArms-1, rng)
          }
          else {
            val resample = sampleArm(posteriors, rng)
            if (arm != resample)
              resample
            else
              repeat(i + 1, max)
          }
        }
        val topTwoArm = repeat(0, 1000)
        val reward = bandit.play(topTwoArm)
        posteriors(topTwoArm).update(rng, reward)
        steps += new StepReport(topTwoArm, reward)
      }
    }
    
    val bestArm = maxIndex(posteriors, (p: Posterior) => p.mean)
    (new RunReport(steps.toVector), bestArm)
  }
}
