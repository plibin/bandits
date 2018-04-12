package vub.bandits.algorithms

import vub.bandits.Bandit
import vub.bandits.algorithms.Report.{RunReport, StepReport}
import vub.bandits.rand.{DiscreteUniform, RNG}

import scala.collection.mutable

object Uniform {
  def bestArm(rewardsPerArm: Vector[Rewards]): Int = {
    //TODO: performance?
    val r: Rewards = rewardsPerArm.maxBy(_.mean)
    rewardsPerArm.indexOf(r)
  }

  def run(bandit: Bandit[Double],
          budget: Int,
          rng: RNG) : (RunReport, Int) = {
    val rewardsPerArm = Vector.fill(bandit.nrArms)(new Rewards)

    val steps = mutable.Buffer[StepReport]()

    for (step <- 0 to budget - 1) {
      val arm = DiscreteUniform(0, bandit.nrArms - 1).sample(rng)

      val reward = bandit.play(arm)
      rewardsPerArm(arm).add(reward)
      steps += new StepReport(arm, reward)
    }

    (new RunReport(steps.toVector), bestArm(rewardsPerArm))
  }
}
