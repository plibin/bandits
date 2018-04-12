package vub.bandits.algorithms

import spire.math.Rational
import vub.bandits.Bandit
import vub.bandits.algorithms.Report.{RunReport, StepReport}
import vub.bandits.rand.{Choice, RNG}

import scala.collection.mutable

object SuccessiveRejects extends App {
  /**
    * SuccessiveRejects algorithm as defined in:
    * Audibert, Best arm identification in multi-armed bandits, 2010
    */

  //TODO: use the Arm type everywhere?
  class Arm (val index: Int, var rewards: Rewards)

  def harmonicSum(i: Int, N: Int) =
    (i to N).map(Rational(1, _)).foldLeft(Rational(0))(_ + _)

  def log_(nrArms: Int): Rational = {
    val K = nrArms
    Rational(1, 2) + harmonicSum(2, K)
  }

  def n(nrArms: Int, budget: Int, phase: Int): Int = {
    val K = nrArms
    val n = budget
    val k = phase
    if (k == 0) {
      0
    } else {
      val frac = log_(nrArms).inverse * Rational(n - K, K + 1 - k)
      math.ceil(frac.toDouble).toInt
    }
  }

  private def worstArms(arms: Seq[Arm]): Seq[Arm] = {
    val minArm = arms.minBy(_.rewards.mean)
    val minMean: Double = minArm.rewards.mean
    arms.filter(_.rewards.mean == minMean)
  }

  //TODO: return a RunReport and the best arm as Int (for now)
  def run(bandit: Bandit[Double],
          budget: Int,
          rng: RNG): (RunReport, Int) = {
    def _n(phase: Int) = n(bandit.nrArms, budget, phase)

    val arms = (0 to bandit.nrArms - 1).map(new Arm(_, new Rewards)).toBuffer

    val steps = mutable.Buffer[StepReport]()
    for (phase <- 1 to bandit.nrArms - 1) {
      val rounds = _n(phase) - _n(phase - 1)
      for (arm <- arms) {
        for (round <- 1 to rounds) {
          val reward = bandit.play(arm.index)
          arm.rewards.add(reward)
          steps += new StepReport(arm.index, reward)
        }
      }
      //reject the worst arm
      val worst = worstArms(arms)
      arms -= Choice.sample(worst, rng)
    }

    assert(arms.size == 1)

    //play the leftover of the budget
    val left = budget - steps.length
    for (i <- 1 to left) {
      val arm = arms(0)
      val reward = bandit.play(arm.index)
      arm.rewards.add(reward)
      steps += new StepReport(arm.index, reward)
    }

    (new RunReport(steps.toVector), arms(0).index)
  }
}
