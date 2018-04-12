package vub.bandits.algorithms

import vub.bandits.{Bandit, utils}
import vub.bandits.algorithms.Report.{RunReport, StepReport}
import vub.bandits.algorithms.posteriors.{GaussianPosterior, Posterior}
import vub.bandits.rand.RNG
import utils._
import vub.bandits.postprocessing.Posteriors

import scala.collection.mutable

object BayesGap {
  trait Bounds[P <: Posterior] {
    //variance over the reward distribution of the different arms
    //this assumes that this variance is approximately equal over the different arms
    def variance(posteriors: Vector[P]): Double
    def beta(bandit: Bandit[Double], budget: Int, variance: Double, hardness: Double): Double
    def U(bandit: Bandit[Double], budget: Int, posterior: P, beta: Double, t: Int): Double
    def L(bandit: Bandit[Double], budget: Int, posterior: P, beta: Double, t: Int): Double
  }

  def hardness(posteriors: Vector[Posterior], epsilon: Double): Double = {
    def armDeltaUpperBound(p: Posterior) = {
      val max = posteriors.filterNot(_ == p).map(p => p.mean + 3 * math.sqrt(p.variance)).max
      max - (p.mean - 3 * math.sqrt(p.variance))
    }
    def armHardness(p: Posterior) =
      math.max((armDeltaUpperBound(p) + epsilon) / 2, epsilon)

    sum(posteriors.map(p => 1/square((armHardness(p)))))
  }

  def run[P <: Posterior](bandit: Bandit[Double],
                          posteriors: Vector[P],
                          bounds: Bounds[P],
                          budget: Int,
                          epsilon: Double,
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

    val variance = bounds.variance(posteriors)

    var minJ = -1
    var minB = Double.MaxValue
    for (t <- 1 to budget - (bandit.nrArms * initializeArms)) {
      val H = hardness(posteriors, epsilon)
      val beta = bounds.beta(bandit, budget, variance, H)

      def U(p: P, t: Int): Double = bounds.U(bandit, budget, p, beta, t)
      def L(p: P, t: Int): Double = bounds.L(bandit, budget, p, beta, t)
      def s(p: P, t: Int): Double = U(p, t) - L(p, t)

      def B(p: P, t: Int): Double =
        posteriors.filterNot(_ == p).map(U(_, t)).max - L(p, t)
      def minSimpleRegret(t: Int): Int = minIndex(posteriors, (p: P) => B(p, t))
      def maxUpperBound(t: Int): Int = maxIndex(posteriors, (p: P) => U(p, t))

      val J_t = minSimpleRegret(t)
      val j_t = maxUpperBound(t)

      val a_t = if (s(posteriors(J_t), t) > s(posteriors(j_t), t)) J_t else j_t
      val reward = bandit.play(a_t)
      posteriors(a_t).update(rng, reward)

      val B_t = B(posteriors(J_t), t)
      if (B_t < minB) {
        minJ = J_t
        minB = B_t
      }

      steps += new StepReport(a_t, reward)
    }

    val bestArm = minJ
    (new RunReport(steps.toVector), bestArm)
  }
}
