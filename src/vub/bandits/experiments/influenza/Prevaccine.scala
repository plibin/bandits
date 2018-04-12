package vub.bandits.experiments.influenza

import java.io.File

import vub.bandits.Bandit
import vub.bandits.algorithms.Report.RunReport
import vub.bandits.algorithms._
import vub.bandits.algorithms.bounds.GaussianBounds
import vub.bandits.algorithms.posteriors.{GaussianPosterior, Posterior}
import vub.bandits.rand.RNG

object Prevaccine extends App {
  if (getParam(args, "seed").isEmpty || getParam(args, "budget").isEmpty ||
    getParam(args, "flutescript").isEmpty || getParam(args, "workdir").isEmpty ||
    getParam(args, "algo").isEmpty || getParam(args, "censor-threshold").isEmpty) {
    print("--seed=$se --steps=$st --flutescript=$fs --workdir=$wd " +
      "--algo=[bayes-gap ttts uniform sr] " +
      "--censor-threshold=$c")
    System.exit(1)
  }

  val rng = RNG.default(getParam(args, "seed").get.toInt)
  val budget = getParam(args, "budget").get.toInt

  val workDir = new File(getParam(args, "workdir").get)

  val fluteScript = new File(getParam(args, "flutescript").get)

  val algo = getParam(args, "algo").get

  val threshold = getParam(args, "censor-threshold").get.toDouble

  val arms = VaccineArms.createArms()
  val bandit = VaccineArms.createBandit(arms, workDir, fluteScript, threshold, rng)

  def jeffreysPrior() = new GaussianPosterior(.5)

  def topTwoThompsonSampling_(b: Bandit[Double], posteriors: Vector[Posterior], budget: Int, rng: RNG) = {
    //for a Jeffreys prior, we need to initialize the arms 2x
    val initializeArms = 2
    TopTwoThompsonSampling.run(b, beta = 0.5, posteriors, budget, initializeArms, rng)
  }

  def bayesGap_(b: Bandit[Double], posteriors: Vector[GaussianPosterior], budget: Int, rng: RNG) = {
    //to have a proper posterior (from the Jeffreys prior), from which variances can be computed,
    //the arms need to be initialized 3 times
    val initializeArms = 3
    BayesGap.run(b,
      posteriors,
      new GaussianBounds,
      budget,
      math.pow(10, -10),
      initializeArms, rng)
  }

  def bayesGap(b: Bandit[Double], budget: Int, rng: RNG) = {
    val posteriors = Vector.fill(b.nrArms)(jeffreysPrior)
    val r = bayesGap_(b, posteriors, budget, rng)
    (r._1, r._2)
  }

  def topTwoThompsonSampling(b: Bandit[Double], budget: Int, rng: RNG) = {
    val posteriors = Vector.fill(b.nrArms)(jeffreysPrior)
    val r = topTwoThompsonSampling_(b, posteriors, budget, rng)
    (r._1, r._2, posteriors)
  }

  def uniform(b: Bandit[Double], budget: Int, rng: RNG) = {
    val r = Uniform.run(b, budget, rng)
    (r._1, r._2)
  }

  def successiveRejects(b: Bandit[Double], budget: Int, rng: RNG) = {
    val r = SuccessiveRejects.run(b, budget, rng)
    (r._1, r._2)
  }

  if (algo == "uniform") {
    val res = uniform(bandit, budget, rng)
    println(res._2)
  } else if (algo == "sr") {
    val res = successiveRejects(bandit, budget, rng)
    println(res._2)
  } else if (algo == "bayes-gap") {
    val res = bayesGap(bandit, budget, rng)
    println(res._2)
  } else if (algo == "ttts") {
    val res = topTwoThompsonSampling(bandit, budget, rng)
    println(res._2)
  }
}
